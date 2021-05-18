;;; tree-sitter-fold.el --- code folding using tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Junyi Hou
;;
;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (tree-sitter "0.15.1"))
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This package provides a code-folding mechanism based on tree-sitter
;; package. Turn on the minor-mode `tree-sitter-fold-mode' to enable
;; this mechanism. Note that all functionalities provided here based on the
;; `tree-sitter-mode', and thus it should be enabled before
;; `tree-sitter-fold-mode' can properly fold codes.

;;; Code:

(require 'tree-sitter)
(require 'seq)

;; =============
;; customization
;; =============

(defgroup tree-sitter-fold nil
  "Code folding using tree-sitter."
  :group 'tree-sitter
  :prefix "tree-sitter-fold-")

(defcustom tree-sitter-fold-foldable-node-alist
  '((python-mode . (function_definition class_definition)) ;
    (go-mode . (type_declaration function_declaration method_declaration))
    (ess-r-mode . (brace_list))
    (nix-mode . (attrset function)))
  "An alist of (mode . (list of tree-sitter-nodes considered foldable in this mode))."
  :type '(alist :key-type symbol :value-type (repeat symbol))
  :group 'tree-sitter-fold)

(defcustom tree-sitter-fold-foldable-patterns-alist
  '((python-mode . [((function_definition) @function_definition) ((class_definition) @class_definition)])
    (ess-r-mode . [((brace_list) @brace_list)])
    (nix-mode . [((attrset) @attrset) ((function) @function)])
    (scala-mode . [(((package_clause) (comment)* \. (import_declaration) @imports))])
    (go-mode . [(((package_clause) (comment)* \. (import_declaration) @imports))
		((type_declaration) @type_declaration)
		((function_declaration) @function_declaration)
		((method_declaration) @method_declaration)
		]))
  "An alist of (mode . (vector of tree-sitter query sexps that are foldable.))"
  :type '(alist :key-type symbol :value-type (vector sexp))
  :group 'tree-sitter-fold)

(defcustom tree-sitter-fold-range-alist
  '((python-mode . ((function_definition . tree-sitter-fold-range-python)
                    (class_definition . tree-sitter-fold-range-python)))
    (ess-r-mode . ((brace_list . tree-sitter-fold-range-r)))
    (nix-mode . ((attrset . tree-sitter-fold-range-nix-attrset)
                 (function . tree-sitter-fold-range-nix-function)))
    (scala-mode . ((imports . tree-sitter-fold-range-scala-imports)))
    (go-mode . ((imports . tree-sitter-fold-range-go-imports)
		(type_declaration . tree-sitter-fold-range-go-type-declaration)
                (function_declaration . tree-sitter-fold-range-go-method)
                (method_declaration . tree-sitter-fold-range-go-method))))
  "An alist of (major-mode . (foldable-node-type . function)).
FUNCTION is used to determine where the beginning and end for FOLDABLE-NODE-TYPE
in MAJOR-MODE.  It should take a single argument (the syntax node with type
FOLDABLE-NODE-TYPE) and return the buffer positions of the beginning and end of
the fold in a cons cell.  See `tree-sitter-fold-range-python' for an example."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol :value-type function))
  :group 'tree-sitter-fold)

(defcustom tree-sitter-fold-mode-hook nil
  "Hook to run when enabling `tree-sitter-fold-mode'."
  :type 'hook
  :group 'tree-sitter-fold)

;; ==========
;; minor mode
;; ==========

(define-minor-mode tree-sitter-fold-mode
  "Folding code using tree sitter."
  :init-value nil
  :lighter nil
  (if tree-sitter-fold-mode
      (progn
        (setq-local line-move-ignore-invisible t)
        (add-to-invisibility-spec '(tree-sitter-fold . t))
        (run-hooks 'tree-sitter-fold-mode-hook))
    (remove-from-invisibility-spec '(tree-sitter-fold . t))
    (let ((tree-sitter-mode t))
      (tree-sitter-fold-open-all))))

;; ================
;; evil integration
;; ================

(eval-when-compile
  (if (bound-and-true-p evil-fold-list)
      (add-to-list 'evil-fold-list
                   '((tree-sitter-fold-mode)
                     :open tree-sitter-fold-open
                     :close tree-sitter-fold-close
                     :open-rec tree-sitter-fold-open-recursively
                     :open-all tree-sitter-fold-open-all
                     :close-all tree-sitter-fold-close-all))))

;; ============================================
;; using `tree-sitter' to determine fold range.
;; ============================================

(defun tree-sitter-fold--foldable-node-at-pos (&optional pos)
  "Return the smallest foldable node at POS using queries defined in
`tree-sitter-fold-foldable-patterns-alist'. If POS is nil, use `point'. Raise
`user-error' if no foldable node is found."
  (let* ((pos (or pos (point)))
	 (patterns (alist-get major-mode tree-sitter-fold-foldable-patterns-alist))
	 (ts-query (tsc-make-query tree-sitter-language patterns))
	 (root (tsc-root-node tree-sitter-tree))
	 (start (tsc-get-descendant-for-position-range root pos pos)))
    (let ((current start) result)
      (while current
	(if-let ((captures (tsc-query-captures ts-query current #'tsc--buffer-substring-no-properties))
		 (found (> (length captures) 0)))
	    (setq result (aref captures 0)
		  current nil)
	  (setq current (tsc-get-parent current))))
      (or result (user-error "No foldable node found at POS")))))

(defun tree-sitter-fold--get-fold-range (node)
  "Return the beginning (as buffer position) of fold for NODE."
  (if-let* ((fold-alist (alist-get major-mode tree-sitter-fold-range-alist))
	    (node-type (car node))
            (fn (alist-get node-type fold-alist)))
      (if (functionp fn)
          (funcall fn (cdr node))
        (user-error
         (format "Current node is not found in `tree-sitter-fold-range-alist' in %s"
                 major-mode)))))

;; ========
;; overlays
;; ========

(defun tree-sitter-fold--create-overlay (range)
  "Create invisible overlay in RANGE."
  (when (not (null range))
    (let ((ov (make-overlay (car range) (cdr range))))
      (overlay-put ov 'invisible 'tree-sitter-fold)
      (overlay-put ov 'isearch-open-invisible #'tree-sitter-fold--isearch-open))))

(defun tree-sitter-fold--isearch-open (ov)
  "Open overlay OV during `isearch' session."
  (delete-overlay ov))

(defun tree-sitter-fold-overlay-at-pos (pos)
  "Return the tree-sitter-fold overlay at POS. Return nil if there is no overlay."
  (when-let ((beg (car pos))
	     (end (cdr pos)))
    (thread-last (overlays-in beg end)
      (seq-filter (lambda (ov)
		    (and (eq (overlay-get ov 'invisible) 'tree-sitter-fold)
			 (= (overlay-start ov) beg)
			 (= (overlay-end ov) end))))
      (car))))

;; ========
;; commands
;; ========

(defmacro tree-sitter-fold--ensure-ts (&rest body)
  "Run BODY only if `tree-sitter-mode' is enabled."
  (declare (indent 0))
  `(if (bound-and-true-p tree-sitter-mode)
       (progn ,@body)
     (user-error "Ignored, tree-sitter-mode is not enable in the current buffer")))

(defun tree-sitter-fold-close (&optional capture-node ignore-point)
  "Fold the syntax node at `point' if it is foldable. Foldable nodes are determined
using queries in `tree-sitter-fold-foldable-patterns-alist' for the current
`major-mode'. If no foldable node is found in point, do nothing. NODE has the
form (CAPTURE-NAME . NODE). CAPTURE-NAME is from the query and is used determine
the folding fn to use. If IGNORE-POINT is non-nil, fold the node even if the
point is outside the fold range. This is useful for `tree-sitter-fold-close-all'
where the point is outside of the fold range."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (when-let* ((capture-node (or capture-node (tree-sitter-fold--foldable-node-at-pos)))
		(node-start (tsc-node-start-position (cdr capture-node)))
		(range (tree-sitter-fold--get-fold-range capture-node))
		(_ (or ignore-point 
		       (and (< (min node-start (car range)) (point))
			    (> (cdr range) (point))))))
      (when-let ((ov (tree-sitter-fold-overlay-at-pos range)))
	(delete-overlay ov))
      (tree-sitter-fold--create-overlay range))))

(defun tree-sitter-fold-open ()
  "Open the fold of the syntax node in which `point' resides.
If the current node is not folded or not foldable, do nothing."
  (interactive)
  (when-let* ((node (tree-sitter-fold--foldable-node-at-pos))
	      (range (tree-sitter-fold--get-fold-range node))
	      (ov (tree-sitter-fold-overlay-at-pos range)))
    (delete-overlay ov)))

(defun tree-sitter-fold-open-recursively ()
  "Open recursively folded syntax NODE that are contained in the node at `point'."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (when-let* ((node (tree-sitter-fold--foldable-node-at-pos))
                (beg (tsc-node-start-position node))
                (end (tsc-node-end-position node)))
      (thread-last (overlays-in beg end)
        (seq-filter (lambda (ov) (eq (overlay-get ov 'invisible) 'tree-sitter-fold)))
        (mapc #'delete-overlay)))))

(defun tree-sitter-fold-close-all ()
  "Fold all foldable syntax nodes in the buffer."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (let* ((node (tsc-root-node tree-sitter-tree))
	   (patterns (alist-get major-mode tree-sitter-fold-foldable-patterns-alist))
	   (query (tsc-make-query tree-sitter-language patterns))
	   (nodes-to-fold (tsc-query-captures query node #'ignore)))
      (mapc (lambda (n)
	      (tree-sitter-fold-close n t))
	    nodes-to-fold))))

(defun tree-sitter-fold-open-all ()
  "Unfold all syntax nodes in the buffer."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (thread-last (overlays-in (point-min) (point-max))
      (seq-filter (lambda (ov) (eq (overlay-get ov 'invisible) 'tree-sitter-fold)))
      (mapc #'delete-overlay))))

(defun tree-sitter-fold-toggle ()
  "Toggle the syntax node at `point'.
If the current syntax node is not foldable, do nothing."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (let ((node (tree-sitter-fold--foldable-node-at-pos (point))))
      (if-let* ((ov (tree-sitter-fold-overlay-at node)))
          (delete-overlay ov)
        (tree-sitter-fold-close node)))))

;; =================
;; language supports
;; =================

(defun tree-sitter-fold-range-python (node)
  "Return the fold range for `function_definition' and `class_definition' NODE in Python."
  (let* ((named-node (or (tsc-get-child-by-field node :superclasses)
                         (tsc-get-child-by-field node :return_type)
                         (tsc-get-child-by-field node :parameters)
                         (tsc-get-child-by-field node :name)))
         ;; the colon is an anonymous node after return_type or parameters node
         (beg (tsc-node-end-position (tsc-get-next-sibling named-node)))
         (end (tsc-node-end-position node)))
    (cons beg end)))

(defun tree-sitter-fold-range-r (node)
  "Return the fold range for `brace_list' NODE in R."
  (let ((beg (tsc-node-end-position (tsc-get-nth-child node 0)))
        (end (1- (tsc-node-end-position node))))
    (cons beg end)))

(defun tree-sitter-fold-range-nix-attrset (node)
  "Return the fold range for `attrset' NODE in Nix express language."
  (let ((beg (tsc-node-end-position (tsc-get-nth-child node 0)))
        (end (1- (tsc-node-end-position node))))
    (cons beg end)))

(defun tree-sitter-fold-range-nix-function (node)
  "Return the fold range for `function' NODE in Nix express language."
  (let ((beg (thread-first node
               (tsc-get-child-by-field :formals)
               (tsc-get-next-sibling)
               (tsc-node-end-position)))
        (end (tsc-node-end-position node)))
    (cons beg end)))

(defun tree-sitter-fold-range-go-imports (node)
  (let ((current node)
	(node-type (tsc-node-type node))
	(beg (tsc-node-start-position (tsc-get-nth-named-child node 0)))
	(end (tsc-node-end-position node)))
    (while current
      (if-let* ((next (tsc-get-next-named-sibling current))
		(same? (eq node-type (tsc-node-type next))))
	  (setq current next)
	(setq end (tsc-node-end-position current)
	      current nil)))
    (cons beg end)))

(defmacro tree-sitter-fold--range-with-query (query)
  `(when-let* ((query ,query)
	       (tsc-query (tsc-make-query tree-sitter-language query))
	       (captures (tsc-query-captures tsc-query node #'ignore))
	       (_ (not (seq-empty-p captures)))
	       (capture (cdr (aref captures 0)))
	       (beg (tsc-node-start-position capture))
	       (end (tsc-node-end-position capture)))
     (cons beg end)))

(defun tree-sitter-fold-range-go-type-declaration (node)
  (tree-sitter-fold--range-with-query [(type_spec type: (_ [(field_declaration_list) (method_spec_list)] @declaration))]))

(defun tree-sitter-fold-range-go-method (node)
  (tree-sitter-fold--range-with-query [(method_declaration body: (_) @body)
				       (function_declaration body: (_) @body)]))

(defun tree-sitter-fold-range-scala-imports (node)
  (let ((current node)
	(node-type (tsc-node-type node))
	(beg (tsc-node-start-position (tsc-get-nth-named-child node 0)))
	(end (tsc-node-end-position node)))
    (while current
      (if-let* ((next (tsc-get-next-named-sibling current))
		(same? (eq node-type (tsc-node-type next))))
	  (setq current next)
	(setq end (tsc-node-end-position current)
	      current nil)))
    (cons beg end)))

(provide 'tree-sitter-fold)
;;; tree-sitter-fold.el ends here
