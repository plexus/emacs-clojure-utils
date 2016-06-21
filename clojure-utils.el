;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hiccup

(defun clju--sexp-to-hiccup-attrs (attrs)
  (if attrs
      (concat " {"
              (s-join " "
                      (loop for attr in attrs
                            collect (concat
                                     ":"
                                     (symbol-name (car attr))
                                     " "
                                     (format "%S" (cdr attr)))))
              "}")))


(defun clju--sexp-to-hiccup-children (cs)
  (if cs
      (loop for ch in cs
            concat (concat " " (if (stringp ch)
                                   (if (string-match-p "^\s*$" ch)
                                       ""
                                     (format "%S" ch))
                                 (clju--sexp-to-hiccup ch))))))


(defun clju--sexp-to-hiccup (s)
  (concat "[:"
          (symbol-name (car s))
          (clju--sexp-to-hiccup-attrs (cadr s))
          (clju--sexp-to-hiccup-children (cddr s))
          "]"))

(defun clju--region-to-hiccup ()
  (let* ((html (libxml-parse-html-region (point) (mark)))
         (inner (caddr (caddr html))))
    (clju--sexp-to-hiccup inner)))

;; TODO if you have multiple sibling nodes without a parent node, this only
;; converts the first node
(defun clju--convert-html-to-hiccup ()
  "Turn a HTML snippet in the region into Hiccup syntax"
  (interactive)
  (let ((hiccup (clju--region-to-hiccup)))
    (delete-region (point) (mark))
    (insert hiccup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDN

(defun clju--list->hash (d)
  "recursively turn lists of cons pairs into hash-tables"
  (cond
   ((consp d) (let ((hsh (make-hash-table)))
                (dolist (c d hsh)
                  (puthash (car c) (list->hash (cdr c)) hsh))))
   ((vectorp d) (apply 'vector (mapcar 'list->map d)))
   (t d)))

(defun clju-convert-json-to-edn (pnt)
  "Replace the json at point with EDN"
  (interactive "d")
  (let ((json (json-read)))
    (delete-region pnt (point))
    (insert (edn-print-string (clju--list->hash json)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing tools

(defun clju-toggle-ignore-form ()
  "clojure - ignore (comment) form"
  (interactive)
  (if (search-backward "#_" 2 t 1)
      (delete-char 2)
    (progn
      (let ((fc (following-char)))
        (cond ((-contains? '( ?\) ?\] ?\} ) fc) (paredit-backward-up))
              ((-contains? '( ?\( ?\[ ?\: ?\{ ) fc) nil)
              (t (beginning-of-thing 'sexp)))
        (insert "#_")))))

(provide 'clojure-utils)
