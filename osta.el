;;; osta.el --- few functions to build static website -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Tony Aldon

;;; osta org backend


(require 'ox)

(org-export-define-backend 'osta
  '((headline . osta-ox-headline)
    (section . osta-ox-section)
    (paragraph . osta-ox-paragraph)

    (plain-text . osta-ox-plain-text)

    (bold . osta-ox-bold)
    (italic . osta-ox-italic)
    (strike-through . osta-ox-strike-through)
    (underline . osta-ox-underline)
    (code . osta-ox-code)
    (verbatim . osta-ox-verbatim)

    (plain-list . osta-ox-plain-list)
    (item . osta-ox-item)

    (src-block . osta-ox-src-block)
    ;; (example-block . org-html-example-block)
    ;; (fixed-width . org-html-fixed-width)
    ;; (quote-block . org-html-quote-block)

    ;; (link . org-html-link)
    )
  )

;;;; tests osta backend

(require 'ert)

;; (org-export-as BACKEND &optional SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)
;; (org-export-as 'osta nil nil t)

(defun osta-prettify-html (html)
  "Return the HTML string prettified.
Use for debugging/exploring purpose."
  (with-temp-buffer
    (insert (replace-regexp-in-string ">\\s-*<" ">\n<" html))
    (html-mode)
    (indent-region (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

(global-set-key (kbd "C-<f2>")
                (lambda () (interactive)
                  (let ((exported (with-current-buffer "content.org"
                                    (org-export-as 'osta))))
                    (with-current-buffer (get-buffer-create "*osta*")
                      (erase-buffer)
                      (insert "<!DOCTYPE html>")
                      (insert "<html>")
                      (insert "<head>")
                      (insert "<link rel=\"stylesheet\" type=\"text/css\" href=\"emacs.css\" />")
                      (insert "</head>")
                      (insert "<body>")
                      ;; (insert (osta-prettify-html exported))
                      (insert exported)
                      (insert "</body>")
                      (insert "</html>")
                      (mhtml-mode)
                      (write-region (point-min) (point-max) "index.html"))
                    (switch-to-buffer "*osta*"))))

;; (global-set-key (kbd "C-<f1>") (lambda () (interactive)(ert "osta-escape-test")))
;; (global-set-key (kbd "C-<f1>") (lambda () (interactive)(ert "osta-ox-test")))

(ert-deftest osta-escape-test ()
  (should (string= (osta-escape "<") "&lt;"))
  (should (string= (osta-escape ">") "&gt;"))
  (should (string= (osta-escape "&") "&amp;"))
  (should (string= (osta-escape "regular text") "regular text"))
  (should (string= (osta-escape "<...>...&") "&lt;...&gt;...&amp;")))

(ert-deftest osta-ox-test ()
  (should (string= (osta-ox-bold nil "bold" nil) "<b>bold</b>"))
  (should (string= (osta-ox-italic nil "italic" nil) "<i>italic</i>"))
  (should (string= (osta-ox-strike-through nil "strike-through" nil) "<del>strike-through</del>"))
  (should (string= (osta-ox-underline nil "underline" nil) "<u>underline</u>"))
  ;; (osta-ox-code nil nil nil) ;;
  ;; (osta-ox-verbatim nil nil nil) ;;
  )

;;;; transcode functions


(defun osta-escape (s)
  "Return the string S with some caracters escaped.
`<', `>' and `&' are escaped."
  (replace-regexp-in-string
   "\\(<\\)\\|\\(>\\)\\|\\(&\\)"
   (lambda (m) (pcase m ("<" "&lt;") (">" "&gt;") ("&" "&amp;")))
   s))


(defun osta-ox-headline (headline contents info)
  (let ((level (org-export-get-relative-level headline info))
        (text (org-export-data (org-element-property :title headline) info)))
    (format "<div><h%s>%s</h%s>%s</div>" level text level contents)))

(defun osta-ox-section (section contents info) (format "<div>%s</div>" contents))
(defun osta-ox-paragraph (paragraph contents info) (format "<p>%s</p>" contents))
(defun osta-ox-plain-text (text info) (osta-escape text))
(defun osta-ox-bold (_bold contents _info) (format "<b>%s</b>" contents))
(defun osta-ox-italic (_italic contents _info) (format "<i>%s</i>" contents))
(defun osta-ox-strike-through (_strike-through contents _info) (format "<del>%s</del>" contents))
(defun osta-ox-underline (_underline contents _info) (format "<u>%s</u>" contents))
(defun osta-ox-code (code _contents _info)
  (format "<code>%s</code>" (osta-escape (org-element-property :value code))))
(defun osta-ox-verbatim (verbatim _contents _info)
  (format "<code>%s</code>" (osta-escape (org-element-property :value verbatim))))

(defun osta-ox-plain-list (plain-list contents _info)
  (let* ((type (pcase (org-element-property :type plain-list)
                 (`ordered "ol")
                 (`unordered "ul")
                 (other (error "`osta' doesn't support list type: %s" other)))))
    (format "<%s>%s</%s>" type contents type)))

(defun osta-ox-item (item contents info)
  (format "<li>%s</li>" contents))

(setq osta-pygmentize-cmd "/home/tony/work/projects/osta/.venv/bin/pygmentize")

(defun osta-pygmentize (code lang)
  "Return an html string with CODE formatted with `pygmentize'."
  (let* ((id (sha1 (mapconcat #'number-to-string (current-time) "")))
         (tmpfile (concat "/tmp/pygmentize-" id))
         (cmd (concat osta-pygmentize-cmd
                      " -f html"
                      " -l " lang
                      " " tmpfile)))
    (with-temp-file tmpfile (insert code))
    (shell-command-to-string cmd)))

(defun osta-ox-src-block (src-block _contents info)
  "Return html string of the code `highlighted' using `pygmentize' external process."
  (let* ((lang (org-element-property :language src-block))
         (code (car (org-export-unravel-code src-block))))
    (osta-pygmentize code lang)))

;;; org-element nodes not supported
;;;; template

;; (inner-template . org-html-inner-template)
;; (template . org-html-template)

;;;; because I almost never used them in org file

;; (center-block . org-html-center-block)
;; (clock . org-html-clock)
;; (drawer . org-html-drawer)
;; (dynamic-block . org-html-dynamic-block)
;; (entity . org-html-entity)
;; (export-block . org-html-export-block)
;; (export-snippet . org-html-export-snippet)
;; (footnote-reference . org-html-footnote-reference)
;; (horizontal-rule . org-html-horizontal-rule)
;; (inline-src-block . org-html-inline-src-block)
;; (inlinetask . org-html-inlinetask)
;; (keyword . org-html-keyword)
;; (latex-environment . org-html-latex-environment)
;; (latex-fragment . org-html-latex-fragment)
;; (line-break . org-html-line-break)
;; (node-property . org-html-node-property)
;; (planning . org-html-planning)
;; (property-drawer . org-html-property-drawer)
;; (radio-target . org-html-radio-target)
;; (special-block . org-html-special-block)
;; (statistics-cookie . org-html-statistics-cookie)
;; (subscript . org-html-subscript)
;; (superscript . org-html-superscript)
;; (target . org-html-target)
;; (timestamp . org-html-timestamp)
;; (verse-block . org-html-verse-block)

;;;; because I almost never use org tables

;; (table . org-html-table)
;; (table-cell . org-html-table-cell)
;; (table-row . org-html-table-row)

;; (:html-table-align-individual-fields nil nil org-html-table-align-individual-fields)
;; (:html-table-caption-above nil nil org-html-table-caption-above)
;; (:html-table-data-tags nil nil org-html-table-data-tags)
;; (:html-table-header-tags nil nil org-html-table-header-tags)
;; (:html-table-use-header-tags-for-first-column nil nil org-html-table-use-header-tags-for-first-column)


;;; osta provide

(provide 'osta)
;;; org-bars.el ends here
