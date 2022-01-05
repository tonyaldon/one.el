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
                  (let* (
                         ;; no evaluation of any source block,
                         ;; they all are exported anyway.
                         (org-export-use-babel nil)
                         (exported (with-current-buffer "content.org"
                                     (org-export-as 'osta))))
                    (with-current-buffer (get-buffer-create "*osta*")
                      (erase-buffer)
                      (insert "<!DOCTYPE html>")
                      (insert "<html>")
                      (insert "<head>")
                      (insert "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\"/>")
                      (insert "<link rel=\"stylesheet\" type=\"text/css\" href=\"osta.css\" />")
                      (insert "</head>")
                      (insert "<body>")
                      (insert "<div class=\"container\">")
                      ;; (insert (osta-prettify-html exported))
                      (insert exported)
                      (insert "</div>")
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
  (should (string= (osta-escape "\"") "&quot;"))
  (should (string= (osta-escape "'") "&apos;"))
  (should (string= (osta-escape "regular text") "regular text"))
  (should (string= (osta-escape "<...>...&...\"...'") "&lt;...&gt;...&amp;...&quot;...&apos;")))

(ert-deftest osta-ox-test ()
  ;; osta-ox-headline
  (let (headline _info)
    (org-test-with-parsed-data "* headline 1<point>"
      (setq headline (org-element-at-point))
      (setq _info info))
    (should (string= (osta-ox-headline headline "<div>contents<div>" _info)
                     "<div><h1>headline 1</h1><div>contents<div></div>"))
    (org-test-with-parsed-data "* headline 1\n** headline 2<point>"
      (setq headline (org-element-at-point))
      (setq _info info))
    (should (string= (osta-ox-headline headline "<div>contents<div>" _info)
                     "<div><h2>headline 2</h2><div>contents<div></div>")))

  ;; section, paragraph, plain-text, bold, italic, strike-through, underline
  (should (string= (osta-ox-section nil "section" nil) "<div>section</div>"))
  (should (string= (osta-ox-paragraph nil "paragraph" nil) "<p>paragraph</p>"))
  (should (string= (osta-ox-plain-text "<...>...&" nil) "&lt;...&gt;...&amp;"))
  (should (string= (osta-ox-bold nil "bold" nil) "<b>bold</b>"))
  (should (string= (osta-ox-italic nil "italic" nil) "<i>italic</i>"))
  (should (string= (osta-ox-strike-through nil "strike-through" nil) "<del>strike-through</del>"))
  (should (string= (osta-ox-underline nil "underline" nil) "<u>underline</u>"))

  ;; code and verbatim nodes
  (let ((code (org-test-with-temp-text "before the ~inline code<point>~"
                (org-element-context)))
        (verbatim (org-test-with-temp-text "before the ~verbatim<point>~"
                    (org-element-context))))
    (should (string= (osta-ox-code code nil nil)
                     "<code class=\"osta-hl osta-hl-inline\">inline code</code>"))
    (should (string= (osta-ox-verbatim verbatim nil nil)
                     "<code class=\"osta-hl osta-hl-inline\">verbatim</code>")))

  ;; plain-list, item
  (let ((ordered-list
         (org-test-with-temp-text "<point>1) first
2) second
3) third"
           (org-element-at-point)))
        (unordered-list
         (org-test-with-temp-text "<point>- first
- second
- third"
           (org-element-at-point)))
        (other-list
         (org-test-with-temp-text "<point>- first :: description 1
- second :: description 2
- third :: description 3"
           (org-element-at-point))))
    (should (string= (osta-ox-plain-list ordered-list "contents" nil) "<ol>contents</ol>"))
    (should (string= (osta-ox-plain-list unordered-list "contents" nil) "<ul>contents</ul>"))
    (should-error (osta-ox-plain-list other-list "contents" nil)))
  (should (string= (osta-ox-item nil "item" nil) "<li>item</li>")))

;;;;; macro from org-mode repository

(comment
 ;; testing/org-test.el
 (defmacro org-test-with-temp-text (text &rest body)
   "Run body in a temporary buffer with Org mode as the active
mode holding TEXT.  If the string \"<point>\" appears in TEXT
then remove it and place the point there before running BODY,
otherwise place the point at the beginning of the inserted text."
   (declare (indent 1))
   `(let ((inside-text (if (stringp ,text) ,text (eval ,text)))
          (org-mode-hook nil))
      (with-temp-buffer
        (org-mode)
        (let ((point (string-match "<point>" inside-text)))
          (if point
              (progn
                (insert (replace-match "" nil nil inside-text))
                (goto-char (1+ (match-beginning 0))))
            (insert inside-text)
            (goto-char (point-min))))
        (font-lock-ensure (point-min) (point-max))
        ,@body)))

 ;; testing/lisp/test-ox.el
 (defmacro org-test-with-parsed-data (data &rest body)
   "Execute body with parsed data available.
DATA is a string containing the data to be parsed.  BODY is the
body to execute.  Parse tree is available under the `tree'
variable, and communication channel under `info'."
   (declare (debug (form body)) (indent 1))
   `(org-test-with-temp-text ,data
      (org-export--delete-comment-trees)
      (let* ((tree (org-element-parse-buffer))
             (info (org-combine-plists
                    (org-export--get-export-attributes)
                    (org-export-get-environment))))
        (org-export--prune-tree tree info)
        (org-export--remove-uninterpreted-data tree info)
        (let ((info (org-combine-plists
                     info (org-export--collect-tree-properties tree info))))
          ,@body)))))

;;;; transcode functions


(defun osta-escape (s)
  "Return the string S with some caracters escaped.
`<', `>' and `&' are escaped."
  (replace-regexp-in-string
   "\\(<\\)\\|\\(>\\)\\|\\(&\\)\\|\\(\"\\)\\|\\('\\)"
   (lambda (m) (pcase m
                 ("<"  "&lt;")
                 (">"  "&gt;")
                 ("&"  "&amp;")
                 ("\"" "&quot;")
                 ("'"  "&apos;")))
   s))


(defun osta-ox-headline (headline contents info)
  (let ((level (org-export-get-relative-level headline info))
        (text (org-export-data (org-element-property :title headline) info)))
    (format "<div><h%s>%s</h%s>%s</div>" level text level contents)))

(defun osta-ox-section (_section contents _info) (format "<div>%s</div>" contents))
(defun osta-ox-paragraph (_paragraph contents _info) (format "<p>%s</p>" contents))
(defun osta-ox-plain-text (text _info) (osta-escape text))
(defun osta-ox-bold (_bold contents _info) (format "<b>%s</b>" contents))
(defun osta-ox-italic (_italic contents _info) (format "<i>%s</i>" contents))
(defun osta-ox-strike-through (_strike-through contents _info) (format "<del>%s</del>" contents))
(defun osta-ox-underline (_underline contents _info) (format "<u>%s</u>" contents))
(defun osta-ox-code (code _contents _info)
  (format "<code class=\"osta-hl osta-hl-inline\">%s</code>" (osta-escape (org-element-property :value code))))
(defun osta-ox-verbatim (verbatim _contents _info)
  (format "<code class=\"osta-hl osta-hl-inline\">%s</code>" (osta-escape (org-element-property :value verbatim))))

(defun osta-ox-plain-list (plain-list contents _info)
  (let* ((type (pcase (org-element-property :type plain-list)
                 (`ordered "ol")
                 (`unordered "ul")
                 (other (error "`osta' doesn't support list type: %s" other)))))
    (format "<%s>%s</%s>" type contents type)))

(defun osta-ox-item (_item contents _info)
  (format "<li>%s</li>" contents))

;;;;; blocks

(defun osta-ox-is-results-p (element)
  "Return t if ELEMENT is considered to be a result block.
In `org-mode', the following \"allowed\" blocks are result blocks:
  - block after the line `#+RESULTS:',
  - block after the line `#+ATTR_OSTA_RESULTS:'.
Blocks that are \"allowed\" to be result blocks are of the type:
  - `src-block',
  - `fixed-width',
  - `example-block'."
  (or (org-element-property :results element)
      (org-element-property :attr_osta_results element)))

(defun osta-ox-htmlize (code lang &optional is-results-p)
  "Return CODE string htmlized using `htmlize.el' in language LANG.

If `is-results-p' is non-nil, CSS class of tag <code> in the returned
string is \"osta-hl osta-hl-results\".
If nil, the CSS class is `osta-hl osta-hl-block'.

Use `org-html-fontify-code'."
  (let* ((org-html-htmlize-font-prefix "osta-hl-")
         (org-html-htmlize-output-type 'css)
         (class (if is-results-p
                    "osta-hl osta-hl-results"
                  "osta-hl osta-hl-block")))
    (format "<pre><code class=\"%s\">%s</code></pre>"
            class
            (replace-regexp-in-string
             "<span class=\"osta-hl-default\">\\([^<]*\\)</span>"
             "\\1"
             (org-html-fontify-code code lang)))))

(defun osta-ox-src-block (src-block _contents _info)
  "Return SRC-BLOCK element htmlized using `htmlize.el'."
  (let* ((code (car (org-export-unravel-code src-block)))
         (lang (org-element-property :language src-block))
         (is-results-p (osta-ox-is-results-p src-block)))
    (osta-ox-htmlize code lang is-results-p)))


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
