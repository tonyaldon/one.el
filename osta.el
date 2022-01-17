;;; osta.el --- few functions to build static website -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Tony Aldon

;;; commentary:
;;
;; the following org-element nodes are not supported:
;;     center-block, clock, drawer, dynamic-block, entity, export-block,
;;     export-snippet, footnote-reference, horizontal-rule, inline-src-block,
;;     inlinetask, keyword, latex-environment, latex-fragment, line-break,
;;     node-property, planning, property-drawer, radio-target, special-block,
;;     statistics-cookie, table, table-cell, table-row,target, timestamp, verse-block
;;
;; As I don't export org files directly but via `osta' HTML template system,
;; I don't implement function for inner-template and template symbol use
;; by org export when exporting files.

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

    (subscript . osta-ox-subscript)
    (superscript . osta-ox-superscript)

    (plain-list . osta-ox-plain-list)
    (item . osta-ox-item)

    (src-block . osta-ox-src-block)
    (example-block . osta-ox-example-block)
    (fixed-width . osta-ox-fixed-width)
    (quote-block . osta-ox-quote-block)

    ;; (link . org-html-link)
    )
  )

;;;; tests osta backend

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
                         ;; _ and ^ characters are exported as is.
                         (org-export-with-sub-superscripts nil)
                         ;; no evaluation of any source block,
                         ;; they all are exported as is.
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


(defun osta-ox-subscript (_subscript contents _info) (format "<sub>%s</sub>" contents))
(defun osta-ox-superscript (_superscript contents _info) (format "<sup>%s</sup>" contents))

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
  (let* ((level (org-export-get-relative-level headline info))
         (title (org-export-data (org-element-property :title headline) info))
         (ct (if (null contents) "" contents))
         (custom-id (org-element-property :CUSTOM_ID headline))
         (id (and custom-id
                  ;; we match "baz" in "/foo/bar/#baz"
                  (string-match "\\`\\(?:[^#]+\\S-\\)#\\(.*\\)" custom-id)
                  (format " id=\"%s\"" (match-string-no-properties 1 custom-id)))))
    (format "<div><h%s%s>%s</h%s>%s</div>" level (or id "") title level ct)))

(defun osta-ox-section (_section contents _info)
  (if (null contents) "" (format "<div>%s</div>" contents)))

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

(defun osta-ox-example-block (example-block _contents _info)
  "Return EXAMPLE-BLOCK element htmlized using `htmlize.el'."
  (let* ((code (car (org-export-unravel-code example-block)))
         (lang "text")
         (is-results-p (osta-ox-is-results-p example-block)))
    (osta-ox-htmlize code lang is-results-p)))

(defun osta-ox-fixed-width (fixed-width _contents _info)
  "Return FIXED-WIDTH element htmlized using `htmlize.el'."
  (let* ((code (car (org-export-unravel-code fixed-width)))
         (lang "text")
         (is-results-p (osta-ox-is-results-p fixed-width)))
    (osta-ox-htmlize code lang is-results-p)))

(defun osta-ox-quote-block (_quote-block contents _info)
  (format "<blockquote class=\"osta-blockquote\">%s</blockquote>" contents))



;;; osta provide

(provide 'osta)
;;; org-bars.el ends here
