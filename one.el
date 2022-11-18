;;; one.el --- few functions to build static websites -*- lexical-binding: t; -*-
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
;; As I don't export org files directly but via `one' HTML template system,
;; I don't implement function for inner-template and template symbol use
;; by org export when exporting files.

;;; utils

(defun one-escape (s)
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

;;; one-ox

(require 'ox)

;;;; one backend

(org-export-define-backend 'one
  '((headline . one-ox-headline)
    (section . one-ox-section)
    (paragraph . one-ox-paragraph)

    (plain-text . one-ox-plain-text)

    (bold . one-ox-bold)
    (italic . one-ox-italic)
    (strike-through . one-ox-strike-through)
    (underline . one-ox-underline)
    (code . one-ox-code)
    (verbatim . one-ox-verbatim)

    (subscript . one-ox-subscript)
    (superscript . one-ox-superscript)

    (plain-list . one-ox-plain-list)
    (item . one-ox-item)

    (src-block . one-ox-src-block)
    (example-block . one-ox-example-block)
    (fixed-width . one-ox-fixed-width)
    (quote-block . one-ox-quote-block)

    (link . one-ox-link))
  :options-alist
  '((:one-root "ONE_ROOT" nil "public")
    (:one-assets "ONE_ASSETS" nil "assets")))

;;;; export/rendering

(defun one-prettify-html (html)
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
                         ;; (exported (with-current-buffer "content.org"
                         ;;             (org-export-as 'one nil nil nil `(:one-links ,(one-map-links)))))
                         (exported (with-current-buffer "content.org" (org-export-as 'one)))
                         )
                    (with-current-buffer (get-buffer-create "*one*")
                      (erase-buffer)
                      (insert "<!DOCTYPE html>")
                      (insert "<html>")
                      (insert "<head>")
                      (insert "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\"/>")
                      (insert "<link rel=\"stylesheet\" type=\"text/css\" href=\"one.css\" />")
                      (insert "</head>")
                      (insert "<body>")
                      (insert "<div class=\"container\">")
                      ;; (insert (one-prettify-html exported))
                      (insert exported)
                      (insert "</div>")
                      (insert "</body>")
                      (insert "</html>")
                      (mhtml-mode)
                      (write-region (point-min) (point-max) "index.html"))
                    (switch-to-buffer "*one*"))))

;;;; headline, section, paragraph, etc.

(defun one-ox-headline (headline contents info)
  (let* ((level (org-export-get-relative-level headline info))
         (title (org-export-data (org-element-property :title headline) info))
         (ct (if (null contents) "" contents))
         (custom-id (org-element-property :CUSTOM_ID headline))
         (id (and custom-id
                  ;; we match "baz" in "/foo/bar/#baz"
                  (string-match "\\`\\(?:[^#]+\\S-\\)#\\(.*\\)" custom-id)
                  (format " id=\"%s\"" (match-string-no-properties 1 custom-id)))))
    (format "<div><h%s%s>%s</h%s>%s</div>" level (or id "") title level ct)))

(defun one-ox-section (_section contents _info)
  (if (null contents) "" (format "<div>%s</div>" contents)))

(defun one-ox-paragraph (_paragraph contents _info)
  (format "<p>%s</p>" contents))

(defun one-ox-plain-text (text _info)
  (one-escape text))

(defun one-ox-bold (_bold contents _info)
  (format "<b>%s</b>" contents))

(defun one-ox-italic (_italic contents _info)
  (format "<i>%s</i>" contents))

(defun one-ox-strike-through (_strike-through contents _info)
  (format "<del>%s</del>" contents))

(defun one-ox-underline (_underline contents _info)
  (format "<u>%s</u>" contents))

(defun one-ox-code (code _contents _info)
  (format "<code class=\"one-hl one-hl-inline\">%s</code>"
          (one-escape (org-element-property :value code))))

(defun one-ox-verbatim (verbatim _contents _info)
  (format "<code class=\"one-hl one-hl-inline\">%s</code>"
          (one-escape (org-element-property :value verbatim))))

(defun one-ox-plain-list (plain-list contents _info)
  (let* ((type (pcase (org-element-property :type plain-list)
                 (`ordered "ol")
                 (`unordered "ul")
                 (other (error "`one' doesn't support list type: %s" other)))))
    (format "<%s>%s</%s>" type contents type)))

(defun one-ox-item (_item contents _info)
  (format "<li>%s</li>" contents))

(defun one-ox-subscript (_subscript contents _info)
  (format "<sub>%s</sub>" contents))

(defun one-ox-superscript (_superscript contents _info)
  (format "<sup>%s</sup>" contents))

;;;; blocks

(defun one-ox-is-results-p (element)
  "Return t if ELEMENT is considered to be a result block.
In `org-mode', the following \"allowed\" blocks are result blocks:
  - block after the line `#+RESULTS:',
  - block after the line `#+ATTR_ONE_RESULTS:'.
Blocks that are \"allowed\" to be result blocks are of the type:
  - `src-block',
  - `fixed-width',
  - `example-block'."
  (or (org-element-property :results element)
      (org-element-property :attr_one_results element)))

(defun one-ox-htmlize (code lang &optional is-results-p)
  "Return CODE string htmlized using `htmlize.el' in language LANG.

If `is-results-p' is non-nil, CSS class of tag <code> in the returned
string is \"one-hl one-hl-results\".
If nil, the CSS class is `one-hl one-hl-block'.

Use `org-html-fontify-code'."
  (let* ((org-html-htmlize-font-prefix "one-hl-")
         (org-html-htmlize-output-type 'css)
         (class (if is-results-p
                    "one-hl one-hl-results"
                  "one-hl one-hl-block")))
    (format "<pre><code class=\"%s\">%s</code></pre>"
            class
            (replace-regexp-in-string
             "<span class=\"one-hl-default\">\\([^<]*\\)</span>"
             "\\1"
             (org-html-fontify-code code lang)))))

(defun one-ox-src-block (src-block _contents _info)
  "Return SRC-BLOCK element htmlized using `htmlize.el'."
  (let* ((code (car (org-export-unravel-code src-block)))
         (lang (org-element-property :language src-block))
         (is-results-p (one-ox-is-results-p src-block)))
    (one-ox-htmlize code lang is-results-p)))

(defun one-ox-example-block (example-block _contents _info)
  "Return EXAMPLE-BLOCK element htmlized using `htmlize.el'."
  (let* ((code (car (org-export-unravel-code example-block)))
         (lang "text")
         (is-results-p (one-ox-is-results-p example-block)))
    (one-ox-htmlize code lang is-results-p)))

(defun one-ox-fixed-width (fixed-width _contents _info)
  "Return FIXED-WIDTH element htmlized using `htmlize.el'."
  (let* ((code (car (org-export-unravel-code fixed-width)))
         (lang "text")
         (is-results-p (one-ox-is-results-p fixed-width)))
    (one-ox-htmlize code lang is-results-p)))

(defun one-ox-quote-block (_quote-block contents _info)
  (format "<blockquote class=\"one-blockquote\">%s</blockquote>" contents))

;;;; links

(define-error 'one-link-broken "Unable to resolve link")

(define-error 'one-options "Option not defined")

(defun one-ox-link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information."
  (let* ((one-root (plist-get info :one-root))
         (one-assets (plist-get info :one-assets))
         ;; (root-assets-re (concat "\\`\\./" "\\(" one-root "\\|" one-assets "\\)"))
         (type (org-element-property :type link))
         (path (org-element-property :path link))
         (raw-link (org-element-property :raw-link link))
         (href (cond
                ((string= type "custom-id") path)
                ((string= type "fuzzy")
                 (let ((beg (org-element-property :begin link)))
                   (signal 'one-link-broken
                           `(,raw-link
                             "fuzzy links not supported"
                             ,(format "goto-char: %s" beg)))))
                ((string= type "file")
                 (or
                  ;; for instance, when `:one-root' is equal to "public",
                  ;; ./public/blog/page-1.md --> /blog/page-1.md
                  (and (or one-root (signal 'one-options '(":one-root")))
                       (string-match (concat "\\`\\./" one-root) path)
                       (replace-match "" nil nil path))
                  ;; for instance, when `:one-assets' is equal to "assets",
                  ;; ./assets/images/image-1.png --> /images/image-1.png
                  (and (or one-assets (signal 'one-options '(":one-assets")))
                       (string-match (concat "\\`\\./" one-assets) path)
                       (replace-match "" nil nil path))
                  ;; any other file link raises an error
                  (let ((beg (org-element-property :begin link)))
                    (signal 'one-link-broken
                            `(,raw-link ,(format "goto-char: %s" beg))))))

                (t raw-link))))
    (format "<a href=\"%s\">%s</a>" href (or (org-string-nw-p desc) href))))

;;; pages

(define-error 'one-path "CUSTOM_ID not defined")

(defun one-list-pages ()
  "Return a list of the pages in current buffer."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (elt)
      (when (and (= (org-element-property :level elt) 1)
                 (string= (org-element-property :ONE_IS_PAGE elt) "t"))
        (list
         :one-path
         (or (org-element-property :CUSTOM_ID elt)
             (let ((beg (org-element-property :begin elt)))
               (signal 'one-path
                       (list (format "goto-char: %s" beg)))))
         :one-render-page-with
         (when-let ((render-function
                     (org-element-property :ONE_RENDER_PAGE_WITH elt)))
           (intern render-function))
         :one-data elt))))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "one-list-pages-test")))

(ert-deftest one-list-pages-test ()
  (should
   (equal
    (org-test-with-temp-text "* page 1
:PROPERTIES:
:ONE_IS_PAGE: t
:CUSTOM_ID: /path/to/page-1/
:END:
* NOT A PAGE
* page 2
:PROPERTIES:
:ONE_IS_PAGE: t
:ONE_RENDER_PAGE_WITH: render-function
:CUSTOM_ID: /path/to/page-2/
:END:
* Headline level 1
** NOT A PAGE (because at level headline level 2)
:PROPERTIES:
:ONE_IS_PAGE: t
:END:"
      (let* ((pages (one-list-pages))
             (page-1 (car pages))
             (page-2 (cadr pages)))
        (list (length pages)
              (plist-get page-1 :one-path)
              (plist-get page-1 :one-render-page-with)
              (car (plist-get page-1 :one-data))
              (plist-get page-2 :one-path)
              (plist-get page-2 :one-render-page-with)
              (car (plist-get page-2 :one-data)))))
    '(2
      "/path/to/page-1/" nil headline
      "/path/to/page-2/" render-function headline)))

  ;; a page must have the property CUSTOM_ID defined
  (should-error
   (org-test-with-temp-text "* CUSTOM_ID PROPERTY IS NOT DEFINED
:PROPERTIES:
:ONE_IS_PAGE: t
:END:"
     (one-list-pages))))

;;; one provide

(provide 'one)
;;; org-bars.el ends here
