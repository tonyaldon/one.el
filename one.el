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

(defun one-headline (headline)
  "Return a plist describing HEADLINE.

The properties are `:id', `:level' and `:title'.
The value of `:id' is built from CUSTOM_ID property of HEADLINE
if it exists or generated randomly."
  (let* ((custom-id (org-element-property :CUSTOM_ID headline))
         (id (or (and custom-id
                      ;; we match "baz" in "/foo/bar/#baz"
                      (string-match "\\`\\(?:[^#]+\\S-\\)#\\(.+\\)" custom-id)
                      (match-string-no-properties 1 custom-id))
                 (format "one-%x" (random #x10000000000))))
         (level (org-element-property :level headline))
         (title (org-element-property :raw-value headline)))
    (list :id id :level level :title title)))

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

    (link . one-ox-link)))

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
                         (exported (org-export-as 'one))
                         )
                    (with-current-buffer (get-buffer-create "*one*")
                      (erase-buffer)
                      (insert "<!DOCTYPE html>")
                      (insert "<html>")
                      (insert "<head>")
                      (insert "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\"/>")
                      (insert "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/one.css\" />")
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
                    ;; (switch-to-buffer "*one*")
                    )))

;;;; headline, section, paragraph, etc.

(defun one-ox-headline (headline contents info)
  ;; Note that markups and links are not exported if
  ;; used in headlines, only the raw value string.
  ;; So don't use them in headlines.
  (let* ((headline-plist (one-headline headline))
         (id (plist-get headline-plist :id))
         (level (plist-get headline-plist :level))
         (title (plist-get headline-plist :title))
         (ct (if (null contents) "" contents)))
    (format "<div><h%s id=\"%s\">%s</h%s>%s</div>" level id title level ct)))

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
  (let* ((type (org-element-property :type link))
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
                  ;; ./assets/images/image-1.png --> /images/image-1.png
                  ;; ./public/blog/page-1.md     --> /blog/page-1.md
                  (and (string-match "\\`\\./\\(assets\\|public\\)" path)
                       (replace-match "" nil nil path))
                  (let ((beg (org-element-property :begin link)))
                    (signal 'one-link-broken
                            `(,raw-link ,(format "goto-char: %s" beg))))))

                (t raw-link))))
    (format "<a href=\"%s\">%s</a>" href (or (org-string-nw-p desc) href))))

;;; pages

(define-error 'one-path "CUSTOM_ID not defined")

(defun one-list-pages ()
  "Return a list of the pages in current buffer.

Each page in the list is a plist with the following properties:

- `:one-path': ...
- `:one-render-page-with': symbol of the function to use to render
  the page.  This function takes three arguments:
  - TREE (corresponding to `:one-tree'),
  - HEADLINES (corresponding to `:one-headlines') and
  - PAGES (corresponding to the list of all pages return by `
    one-list-pages').  This argument is optional.
- `:one-tree': tree (as produced by `org-element') containing the
  content of the page.
- `:one-headlines': list in order of the headlines in the tree `:one-tree'.
  Each headline in that list is a plist with the following properties `:id',
  `:level' and `:title'."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (elt)
      (when (and (= (org-element-property :level elt) 1)
                 (string= (org-element-property :ONE_IS_PAGE elt) "t"))
        (list
         :one-path
         (or (org-element-property :CUSTOM_ID elt)
             (let ((beg (org-element-property :begin elt)))
               (signal 'one-path (list (format "goto-char: %s" beg)))))
         :one-render-page-with
         (when-let ((render-function
                     (org-element-property :ONE_RENDER_PAGE_WITH elt)))
           (intern render-function))
         :one-tree elt
         :one-headlines (org-element-map elt 'headline
                          (lambda (elt) (one-headline elt))))))))

(defvar one-css
  "@import url('https://fonts.googleapis.com/css2?family=Fira+Mono:wght@400;700&family=Signika:wght@300;400;500;600;700&display=swap');

body {
  background-image: radial-gradient(47.66% 38.4% at 52.19% 28.51%, rgb(18, 25, 40) 0%, rgb(8, 11, 18) 100%);
  color: rgba(204, 215, 229, 1);
	font-size: 140%;
  font-family: \"Signika\",sans-serif;
}

.container {
  max-width: 800px;
  margin: auto;
  padding: 0 2em;
}

@media (max-width: 768px) {
  .container {
    padding: 0 0.2em;
  }

  body {
    font-size: 110%;
  }
}

h1,
h2,
h3,
h4,
h5,
h6 {
  color: #f7dcba;
}

a {
  color: #00d1b2;
  cursor: pointer;
  text-decoration: none;
}

a:hover {
  text-decoration: underline;
}

.one-blockquote {
  background: linear-gradient(to right bottom, #313b52, #2f394f, #2d364b, #2b3448, #293245);
  border-left: 0.3em solid #d3b2a1;
  color: #d3b2a1;
  margin: auto;
  padding: 0.1em 1.5em;
  width: 50%;
}

.one-hl {
  border-radius:0.5em;
  color: #dedede;
	font-family: 'Fira Mono', monospace;
  font-size: 0.9em;
  font-weight: normal;
}

.one-hl-inline {
  background: linear-gradient(to right bottom, #222939, #1e2535, #1b2231, #171e2e, #141b2a);
	border: 1px solid #141b2a;
  padding: 0.2em 0.35em;
}

.one-hl-block {
	background: linear-gradient(to right bottom, #222939, #1e2535, #1b2231, #171e2e, #141b2a);
	border-radius: 0.5em;
  border: 1px solid #141b2a;
  display: block;
  overflow-x: auto;
  padding: 0.5em;
}

.one-hl-results {
  background: linear-gradient(to right bottom, #313b52, #2f394f, #2d364b, #2b3448, #293245);
  border-left: 3px solid #dedede;
	border-radius: 0.2em;
  display: block;
  margin: auto;
  padding: 0.5em 1em;
  overflow-x: auto;
  width: 92%;
}

.one-hl-negation-char { color: #ff6c60}              /* font-lock-negation-char-face */
.one-hl-warning { color: #fd971f}                    /* font-lock-warning-face */
.one-hl-variable-name { color: #fd971f}              /* font-lock-variable-name-face */
.one-hl-doc { color: #eedc82}                        /* font-lock-doc-face */
.one-hl-doc-string { color: #eedc82}                 /* font-lock-doc-string-face */
.one-hl-string { color: #eedc82}                     /* font-lock-string-face */
.one-hl-function-name { color: #458b74}              /* font-lock-function-name-face */
.one-hl-builtin { color: #457f8b}                    /* font-lock-builtin-face */
.one-hl-type { color: #d3b2a1}                       /* font-lock-type-face */
.one-hl-keyword { color: #f92672}                    /* font-lock-keyword-face */
.one-hl-preprocessor { color: #f92672}               /* font-lock-preprocessor-face */
.one-hl-comment-delimiter { color: #8c8c8c}          /* font-lock-comment-delimiter-face */
.one-hl-comment { color: #8c8c8c}                    /* font-lock-comment-face */
.one-hl-constant { color: #87cefa}                   /* font-lock-constant-face */
.one-hl-reference { color: #f5ebb6}                  /* font-lock-reference-face */
.one-hl-regexp-grouping-backslash { color: #87cefa}  /* font-lock-regexp-grouping-backslash */
.one-hl-regexp-grouping-construct { color: #87cefa}  /* font-lock-regexp-grouping-construct */
.one-hl-number { color: #eedc82}                     /* font-lock-number-face */


.one-hl-clojure-keyword { color: #457f8b}            /* clojure-keyword-face */
.one-hl-sh-quoted-exec { color: #62bd9c}             /* sh-quoted-exec */

::-webkit-scrollbar {
  width: 0.6em;
  height: 0.6em;
}

::-webkit-scrollbar-track {
  background: rgb(8, 11, 18);
}

::-webkit-scrollbar-thumb {
  background-image: linear-gradient(to right top, #345157, #2d4751, #283d4a, #253342, #222939);
  border-radius: 0.5em;
}

::-webkit-scrollbar-thumb:hover {
  background-image: linear-gradient(to right top, #345157, #304c54, #2d4751, #2a424e, #283d4a);
}
"
  "css file used by `one'.")

(defun one-new-project ()
  ""
  (interactive)
  (let ((one-org "* Home
:PROPERTIES:
:ONE_IS_PAGE: t
:ONE_RENDER_PAGE_WITH: one-default-home
:CUSTOM_ID: /
:END:

This text is inserted before we list the pages in our web site.

- [[#/blog/page-1/]]
- [[#/blog/page-2/]]
- [[#/blog/page-3/]]

* Page 1
:PROPERTIES:
:ONE_IS_PAGE: t
:CUSTOM_ID: /blog/page-1/
:END:

When we don't specify a function to render a page with the org
property ~ONE_RENDER_PAGE_WITH~, the function ~one-default~ is used
by default.

* Page 2
:PROPERTIES:
:ONE_IS_PAGE: t
:ONE_RENDER_PAGE_WITH: one-default
:CUSTOM_ID: /blog/page-2/
:END:

This page is rendered with the function ~one-default~ specified in the
org property ~ONE_RENDER_PAGE_WITH~.

** headline 1
*** headline 1.1

foo

*** headline 1.2

bar

** headline 2

As you can see, ~one-default~ doesn't add the table of content (TOC).
If you want a default function that adds the TOC to the page you can
use the function ~one-default-with-toc~ (see [[#/blog/page-3/]])

* Page 3
:PROPERTIES:
:ONE_IS_PAGE: t
:ONE_RENDER_PAGE_WITH: one-default-with-toc
:CUSTOM_ID: /blog/page-3/
:END:

This page is rendered with the function ~one-default-with-toc~ specified
in the org property ~ONE_RENDER_PAGE_WITH~.

** headline 1
*** headline 1.1

foo

*** headline 1.2

bar

** headline 2

As you can see, ~one-default~ doesn't add the table of content (TOC).
If you don't want the table of content (TOC) to be added, you can use
the function ~one-default~ (see [[#/blog/page-2/]])
"))
    (with-temp-file "one.org" (insert one-org))
    (make-directory "assets" t)
    (with-temp-file "./assets/one.css" (insert one-css))
    (find-file "one.org")))

(defun one-default-home (tree headlines &optional pages)
  ""
  (let ((org-export-with-sub-superscripts nil)
        (org-export-use-babel nil)
        (title (org-element-property :raw-value tree))
        (content (org-export-data-with-backend
                  (org-element-contents tree) 'one nil)))
    (with-temp-buffer
      (insert
       "<!DOCTYPE html>
<html>
<head>
<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\"/>
<link rel=\"stylesheet\" type=\"text/css\" href=\"/one.css\" />\n")
      (insert (concat "<title>" title "</title>"))
      (insert "</head>")
      (insert "<div class=\"container\">")
      (insert "<body>")
      (insert (concat "<div style=\"text-align: center;\">" (upcase title) "</div>"))
      (insert content)
      (insert "</div>")
      (insert "</body>\n</html>")
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun one-default (tree headlines &optional pages)
  ""
  (let ((org-export-with-sub-superscripts nil)
        (org-export-use-babel nil)
        (title (org-element-property :raw-value tree))
        (content (org-export-data-with-backend
                  (org-element-contents tree) 'one nil)))
    (with-temp-buffer
      (insert
       "<!DOCTYPE html>
<html>
<head>
<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\"/>
<link rel=\"stylesheet\" type=\"text/css\" href=\"/one.css\" />\n")
      (insert (concat "<title>" title "</title>"))
      (insert "</head>")
      (insert "<div class=\"container\">")
      (insert "<body>")
      (insert (concat "<div style=\"text-align: center;\">" (upcase title) "</div>"))
      (insert content)
      (insert "</div>")
      (insert "</body>\n</html>")
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun one-default-with-toc (tree headlines &optional pages)
  ""
  (let ((org-export-with-sub-superscripts nil)
        (org-export-use-babel nil)
        (title (org-element-property :raw-value tree))
        (content (org-export-data-with-backend
                  (org-element-contents tree) 'one nil)))
    (with-temp-buffer
      (insert
       "<!DOCTYPE html>
<html>
<head>
<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\"/>
<link rel=\"stylesheet\" type=\"text/css\" href=\"/one.css\" />\n")
      (insert (concat "<title>" title "</title>"))
      (insert "</head>")
      (insert "<div class=\"container\">")
      (insert "<body>")
      (insert (concat "<div style=\"text-align: center;\">" (upcase title) "</div>"))
      (insert content)
      (insert "</div>")
      (insert "</body>\n</html>")
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun one-build-only-html ()
  "Build `one' web site of the current buffer under subdirectory `./public/'.

Doesn't copy files from `./assets/' to `./public/'.
See also `one-build'."
  (interactive)
  (dolist (page (one-list-pages))
    (let* ((path (concat "./public" (plist-get page :one-path)))
           (file (concat path "index.html"))
           (render-page-with (plist-get page :one-render-page-with))
           (tree (plist-get page :one-tree))
           (headlines (plist-get page :one-headlines)))
      (make-directory path t)
      (with-temp-file file
        (insert
         (funcall (or render-page-with 'one-default)
                  tree headlines))))))

;;; one provide

(provide 'one)
;;; org-bars.el ends here
