;;; one.el --- Few functions to build static websites -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2022 Tony Aldon

;; Author: Tony Aldon <tony.aldon.adm@gmail.com>
;; Version: 0.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: html, org, static-site, blog
;; Homepage: https://github.com/tonyaldon/one

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
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

;;; Code

(require 'jack)
(require 'ox)
(require 'htmlize)

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

    (subscript . one-ox-no-subscript)
    (superscript . one-ox-no-superscript)

    (plain-list . one-ox-plain-list)
    (item . one-ox-item)

    (src-block . one-ox-src-block)
    (example-block . one-ox-example-block)
    (fixed-width . one-ox-fixed-width)
    (quote-block . one-ox-quote-block)

    (link . one-ox-link)))

;;;; headline, section, paragraph, etc.

(defun one-ox-headline (headline contents info)
  ;; Note that markups and links are not exported if
  ;; used in headlines, only the raw value string.
  ;; So don't use them in headlines.
  (let* ((level (org-element-property :level headline))
         (title (org-element-property :raw-value headline))
         ;; the property `:one-internal-id' is set by
         ;; `one-parse-buffer' This allow to produce unified
         ;; ids that can be use to build a TOC for each page.
         (id (org-element-property :one-internal-id headline))
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

(defun one-ox-no-subscript (_subscript contents _info)
  (concat "_" contents))

(defun one-ox-no-superscript (_superscript contents _info)
  (concat "^" contents))

;;;; blocks

(defun one-ox-fontify-code (code lang)
  "Color CODE with htmlize library.
CODE is a string representing the source code to colorize.  LANG
is the language used for CODE, as a string, or nil."
  (when code
    (let* ((lang (or (assoc-default lang org-src-lang-modes) lang))
           (lang-mode (and lang (intern (format "%s-mode" lang)))))
      (if (functionp lang-mode)
          (let* ((code
                  (let ((inhibit-read-only t))
                    (with-temp-buffer
                      (funcall lang-mode)
                      (insert code)
                      (org-font-lock-ensure)
                      (org-src-mode)
                      (set-buffer-modified-p nil)
                      (let* ((htmlize-output-type 'css)
                             (htmlize-css-name-prefix "one-hl-")
                             (htmlbuf (htmlize-buffer)))
                        (unwind-protect
	                          (with-current-buffer htmlbuf
	                            (buffer-substring
                               (plist-get htmlize-buffer-places 'content-start)
			                         (plist-get htmlize-buffer-places 'content-end)))
                          (kill-buffer htmlbuf)))))))
            ;; Strip any enclosing <pre></pre> tags.
            (if-let ((beg (and (string-match "\\`<pre[^>]*>\n?" code) (match-end 0)))
                     (end (string-match "</pre>\\'" code)))
                (substring code beg end)
              code))
        (one-escape code)))))

(defun one-ox-htmlize (code lang &optional is-results-p)
  "Return CODE string htmlized using `htmlize.el' in language LANG.

If `is-results-p' is non-nil, CSS class of tag <code> in the returned
string is \"one-hl one-hl-results\".
If nil, the CSS class is `one-hl one-hl-block'."
  (let* ((class (if is-results-p
                    "one-hl one-hl-results"
                  "one-hl one-hl-block")))
    (format "<pre><code class=\"%s\">%s</code></pre>"
            class
            (replace-regexp-in-string
             "<span class=\"one-hl-default\">\\([^<]*\\)</span>"
             "\\1"
             (if (null lang)
                 (one-escape (or code "")) ; example blocks
               (one-ox-fontify-code code lang))))))

(defun one-ox-src-block (src-block _contents _info)
  "Return SRC-BLOCK element htmlized using `htmlize.el'."
  (let* ((code (car (org-export-unravel-code src-block)))
         (lang (org-element-property :language src-block))
         (is-results-p (org-element-property :results src-block)))
    (one-ox-htmlize code lang is-results-p)))

(defun one-ox-example-block (example-block _contents _info)
  "Return EXAMPLE-BLOCK element htmlized using `htmlize.el'."
  (let* ((code (car (org-export-unravel-code example-block)))
         (lang "text")
         (is-results-p (org-element-property :results example-block)))
    (one-ox-htmlize code lang is-results-p)))

(defun one-ox-fixed-width (fixed-width _contents _info)
  "Return FIXED-WIDTH element htmlized using `htmlize.el'."
  (let* ((code (car (org-export-unravel-code fixed-width)))
         (lang "text")
         (is-results-p (org-element-property :results fixed-width)))
    (one-ox-htmlize code lang is-results-p)))

(defun one-ox-quote-block (_quote-block contents _info)
  (format "<blockquote class=\"one-blockquote\">%s</blockquote>" contents))

;;;; links

(define-error 'one-link-broken "Unable to resolve link")

(defun one-ox-link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link))
         (raw-link (org-element-property :raw-link link))
         (custom-type-link
          (let ((export-func (org-link-get-parameter type :export)))
            (and (functionp export-func)
                 (funcall export-func path desc 'one info))))
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
    (or custom-type-link
        (format "<a href=\"%s\">%s</a>"
                href (or (org-string-nw-p desc) href)))))

;;; Commands to build `one' web sites

(define-error 'one-path "CUSTOM_ID not defined")

(defun one-internal-id (headline)
  "Return a string id for HEADLINE to be used as :one-internal-id property.

The id is built from CUSTOM_ID property of HEADLINE if set
or generated a randomly."
  (let* ((custom-id (org-element-property :CUSTOM_ID headline)))
    (or (and custom-id
             ;; we match "baz" in "/foo/bar/#baz"
             (string-match "\\`\\(?:[^#]+\\S-\\)#\\(.+\\)" custom-id)
             (match-string-no-properties 1 custom-id))
        (format "one-%x" (random #x10000000000)))))

(defun one-parse-buffer ()
  "Parse current org buffer and return structure.

The only difference with `org-element-parse-buffer' is that
we add the property `:one-internal-id' to each headline."
  (let ((tree (org-element-parse-buffer)))
    ;; destructively add :one-internal-id property to headlines in `tree'
    (org-element-map tree 'headline
      (lambda (elt)
        (org-element-put-property
         elt :one-internal-id (one-internal-id elt))))
    tree))

(defun one-is-page (headline)
  "Return nil if HEADLINE element is not a `one' page.

If HEADLINE is a page, return a plist with the properties
`:one-title', `:one-path', `:one-render-page-function' and
`:one-page-tree' defined like this:

- `:one-title': the raw value of the first headline of HEADLINE,
- `:one-path': the path of the page as a string,
- `:one-render-page-function': the function to render the page as
  a symbol.  This function is declared in the org buffer for
  each page using the org property ONE.

  This function takes 3 arguments:

  - `page-tree:' which correspond to the data in `:one-page-tree',
  - `pages:' list of pages,
  - `global:' a plist of global informations that are computed once
    when `one' website is built (before rendering the pages), see
    `one-build-only-html' and `one-build'.  This argument can be
    modified by the user at build time.  That means that if your
    render function needs extra information you can tell `one' to
    compute those informations and to add them to `global'.

  You can see how to implement render functions looking at the
  default render functions `one-default-home', `one-default' and
  `one-default-with-toc'.

- `:one-page-tree': the argument HEADLINE passed to `one-is-page'.

See `one-list-pages'."
  (when (= (org-element-property :level headline) 1)
    (when-let ((path (org-element-property :CUSTOM_ID headline))
               (render-page-function (org-element-property :ONE headline)))
      `(:one-title ,(org-element-property :raw-value headline)
        :one-path ,path
        :one-render-page-function ,(intern render-page-function)
        :one-page-tree ,headline))))

(defun one-list-pages (tree)
  "Return the list of the pages in TREE.

TREE is a parsed org buffer as returned by `one-parse-buffer'.
The function `one-is-page' determines which headlines in TREE
are pages."
  (org-element-map tree 'headline
    (lambda (headline) (one-is-page headline))))

(defvar one-add-to-global
  '((:one-global-property :one-tree
     :one-global-function (lambda (pages tree) tree)))
  "List used to set the `global' argument passed to render functions.

Elements in that list are plist with the following properties:

- `:one-global-property': a keyword that is used as proprety
  in the `global' argument passed to the render functions.
- `:one-global-function': a function that takes two arguments `pages'
  (list of pages, see `one-list-pages') and `tree'
  (see `one-parse-buffer').  That function is called once in
  `one-build-only-html' and its result is used as the value of
  the property `:one-global-property' in the `global' argument
  passed to the render functions.")

(defvar one-hook nil
  "List of functions called once in `one-build-only-html'.

Those functions take three arguments:

- `pages': list of pages, see `one-list-pages',
- `tree': see `one-parse-buffer',
- `global': see `one-add-to-global'.

As those functions take `global' argument they are called after
that argument has been let binded using `one-add-to-global'.")

(defun one-build-only-html ()
  "Build `one' web site of the current buffer under subdirectory `./public/'.

Doesn't copy files from `./assets/' to `./public/'.
See also `one-build'."
  (interactive)
  (let* ((tree (one-parse-buffer))
         (pages (one-list-pages tree))
         (global
          (let (global)
            (dolist (glob one-add-to-global)
              (push (funcall (plist-get glob :one-global-function) pages tree)
                    global)
              (push (plist-get glob :one-global-property) global))
            global)))
    (dolist (hook one-hook) (funcall hook pages tree global))
    (dolist (page pages)
      (let* ((path (concat "./public" (plist-get page :one-path)))
             (file (concat path "index.html"))
             (render-page-function (plist-get page :one-render-page-function))
             (page-tree (plist-get page :one-page-tree)))
        (make-directory path t)
        (with-temp-file file
          (insert (funcall render-page-function page-tree pages global)))))))

(defun one-build ()
  "Build `one' web site of the current buffer under subdirectory `./public/'.

Also copy files in directory `./assets/' under the directory `./public/'.
If you've already built the web site and you are just working
on the content of the current buffer (meaning files in `./assets/'don't change),
you might prefer to use the command `one-build-only-html'which doesn't copy files
from `./assets/' directory."
  (interactive)
  (delete-directory "./public/" t)
  (copy-directory "./assets/" "./public/" nil nil 'copy-contents)
  (one-build-only-html))

;;; A default web site

(defvar one-default-css
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

.one-hl-tms-cmd-line {                       /* tms-cmd-line-face */
  background: #d3b2a1;
  color: #313b52;
  font-weight: bold;
	display: inline-block;
  border-radius: 0.2em;
  margin: 0.4em 0em;
  padding: 0.2em 0.2em;
}
.one-hl-tms-ps1-user-host {color: #d7af87;}  /* tms-ps1-user-host-face */
.one-hl-tms-ps1-directory {color: #ffd787;}  /* tms-ps1-directory-face */
.one-hl-tms-ps1-git {color: #ff8700;}        /* tms-ps1-git-face */

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
  "Default CSS used by `one'.

See `one-default-new-project' and `one-default-add-css-file'.")

(defvar one-default-org-content
  "* Home
:PROPERTIES:
:ONE: one-default-home
:CUSTOM_ID: /
:END:

This text is inserted before we list the pages in our web site.

* Page 1
:PROPERTIES:
:ONE: one-default
:CUSTOM_ID: /blog/page-1/
:END:

This page is rendered with the function ~one-default~ specified in the
org property ~ONE~.

** headline 1
*** headline 1.1

foo

*** headline 1.2

bar

** headline 2

As you can see, ~one-default~ doesn't add the table of content (TOC).
If you want a default function that adds the TOC to the page you can
use the function ~one-default-with-toc~ (see [[#/blog/page-2/]])

* Page 2
:PROPERTIES:
:ONE: one-default-with-toc
:CUSTOM_ID: /blog/page-2/
:END:

This page is rendered with the function ~one-default-with-toc~ specified
in the org property ~ONE~.

** headline 1
*** headline 1.1

foo

*** headline 1.2

bar

** headline 2

As you can see, ~one-default~ doesn't add the table of content (TOC).
If you don't want the table of content (TOC) to be added, you can use
the function ~one-default~ (see [[#/blog/page-1/]])
"
  "Default org file to start a new `one' project.

See `one-default-new-project'.")

(defun one-default-add-css-file ()
  "Add default css file `./assets/one.css' with the content `one-default-css'."
  (interactive)
  (make-directory "assets" t)
  (with-temp-file "./assets/one.css" (insert one-default-css)))

(defun one-default-new-project ()
  ""
  (interactive)
  (one-default-add-css-file)
  (with-temp-file "one.org" (insert one-default-org-content))
  (find-file "one.org"))

(defun one-default-home (page-tree pages global)
  ""
  (let* ((title (org-element-property :raw-value page-tree))
         (content (org-export-data-with-backend
                   (org-element-contents page-tree)
                   'one nil))
         (pages-list (one-default-pages pages))
         (website-name (one-default-website-name pages)))
    (jack-html
     "<!DOCTYPE html>"
     `(:html
       (:head
        (:meta (@ :name "viewport" :content "width=device-width,initial-scale=1"))
        (:link (@ :rel "stylesheet" :type "text/css" :href "/one.css"))
        (:title ,title))
       (:body
        (:div/header ,website-name)
        (:div.container
         (:div/home ,content)
         (:div/pages ,pages-list)))))))

(defun one-default (page-tree pages global)
  ""
  (let* ((title (org-element-property :raw-value page-tree))
         (path (org-element-property :CUSTOM_ID page-tree))
         (content (org-export-data-with-backend
                   (org-element-contents page-tree)
                   'one nil))
         (website-name (one-default-website-name pages)))
    (jack-html
     "<!DOCTYPE html>"
     `(:html
       (:head
        (:meta (@ :name "viewport" :content "width=device-width,initial-scale=1"))
        (:link (@ :rel "stylesheet" :type "text/css" :href "/one.css"))
        (:title ,title))
       (:body
        (:div/header (:a (@ :href "/") ,website-name))
        (:div.container
         (:div/page-title (:h1 ,title))
         ,content
         ,(one-default-nav path pages)))))))

(defun one-default-with-toc (page-tree pages global)
  ""
  (let* ((title (org-element-property :raw-value page-tree))
         (path (org-element-property :CUSTOM_ID page-tree))
         (content (org-export-data-with-backend
                   (org-element-contents page-tree)
                   'one nil))
         (website-name (one-default-website-name pages))
         (headlines (cdr (one-default-list-headlines page-tree))))
    (jack-html
     "<!DOCTYPE html>"
     `(:html
       (:head
        (:meta (@ :name "viewport" :content "width=device-width,initial-scale=1"))
        (:link (@ :rel "stylesheet" :type "text/css" :href "/one.css"))
        (:title ,title))
       (:body
        (:div/header (:a (@ :href "/") ,website-name))
        (:div.container
         (:div/page-title (:h1 ,title))
         ,(when headlines
            `(:div/toc
              (:div
               (:div "Table of content")
               (:div ,(one-default-toc headlines)))))
         ,content
         ,(one-default-nav path pages)))))))

(defun one-default-doc (page-tree pages global)
  ""
  (let* ((title (org-element-property :raw-value page-tree))
         (path (org-element-property :CUSTOM_ID page-tree))
         (content (org-export-data-with-backend
                   (org-element-contents page-tree)
                   'one nil))
         (website-name (one-default-website-name pages))
         (headlines (cdr (one-default-list-headlines page-tree)))
         (pages-list (one-default-pages pages)))
    (jack-html
     "<!DOCTYPE html>"
     `(:html
       (:head
        (:meta (@ :name "viewport" :content "width=device-width,initial-scale=1"))
        (:link (@ :rel "stylesheet" :type "text/css" :href "/one.css"))
        (:title ,title))
       (:body
        (:div/sidebar-mobile
         (:div/sidebar-left (@ :onclick "followSidebarLink()")
          (:div (:div ,website-name))
          ,pages-list)
         (:div/sidebar-main))
        (:div/header-doc
         (:svg/hamburger (@ :viewBox "0 0 24 24" :onclick "sidebarShow()")
          (:path (@ :d "M21,6H3V5h18V6z M21,11H3v1h18V11z M21,17H3v1h18V17z")))
         (:a (@ :href "/") ,website-name))
        (:div.container-doc
         (:div/sidebar ,pages-list)
         (:article
          (:div/page-title (:h1 ,title))
          ,(when headlines
             `(:div/toc
               (:div
                (:div "Table of content")
                (:div ,(one-default-toc headlines)))))
          ,content
          ,(one-default-nav path pages))))
       (:script "
function sidebarShow() {
  if (window.innerWidth < 481)
    document.getElementById('sidebar-left').style.width = '75vw';
  else {
    document.getElementById('sidebar-left').style.width = 'min(300px, 34vw)';
  }
  document.getElementById('sidebar-main').setAttribute('onclick', 'sidebarHide()');
  document.getElementById('sidebar-main').style.display = 'block';
}
function sidebarHide() {
  document.getElementById('sidebar-left').style.width = '0';
  document.getElementById('sidebar-main').style.display = 'none';
}
")))))

(defun one-default-pages (pages)
  "Return the list of PAGES as a `jack-html' component excluding the home page.

Evaluating the following form

    (one-default-pages
       '((:one-title \"HOME\" :one-path \"/\")
         (:one-title \"FOO-1\" :one-path \"/foo-1/\")
         (:one-title \"FOO-2\" :one-path \"/foo-2/\")))

returns:

    (:ul
     (:li (:a (@ :href \"/foo-1/\") \"FOO-1\"))
     (:li (:a (@ :href \"/foo-2/\") \"FOO-2\")))"
  (when-let ((li-items
              (delq nil
                    (mapcar
                     (lambda (page)
                       (let ((href (plist-get page :one-path))
                             (title (plist-get page :one-title)))
                         (when (not (string= href "/"))
                           `(:li (:a (@ :href ,href) ,title)))))
                     pages))))
    `(:ul ,@li-items)))

(defun one-default-website-name (pages)
  "Return the website's name.

This corresponds to the title (value of the property `:one-title')
of the page in PAGES whose path is \"/\" (the home page).
Return nil if the home page is not part of PAGES.

See `one-default-home',`one-default',`one-default-with-toc'
and `one-default-doc'."
  (seq-some
   (lambda (page)
     (when (string= (plist-get page :one-path) "/")
       (plist-get page :one-title)))
   pages))

(defun one-default-nav (path pages)
  "Return nav component for the default render functions."
  (let* ((pages-no-home
          (seq-remove
           (lambda (page) (string= (plist-get page :one-path) "/"))
           pages))
         (pages-no-home/path
          (seq-remove
           (lambda (page) (string= (plist-get page :one-path) path))
           pages-no-home)))
    (when (<= 2 (length pages-no-home))
      (let (prev
            (tail pages-no-home)
            (random (seq-random-elt pages-no-home/path)))
        (while (not (string= (plist-get (car tail) :one-path) path))
          (setq prev (car tail))
          (setq tail (cdr tail)))
        `(:div/nav
          ,(when prev `(:a (@ :href ,(plist-get prev :one-path)) "PREV"))
          ,(when (<= 3 (length pages-no-home))
             `(:a (@ :href ,(plist-get random :one-path)) "RANDOM"))
          ,(when-let ((next (plist-get (cadr tail) :one-path)))
             `(:a (@ :href ,next) "NEXT")))))))

(defun one-default-list-headlines (data)
  "Return the list in order of the headlines in the DATA.

Each headline in that list is a plist with the following properties
`:id',`:level' and `:title'.

See `one-default-toc'."
  (org-element-map data 'headline
    (lambda (elt)
      `(:id ,(org-element-property :one-internal-id elt)
        :level ,(org-element-property :level elt)
        :title ,(org-element-property :raw-value elt)))))

(defun one-default-toc (headlines)
  "Generate the TOC (a `jack' component) from the list HEADLINES of headlines.
See `jack-html', `one-default-list-headlines' and `one-default-with-toc'."
  (let* ((-headlines (cdr headlines))
         (stack (list (car headlines)))
         headline
         (anchor-title-id
          (lambda (title id)
            `(:a (@ :href ,(concat "#" id)) ,title)))
         (ul-or-child
          (lambda (title id child)
            (or (and title child `(:ul (:li ,(funcall anchor-title-id title id) ,child)))
                (and title `(:ul (:li ,(funcall anchor-title-id title id))))
                child))))
    (while -headlines
      (setq headline (pop -headlines))
      (while headline
        (if (>= (plist-get headline :level)
                (plist-get (car stack) :level))
            (progn (push headline stack)
                   (setq headline nil))
          (let* ((stack-0 (pop stack))
                 (level-0 (plist-get stack-0 :level))
                 (title-0 (plist-get stack-0 :title))
                 (id-0 (plist-get stack-0 :id))
                 (child-0 (plist-get stack-0 :child))
                 (stack-1 (pop stack))
                 (level-1 (plist-get stack-1 :level))
                 (title-1 (plist-get stack-1 :title))
                 (id-1 (plist-get stack-1 :id))
                 (child-1 (plist-get stack-1 :child))
                 (anchor-0 (funcall anchor-title-id title-0 id-0))
                 (anchor-1 (funcall anchor-title-id title-1 id-1)))
            (if (> level-0 level-1)
                (push `(:level ,level-1 :title ,title-1 :id ,id-1
                        :child ,(funcall ul-or-child title-0 id-0 child-0))
                      stack)
              (push
               `(:level ,level-1
                 :child
                 ,(if child-0
                      `(:ul
                        ,(if child-1 `(:li ,anchor-1 ,child-1) `(:li ,anchor-1))
                        ,@(cdr child-0))
                    `(:ul
                      ,(if child-1 `(:li ,anchor-1 ,child-1) `(:li ,anchor-1))
                      (:li ,anchor-0))))
               stack))))))
    ;; pop the stack
    (while (>= (length stack) 2)
      (let* ((stack-0 (pop stack))
             (level-0 (plist-get stack-0 :level))
             (title-0 (plist-get stack-0 :title))
             (id-0 (plist-get stack-0 :id))
             (child-0 (plist-get stack-0 :child))
             (stack-1 (pop stack))
             (level-1 (plist-get stack-1 :level))
             (title-1 (plist-get stack-1 :title))
             (id-1 (plist-get stack-1 :id))
             (child-1 (plist-get stack-1 :child))
             (anchor-0 (funcall anchor-title-id title-0 id-0))
             (anchor-1 (funcall anchor-title-id title-1 id-1)))
        (if (> level-0 level-1)
            (push `(:level ,level-1 :title ,title-1 :id ,id-1
                    :child ,(funcall ul-or-child title-0 id-0 child-0))
                  stack)
          (push
           `(:level ,level-1
             :child ,(if title-0
                         `(:ul
                           ,(if child-1 `(:li ,anchor-1 ,child-1) `(:li ,anchor-1))
                           ,(if child-0 `(:li ,anchor-0 ,child-0) `(:li ,anchor-0)))
                       `(:ul
                         ,(if child-1
                              `(:li ,anchor-1 ,child-1)
                            `(:li ,anchor-1))
                         ,@(cdr child-0))))
           stack))))
    (funcall
     ul-or-child
     (plist-get (car stack) :title)
     (plist-get (car stack) :id)
     (plist-get (car stack) :child))))

;;; one provide

(provide 'one)
;;; one.el ends here
