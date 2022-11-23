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

    (subscript . one-ox-subscript)
    (superscript . one-ox-superscript)

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
         ;; the property `:one-internal-id' is set by `one'
         ;; mechanism, spefically the the function `one-list-pages'.
         ;; This allow to produce unified ids that can be
         ;; use to build a TOC for each page.
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
  "Parse current org buffer.

The only difference with `org-element-parse-buffer' is that
we add the property `:one-internal-id' to each headline."
  (let ((tree (org-element-parse-buffer)))
    ;; destructively add :one-internal-id property to headlines in `tree'
    (org-element-map tree 'headline
      (lambda (elt)
        (org-element-put-property
         elt :one-internal-id (one-internal-id elt))))
    tree))

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
  (org-element-map (one-parse-buffer) 'headline
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
         :one-headlines
         (org-element-map elt 'headline
           (lambda (elt)
             `(:id ,(org-element-property :one-internal-id elt)
               :level ,(org-element-property :level elt)
               :title ,(org-element-property :raw-value elt)))))))))

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

(defun one-build ()
  "Build `one' web site of the current buffer under subdirectory `./public/'.

Also copy files in directory `./assets/' under the directory `./public/'.
If you've already built the web site and you are just working
on the content of the current buffer (meaning files in `./assets/'
                                              don't change), you might prefer to use the command `one-build-only-html'
which doesn't copy files from `./assets/' directory."
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

(defun one-default-home (tree headlines &optional pages)
  ""
  (let ((org-export-with-sub-superscripts nil)
        (org-export-use-babel nil)
        (title (org-element-property :raw-value tree))
        (content (org-export-data-with-backend
                  (org-element-contents tree) 'one nil)))
    (jack-html
     "<!DOCTYPE html>"
     `(:html
       (:head
        (:meta (@ :name "viewport" :content "width=device-width,initial-scale=1"))
        (:link (@ :rel "stylesheet" :type "text/css" :href "/one.css"))
        (:title ,title))
       (:div.container
        (:body
         (:div (@ :style "text-align: center;") ,(upcase title))
         ,content))))))

(defun one-default (tree headlines &optional pages)
  ""
  (let ((org-export-with-sub-superscripts nil)
        (org-export-use-babel nil)
        (title (org-element-property :raw-value tree))
        (content (org-export-data-with-backend
                  (org-element-contents tree) 'one nil)))
    (jack-html
     "<!DOCTYPE html>"
     `(:html
       (:head
        (:meta (@ :name "viewport" :content "width=device-width,initial-scale=1"))
        (:link (@ :rel "stylesheet" :type "text/css" :href "/one.css"))
        (:title ,title))
       (:div.container
        (:body
         (:div (@ :style "text-align: center;") ,(upcase title))
         ,content))))))

(defun one-default-with-toc (tree headlines &optional pages)
  ""
  (let ((org-export-with-sub-superscripts nil)
        (org-export-use-babel nil)
        (title (org-element-property :raw-value tree))
        (content (org-export-data-with-backend
                  (org-element-contents tree) 'one nil)))
    (jack-html
     "<!DOCTYPE html>"
     `(:html
       (:head
        (:meta (@ :name "viewport" :content "width=device-width,initial-scale=1"))
        (:link (@ :rel "stylesheet" :type "text/css" :href "/one.css"))
        (:title ,title))
       (:div.container
        (:body
         (:div (@ :style "text-align: center;") ,(upcase title))
         ,(one-default--toc (cdr headlines))
         ,content))))))

(defun one-default--toc (headlines)
  "Generate the TOC (a `jack' component) from the list HEADLINES of headlines.
See `jack-html', `one-list-pages' and `one-default-with-toc'."
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
                 (:ul
                  (:li ,anchor-1 ,child-1)
                  ,(if child-0 `(:li ,anchor-0 child-0) `(:li ,anchor-0))))
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
