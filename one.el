;;; one.el --- Few functions to build static websites -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2022 Tony Aldon

;; Author: Tony Aldon <tony.aldon.adm@gmail.com>
;; Version: 0.0
;; Package-Requires: ((emacs "28.1") (jack "1.0") (htmlize "1.57"))
;; Keywords: html, org, static-site, blog
;; Homepage: https://github.com/tonyaldon/one.el

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

;;; Code

(require 'jack)
(require 'ox)
(require 'htmlize)

(defvar htmlize-buffer-places)

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

(org-export-define-backend 'one-ox
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

(defun one-ox-headline (headline contents _info)
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
                 (other (error "`one-ox' doesn't support list type: %s" other)))))
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

If IS-RESULTS-P is non-nil, CSS class of tag <code> in the returned
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

(defvar one-ox-link-image-extensions
  (format "\\.%s\\'"
          (regexp-opt
           '("webp" "avif" "png" "jpeg" "jpg" "gif"
             "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm")
           t))
  "Regexp matching image extensions.

See `one-ox-link'.")

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
                 (funcall export-func path desc 'one-ox info))))
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
        (and
         (string-match one-ox-link-image-extensions path)
         (format "<p><img src=\"%s\" alt=\"%s\" /></p>"
                 href (or (org-string-nw-p desc) href)) )
        (format "<a href=\"%s\">%s</a>"
                href (or (org-string-nw-p desc) href)))))

;;; Commands to build `one.el' web sites

(defvar one-add-to-global
  '((:one-global-property :one-tree
     :one-global-function (lambda (pages tree) tree)))
  "List used to set the `global' argument passed to render functions.

Elements in that list are plist with the following properties:

- `:one-global-property': a keyword that is used as proprety
  in the `global' argument passed to the render functions,
- `:one-global-function': a function that takes two arguments `pages'
  (list of pages, see `one-list-pages') and `tree'
  (see `one-parse-buffer').  That function is called once in
  `one-render-pages' and its result is used as the value of
  the property `:one-global-property' in the `global' argument
  passed to the render functions.

For instance, if `one-add-to-global' is set to

    ((:one-global-property :one-tree
      :one-global-function (lambda (pages tree) tree)))

then `global' local variable will be set to

    ((:one-tree tree))

where `tree' is the value returned by `one-parse-buffer' function.")

(defvar one-hook nil
  "List of functions called once in `one-render-pages'.

Those functions take three arguments:

- `pages': list of pages, see `one-list-pages',
- `tree': see `one-parse-buffer',
- `global': see `one-add-to-global'.

As those functions take `global' argument they are called after
that argument has been let binded using `one-add-to-global'.")

(defvar one-emacs-cmd-line-args-async nil
  "List of command line arguments to pass to `emacs' subprocess.

The function `one-render-pages-async' and `one-build-async' spawn an
`emacs' subprocess in order to build html pages asynchronously.  The
arguments passed to `emacs' depends on `one-emacs-cmd-line-args-async' value.

By default, when `one-emacs-cmd-line-args-async' is nil, we run `emacs'
in \"batch mode\", we load the user's initialization file and we evaluate
a specific sexp that builds html pages.  Specifically, we pass
the following `command' (`emacs' file name followed by command line
arguments) to `make-process' function like this:

    (let* ((emacs (file-truename
                   (expand-file-name invocation-name invocation-directory)))
           (command \\=`(,emacs \"--batch\"
                             \"-l\" ,user-init-file
                             \"--eval\" ,sexp))
           (sexp ...))
      (make-process
       :name ...
       :buffer ...
       :command command))

If `one-emacs-cmd-line-args-async' is non-nil, we no longer load the user's
initialization file and replace '\"-l\" ,user-init-file' in `command' above
by the elements of `one-emacs-cmd-line-args-async'.  For instance, if
`one-emacs-cmd-line-args-async' is equal to

    \\='(\"-l\" \"/path/to/some-elisp-file.el\")

then `command' becomes

    (let* (...
           (command \\=`(,emacs \"--batch\"
                             \"-l\" \"/path/to/some-elisp-file.el\"
                             \"--eval\" ,sexp))
           ...)
      ...)")

(define-error 'one-path "CUSTOM_ID not defined")

(defun one-internal-id (headline)
  "Return a string id for HEADLINE to be used as :one-internal-id property.

The id is built from CUSTOM_ID property of HEADLINE if set
or generated a randomly."
  (let* ((custom-id (org-element-property :CUSTOM_ID headline)))
    (or (and custom-id
             ;; we match "baz" in "/foo/bar/#baz"
             (string-match "\\`\\(?:[^#]+\\S-*\\)#\\(.+\\)" custom-id)
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
  "Return nil if HEADLINE element is not a `one.el' page.

If HEADLINE is a page, return a plist with the properties
`:one-title', `:one-path', `:one-render-page-function' and
`:one-page-tree' defined like this:

- `:one-title': the raw value of the first headline of HEADLINE,
- `:one-path': the path of the page as a string,
- `:one-render-page-function': the function to render the page as
  a symbol.  This function is declared in the org buffer for
  each page using the org property ONE.

  This function takes 3 arguments:

  - `page-tree' which correspond to the data in `:one-page-tree',
  - `pages' list of pages,
  - `global' a plist of global informations that are computed once
    when `one.el' website is built (before rendering the pages), see
    `one-render-pages' and `one-build'.  This argument can be
    modified by the user at build time.  That means that if your
    render function needs extra information you can tell `one.el' to
    compute those informations and to add them to `global'.

  You can see how to implement render functions looking at the
  default render functions `one-default-home', `one-default',
  `one-default-with-toc' and `one-default-doc'.

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

(defun one-render-page (page pages global)
  ""
  (let* ((path (concat "./public" (plist-get page :one-path)))
         (file (concat path "index.html"))
         (render-page-function (plist-get page :one-render-page-function))
         (page-tree (plist-get page :one-page-tree)))
    (make-directory path t)
    (with-temp-file file
      (insert (funcall render-page-function page-tree pages global)))))

;;;###autoload
(defun one-render-pages (&optional one-path)
  "Render webpages of the current buffer under `./public/' dir.

If ONE-PATH is non-nil, it must be the path of page in the current
buffer.  That means it must match the `CUSTOM_ID' org property value
of a level 1 headline.

If a file `onerc.el' exist in the current directory, it will be loaded
first.  This way we can customize how the website is built by adding
some Elisp code in that file.

Information in `one-add-to-global' are used to set the ‘global’ argument
passed to render functions.

Once `onerc.el' file has been loaded and `global' argument passed to
render functions set the hook `one-hook' is run.  Then the webpages
are rendered.

The current buffer should look like this.

    ---------- Buffer ----------
    * Home
    :PROPERTIES:
    :ONE: render-function-0
    :CUSTOM_ID: /
    :END:

    Content of the home page

    * Page 1
    :PROPERTIES:
    :ONE: render-function-1
    :CUSTOM_ID: /blog/page-1/
    :END:

    Content of page 1

    * Page 2
    :PROPERTIES:
    :ONE: render-function-2
    :CUSTOM_ID: /blog/page-2/
    :END:

    Content of page 2

    * I'm not a Page and I won't be rendered
    ---------- Buffer ----------

Each level 1 headline with the org properties `ONE' and
`CUSTOM_ID' set is a page.  See `one-is-page'.

How those pages are rendered and where they are rendered
depends on the render functions specified in `ONE' org
property and the path specified in `CUSTOM_ID' org property.

For instance, with the above buffer, assuming the render
functions `render-function-0' and `render-function-2'
are well defined and assuming the render function
`render-function-1' is defined like this

    (defun render-function-1 (page-tree pages global)
      \"<h1>Hello world!</h1>\")

calling the command `one-render-pages' produces
the following files

    .
    └── public
        ├── blog
        │   ├── page-1
        │   │   └── index.html
        │   └── page-2
        │       └── index.html
        └── index.html

and the content of the file `./public/blog/page-1/index.html' is

    ---------- File: ./public/blog/page-1/index.html ----------
    <h1>Hello world!</h1>
    ---------- File: ./public/blog/page-1/index.html ----------

Therefore if you serve the website in `./public/' directory at
`http://localhost:3000' you can access the \"Hello word!\" page
at `http://localhost:3000/blog/page-1/'.

You can see how to implement render functions looking at the
implementation of the default render functions `one-default-home',
`one-default', `one-default-with-toc' and `one-default-doc'.

Note that `one-render-pages' doesn't copy files from
`./assets/' directory to `./public/' directory.

See `one-build'.

If you want to start a new `one.el' project with the default style
see `one-default-new-project' command.

Note: I use https://browsersync.io to serve the website in `./public/'
directory.  Once you have it installed, to start a web server with
live reloading, you can run the following commands (in a terminal):

    $ cd public
    $ browser-sync start -s -w --files \"*\""
  (interactive)
  (let ((onerc (concat default-directory "onerc.el"))
        (inhibit-message t))
    (when (file-exists-p onerc) (load onerc)))
  (let* ((tree (org-with-wide-buffer (one-parse-buffer)))
         (pages (one-list-pages tree))
         (global
          (let (global)
            (dolist (glob one-add-to-global)
              (push (funcall (plist-get glob :one-global-function) pages tree)
                    global)
              (push (plist-get glob :one-global-property) global))
            global)))
    (dolist (hook one-hook) (funcall hook pages tree global))
    (if one-path
        (if-let ((page (seq-some
                        (lambda (page)
                          (when (string= (plist-get page :one-path) one-path)
                            page))
                        pages)))
            (progn
              (message "Build page `%s'..." one-path)
              (one-render-page page pages global)
              (message "Build page `%s'...done" one-path))
          (error "Page `%s' doesn't exist" one-path))
      (message "Build pages...")
      (dolist (page pages)
        (progn
          (message "Build page `%s'" (plist-get page :one-path))
          (one-render-page page pages global)))
      (message "Build pages...done"))))

;;;###autoload
(defun one-render-pages-async (&optional one-path)
  "Render webpages of the current buffer under `./public/' dir asynchronously.

The function `one-render-pages-async' spawns an `emacs' subprocess
in order to build html pages asynchronously.  The arguments passed to
`emacs' depends on `one-emacs-cmd-line-args-async' value.

If ONE-PATH is non-nil, it must be the path of page in the current
buffer.  That means it must match the `CUSTOM_ID'org property value
of a level 1 headline.

See `one-render-pages'."
  (interactive)
  (let* ((org-content (org-with-wide-buffer
                       (buffer-substring (point-min) (point-max))))
         (org-content-file (make-temp-file "one-content-" nil ".org"))
         (current-dir default-directory)
         (sexp (with-output-to-string
                 (prin1 `(progn
                           (require 'one)
                           (find-file ,org-content-file)
                           (setq default-directory ,current-dir)
                           (one-render-pages ,one-path)))))
         (emacs (file-truename
                 (expand-file-name invocation-name invocation-directory)))
         (command (if one-emacs-cmd-line-args-async
                      `(,emacs "--batch" ,@one-emacs-cmd-line-args-async "--eval" ,sexp)
                    `(,emacs "--batch" "-l" ,user-init-file "--eval" ,sexp)))
         (sentinel (lambda (process msg)
                     (internal-default-process-sentinel process msg)
                     (if (string-match-p "finished" msg)
                         (if one-path
                             (message "Build page `%s'...done" one-path)
                           (message "Build pages...done"))
                       (message "%s, check buffer `*one*'"
                                (string-trim-right msg)
                                (buffer-name (process-buffer process)))))))
    (with-temp-file org-content-file (insert org-content))
    (if one-path
        (message "Build page `%s'..." one-path)
      (message "Build pages..."))
    (let ((process-connection-type nil)
          (inhibit-message t))
      (make-process
       :name "one"
       :buffer (get-buffer-create "*one*")
       :command command
       :connection-type nil
       :sentinel sentinel))))

(defun one-page-at-point ()
  "Return `one-path' of the page at point.

Return nil if no page found.
Doesn't move point nor change the match data."
  (save-match-data
    (save-excursion
      (org-with-wide-buffer
       (goto-char (point-at-bol))
       (when (not (looking-at "^\\* "))
         (search-backward-regexp "^\\* " nil t))
       (org-element-property :CUSTOM_ID (org-element-at-point))))))

;;;###autoload
(defun one-render-page-at-point ()
  "Build page at point.

See `one-render-pages'."
  (interactive)
  (if-let ((one-path (one-page-at-point)))
      (one-render-pages one-path)
    (message "No page found at point")))

;;;###autoload
(defun one-render-page-at-point-async ()
  "Build page at point asynchronously.

See `one-render-pages-async'."
  (interactive)
  (if-let ((one-path (one-page-at-point)))
      (one-render-pages-async one-path)
    (message "No page found at point")))

(defun one-copy-assets-to-public ()
  "Copy `./assets/' files into `./public/' subdirectory."
  (interactive)
  (when (file-exists-p "./assets/")
    (copy-directory "./assets/" "./public/" nil t 'copy-contents)))

;;;###autoload
(defun one-build-async ()
  "Build website of the current buffer under `./public/' dir asynchronously.

The function `one-build-async' spawns an `emacs' subprocess in order to
render html pages, clean `./public/' directory and copy `./assets/'
directory asynchronously.  The arguments passed to `emacs' depends
on `one-emacs-cmd-line-args-async' value.

See `one-build'."
  (interactive)
  (let* ((org-content (org-with-wide-buffer
                       (buffer-substring (point-min) (point-max))))
         (org-content-file (make-temp-file "one-content-" nil ".org"))
         (current-dir default-directory)
         (sexp (with-output-to-string
                 (prin1 `(progn
                           (find-file ,org-content-file)
                           (setq default-directory ,current-dir)
                           (when (file-exists-p "./public/")
                             (dolist (file (cddr (directory-files "./public/" 'full)))
                               (if (file-directory-p file)
                                   (delete-directory file t)
                                 (delete-file file))))
                           (require 'one)
                           (one-copy-assets-to-public)
                           (one-render-pages)))))
         (emacs (file-truename
                 (expand-file-name invocation-name invocation-directory)))
         (command (if one-emacs-cmd-line-args-async
                      `(,emacs "--batch" ,@one-emacs-cmd-line-args-async "--eval" ,sexp)
                    `(,emacs "--batch" "-l" ,user-init-file "--eval" ,sexp)))
         (sentinel (lambda (process msg)
                     (internal-default-process-sentinel process msg)
                     (if (string-match-p "finished" msg)
                         (message "Build pages...done")
                       (message "%s, check buffer `*one*'"
                                (string-trim-right msg)
                                (buffer-name (process-buffer process)))))))
    (with-temp-file org-content-file (insert org-content))
    (message "Build pages...")
    (let ((process-connection-type nil)
          (inhibit-message t))
      (make-process
       :name "one"
       :buffer (get-buffer-create "*one*")
       :command command
       :connection-type nil
       :sentinel sentinel))))

;;;###autoload
(defun one-build ()
  "Build website of the current buffer under `./public/' subdirectory.

Specifically:

1) clean `./public/' subdirectory (if it exists),
2) copy `./assets/' files into `./public/' subdirectory and
3) call `one-render-pages' once.

See `one-render-pages'."
  (interactive)
  (when (file-exists-p "./public/")
    (dolist (file (cddr (directory-files "./public/" 'full)))
      (if (file-directory-p file)
          (delete-directory file t)
        (delete-file file))))
  (one-copy-assets-to-public)
  (one-render-pages))

;;; A default web site

(defvar one-default-css
  "@import url('https://fonts.googleapis.com/css2?family=Fira+Mono:wght@400&family=Noto+Sans:wght@400;700&display=swap');

html, body, p, ol, ul, li, dl, dt, dd,
blockquote, figure, fieldset, legend, textarea,
pre, iframe, hr, h1, h2, h3, h4, h5, h6 {
  margin: 0;
  padding: 0;
}

*, *::before, *::after {
  box-sizing: border-box;
}

p, blockquote, ul, ol, code,
dl, table, pre, details {
  margin-bottom: 16px;
  margin-top: 0;
}

ul {
  padding-left: 2em;
  list-style: disc;
}

ul ul {
  margin-top: 0;
  margin-bottom: 0;
}

ol {
  padding-left: 2em;
  list-style: decimal;
}

li p:first-of-type {
  margin: 0;
}

li p {
  margin: 16px 0;
}

li code {
  margin: 16px 0;
}

html {
  scroll-padding-top: 4rem; /* because we use a sticky header */
}

body {
  background: #151515;
  color: #dedede;
  font-family: \"Noto Sans\",sans-serif;
	font-size: 106%;
  line-height: 1.5;
  word-wrap: break-word;
}

h1 {
  font-size: 2em;
}

h2, h3, h4, h5, h6 {
  padding-bottom: 0.3em;
  margin-top: 24px;
  margin-bottom: 16px;
  font-weight: bold;
  line-height: 1.25;
}

h2, h3 {
  border-bottom: 1px solid #1d272b;
}

h2 {font-size: 2em;}
h3 {font-size: 1.5em;}
h4 {font-size: 1.25em;}
h5 {font-size: 1em;}
h6 {font-size: .875em;}

a {
  color: #ffd787;
  cursor: pointer;
  text-decoration: none;
}

a:hover {
  text-decoration: underline;
}

a:visited {
  color: #ffd787;
}

img {
  width: 100%;
  height: auto;
  border-radius: 6px;
}

/* ------- '.one' classes used by 'one-ox' org backend ------- */

.one-hl {
	font-family: 'Fira Mono', monospace;
  font-size: 80%;
  border-radius: 6px;
}

.one-hl-inline {
	background: #31424a;
  padding: 0.2em 0.4em;
  margin: 0;
  white-space: break-spaces;
}

.one-hl-block {
  background: #161f22;
  color: #c5c5c5;
  display: block;
  overflow: auto;
  padding: 16px;
  line-height: 1.45;
}

.one-blockquote {
  background: #202d31;
  border-left: 0.3em solid #31424a;
  margin: 0px auto 16px;
  padding: 1em 1em;
  width: 90%;
}

.one-blockquote > p:last-child {
  margin-bottom: 0;
}

.one-hl-results {
  background: #202d31 ;
  border-left: 2px solid #c5c5c5;
  display: block;
  margin: auto;
  padding: 0.5em 1em;
  overflow: auto;
  width: 98%;
}

.one-hl-negation-char { color: #ff6c60}              /* font-lock-negation-char-face */
.one-hl-warning { color: #fd971f}                    /* font-lock-warning-face */
.one-hl-variable-name { color: #fd971f}              /* font-lock-variable-name-face */
.one-hl-doc { color: #d3b2a1}                        /* font-lock-doc-face */
.one-hl-doc-string { color: #d3b2a1}                 /* font-lock-doc-string-face */
.one-hl-string { color: #d3b2a1}                     /* font-lock-string-face */
.one-hl-function-name { color: #02d2da}              /* font-lock-function-name-face */
.one-hl-builtin { color: #b2a1d3}                    /* font-lock-builtin-face */
.one-hl-type { color: #457f8b}                       /* font-lock-type-face */
.one-hl-keyword { color: #f92672}                    /* font-lock-keyword-face */
.one-hl-preprocessor { color: #f92672}               /* font-lock-preprocessor-face */
.one-hl-comment-delimiter { color: #8c8c8c}          /* font-lock-comment-delimiter-face */
.one-hl-comment { color: #8c8c8c}                    /* font-lock-comment-face */
.one-hl-constant { color: #f5ebb6}                   /* font-lock-constant-face */
.one-hl-reference { color: #f5ebb6}                  /* font-lock-reference-face */
.one-hl-regexp-grouping-backslash { color: #966046}  /* font-lock-regexp-grouping-backslash */
.one-hl-regexp-grouping-construct { color: #aa86ee}  /* font-lock-regexp-grouping-construct */
.one-hl-number { color: #eedc82}                     /* font-lock-number-face */

.one-hl-sh-quoted-exec { color: #62bd9c}             /* sh-quoted-exec */

/* -------- scrollbar -------- */

::-webkit-scrollbar {
  width: 1em;
  height: 1em;
}

::-webkit-scrollbar-track {
  background: #202d31;
}

::-webkit-scrollbar-thumb {
  background: #31424a;
  border-radius: 0.5em;
}

::-webkit-scrollbar-thumb:hover {
  background: #31424a;
}

/* -------- specific to the default render functions -------- */

.header {
  color: #ffffff;
  font-size: 2em;
  font-weight: bold;
  padding: 0 16px 0 16px;
  background: #151515;
  width: 100%;
  height: 3.5rem;
  position: fixed;
  top: 0;
  left: 0;
  border-bottom: 1px solid #1d272b;
  display: flex;
  justify-content: center;
  align-items: center;
}

.header > a {
  color: inherit;
  cursor: pointer;
  text-decoration: none;
}

.header > a:visited {
  color: inherit;
}

.content {
  margin: 3.5rem auto;
  padding-top: 1.8rem;
  max-width: 740px;
  padding: 0 16px;
}

.title {
  text-align: center;
  padding: 1.8rem 0;
}

/* -------- one-default-home -------- */

#home {
  margin: 5rem 0 1.5rem 0;
}

/* -------- one-default-home-list-pages -------- */

#home-list-pages {
  margin: 5rem 0 1.5rem 0;
}

#pages ul {
  padding: 0;
  list-style: none;
}

#pages a {
  display: block;
  line-height: 1.2em;
  font-size: 1.2em;
  color: #dedede;
  border-bottom: 1px solid #1d272b;
  padding: 1em 0.3em;
}

#pages a:hover {
  text-decoration: none;
  background: #31424a;
  color: #ffffff;
}

/* -------- one-default, one-default-with-toc, one-default-doc -------- */

.nav {
  border-top: 1px solid #c5c5c5;
  margin-top: 3em;
  padding: 2em 0;
  display: flex;
  justify-content: center;
  gap: 0.5em;
  font-weight: bold;
}

.nav a {
  display: block;
  background: #dedede;
  border-radius: 6px;
  padding: 0.2em 0.8em;
  color: #151515;
  width: 20%;
  text-align: center;
}

@media (max-width:600px) {
  .nav a {
    width: auto;
  }
}

/* -------- one-default-with-toc, one-default-doc -------- */

.toc {
  display: flex;
  justify-content: center;
  margin-bottom: 1.8rem;
  color: #d1d1d1;
}

.toc > div {
  padding: 0 1em;
}

.toc a {
  color: #d1d1d1;
}

.toc > div > div:first-child {
  text-decoration: underline 1px;
  text-align: center;
  font-size: 1.2em;
  margin-bottom: 16px;
}

/* --------- one-default-doc --------- */

#header-doc {
  color: #ffffff;
  font-size: 2em;
  font-weight: bold;
  padding: 0 16px 0 16px;
  background: #151515;
  width: 100%;
  height: 3.5rem;
  position: fixed;
  top: 0;
  left: 0;
  border-bottom: 1px solid #1d272b;
  display: flex;
  justify-content: center;
  align-items: center;
}

#header-doc > a {
  color: inherit;
  cursor: pointer;
  text-decoration: none;
}

#header-doc > a:visited {
  color: inherit;
}

#hamburger {
  cursor: pointer;
  height: 1em;
  fill: #dedede;
  display: none;
  font-weight: normal;
  margin-right: 0.3em;
}

#content-doc {
  margin: 3.5rem auto;
  display: flex;
  margin-left: auto;
  margin-right: auto;
  max-width: 1140px;
  width: 100%;
  padding: 1em 16px;
}

#sidebar {
  border-right: 2px solid #31424a;
  top: 4.5rem;
  position: sticky;
  padding-top: 2.2em;
  padding-bottom: 6em;
  width: 250px;
  max-height: 100vh;
  overflow-y: auto;
}

#sidebar a {
  display: block;
  color: #dedede;
}

#sidebar a:hover {
  text-decoration: none;
}

#sidebar ul {
  list-style: none;
  padding:0;
}

#sidebar li {
  padding: 0.5em 0.6em;
}

#sidebar li:hover {
  background: #31424a;
}

article {
  padding: 0 1.5em;
  max-width: 640px;
  width: 100%;
}

#sidebar-left {
  width: 0;
  height: 100%;
  position: fixed;
  z-index: 3;
  top: 0;
  left: 0;
  transition: 0.25s;
	background: #2c444f;
  overflow: hidden; /* to make the children disappear when width is 0 */
  overflow-y: auto;
}

#sidebar-left > div:first-child {
  height: 3.5rem;
  font-size: 2em;
  font-weight: bold;
  border-bottom: 1px solid #b8b8b8;
  padding-left: 16px;
  margin-bottom: 16px;
  display: flex;
  align-items: center;
}

#sidebar-left > ul {
  padding: 0 16px 0 16px;
}

#sidebar-left > ul ul {
  padding-left: 0.8em;
  margin-left: 3px;
  border-left: 1px solid #b8b8b8;
}

#sidebar-left a {
  color: #dedede;
  text-decoration: none;
}

#sidebar-left li {
  padding: 0.5em 0;
  list-style-type: none;
}

#sidebar-main {
  display: none;
  top: 0;
  right: 0;
  width: 100%;
  height: 100%;
  position: fixed;
  background: #080808;
  opacity: 0.80;
  z-index: 2;
}

@media (max-width: 840px) {
  #hamburger {
    display: block;
  }
  #sidebar {
    display: none;
  }
  #content-doc {
    justify-content: center;
  }
  #header-doc {
    justify-content: left;
  }
  article {
    padding: 0;
  }
}
"
  "Default CSS style sheet.

This style sheet is meant to be used with the default render functions
`one-default-home',`one-default',`one-default-with-toc',`one-default-doc'.

See `one-default-new-project' and `one-default-add-css-file'.")

(defvar one-default-org-content
  "* one.el
:PROPERTIES:
:ONE: one-default-home
:CUSTOM_ID: /
:END:

This is a new ~one.el~ project.

If you don't know how ~one.el~ works, you can check the documentation at
https://one.tonyaldon.com.

If you want to list all the pages on your website on the home page,
check [[#/blog/default-home-list-pages/][List all website's pages on the home page]].

* List all website's pages on the home page
:PROPERTIES:
:ONE: one-default-home-list-pages
:CUSTOM_ID: /blog/default-home-list-pages/
:END:

This page is rendered with the default render function
~one-default-home-list-pages~ specified in ~ONE~ org property which lists
below all the pages on the website.  You can use it instead of
~one-default-home~ for your home page.

* The default page
:PROPERTIES:
:ONE: one-default
:CUSTOM_ID: /blog/default/
:END:

This page is rendered with the default render function ~one-default~
specified in ~ONE~ org property.

** Do you want a table of content?

As you can see, ~one-default~ doesn't add a table of content (TOC).  If
you want a default render function that adds the TOC to the page you can
use the render function ~one-default-with-toc~ presented in [[#/blog/one-default-with-toc/][The default
page with a TOC]].

** Headline foo
*** Headline bar

Some content.

*** Headline baz

#+BEGIN_SRC bash :results verbatim
tree
#+END_SRC

#+RESULTS:
#+begin_example
.
├── assets
│   └── one.css
├── one.org
└── public
    ├── blog
    │   ├── default
    │   │   └── index.html
    │   ├── default-home-list-pages
    │   │   └── index.html
    │   ├── one-default-doc
    │   │   └── index.html
    │   └── one-default-with-toc
    │       └── index.html
    ├── index.html
    └── one.css

7 directories, 8 files
#+end_example

* The default page with a TOC
:PROPERTIES:
:ONE: one-default-with-toc
:CUSTOM_ID: /blog/one-default-with-toc/
:END:

This page is rendered with the render function ~one-default-with-toc~
specified in the org property ~ONE~.

** Do you want a sidebar?

Perhaps you want a sidebar listing all the pages on our website, as
many modern documentation sites do.  If so, you can use the default
render function ~one-default-doc~ presented in [[#/blog/one-default-doc/][The default page with TOC
and sidebar]].

** Headline foo
*** Headline bar

Some content.

*** Headline baz

#+BEGIN_SRC bash :results verbatim
tree
#+END_SRC

#+RESULTS:
#+begin_example
.
├── assets
│   └── one.css
├── one.org
└── public
    ├── blog
    │   ├── default
    │   │   └── index.html
    │   ├── default-home-list-pages
    │   │   └── index.html
    │   ├── one-default-doc
    │   │   └── index.html
    │   └── one-default-with-toc
    │       └── index.html
    ├── index.html
    └── one.css

7 directories, 8 files
#+end_example

* The default page with TOC and sidebar
:PROPERTIES:
:ONE: one-default-doc
:CUSTOM_ID: /blog/one-default-doc/
:END:

This page is rendered with the function ~one-default-doc~ specified
in the org property ~ONE~.

** Do you want to know more about one.el?

Check the documentation at https://one.tonyaldon.com.

** Headline foo
*** Headline bar

Some content.

*** Headline baz

#+BEGIN_SRC bash :results verbatim
tree
#+END_SRC

#+RESULTS:
#+begin_example
.
├── assets
│   └── one.css
├── one.org
└── public
    ├── blog
    │   ├── default
    │   │   └── index.html
    │   ├── default-home-list-pages
    │   │   └── index.html
    │   ├── one-default-doc
    │   │   └── index.html
    │   └── one-default-with-toc
    │       └── index.html
    ├── index.html
    └── one.css

7 directories, 8 files
#+end_example
"
  "Default org file to start a new `one' project.

See `one-default-new-project'.")

(defun one-default-add-css-file ()
  "Add default css file `./assets/one.css' with the content `one-default-css'.

See `one-default-new-project'.

See the default render functions `one-default-home',`one-default',
`one-default-with-toc',`one-default-doc'."
  (interactive)
  (make-directory "assets" t)
  (with-temp-file "./assets/one.css" (insert one-default-css)))

;;;###autoload
(defun one-default-new-project ()
  "Initialize a new `one.el' project in the current directory with the default style.

It is structured like this:

    .
    ├── assets
    │   └── one.css
    └── one.org

The content of the file `./assets/one.css' is `one-default-css'.
The content of the file `./one.org' is `one-default-org-content'.

Once you've initialized this new `one.el' project, you can build it
calling `one-build' command while visiting the file `./one.org'.
This results in producing the website under the subdirectory `./public/'.

See `one-render-pages'."
  (interactive)
  (one-default-add-css-file)
  (with-temp-file "one.org" (insert one-default-org-content))
  (find-file "one.org"))

(defun one-default-home (page-tree pages _global)
  "Default render function to use in the home page.

See `one-is-page', `one-render-pages' and `one-default-css'."
  (let* ((title (org-element-property :raw-value page-tree))
         (content (org-export-data-with-backend
                   (org-element-contents page-tree)
                   'one-ox nil))
         (website-name (one-default-website-name pages)))
    (jack-html
     "<!DOCTYPE html>"
     `(:html
       (:head
        (:meta (@ :name "viewport" :content "width=device-width,initial-scale=1"))
        (:link (@ :rel "stylesheet" :type "text/css" :href "/one.css"))
        (:title ,title))
       (:body
        (:div.header ,website-name)
        (:div.content
         (:div/home ,content)))))))

(defun one-default-home-list-pages (page-tree pages _global)
  "Default render function to use in the home page that lists pages.

See `one-is-page', `one-render-pages' and `one-default-css'."
  (let* ((title (org-element-property :raw-value page-tree))
         (content (org-export-data-with-backend
                   (org-element-contents page-tree)
                   'one-ox nil))
         (website-name (one-default-website-name pages))
         ;; All pages but the home pages
         (pages-list (one-default-pages pages "/.+")))
    (jack-html
     "<!DOCTYPE html>"
     `(:html
       (:head
        (:meta (@ :name "viewport" :content "width=device-width,initial-scale=1"))
        (:link (@ :rel "stylesheet" :type "text/css" :href "/one.css"))
        (:title ,title))
       (:body
        (:div.header (:a (@ :href "/") ,website-name))
        (:div.content
         (:div/home-list-pages ,content)
         (:div/pages (:ul ,(reverse pages-list)))))))))

(defun one-default (page-tree pages _global)
  "Default render function.

See `one-is-page', `one-render-pages' and `one-default-css'."
  (let* ((title (org-element-property :raw-value page-tree))
         (path (org-element-property :CUSTOM_ID page-tree))
         (content (org-export-data-with-backend
                   (org-element-contents page-tree)
                   'one-ox nil))
         (website-name (one-default-website-name pages))
         (nav (one-default-nav path pages)))
    (jack-html
     "<!DOCTYPE html>"
     `(:html
       (:head
        (:meta (@ :name "viewport" :content "width=device-width,initial-scale=1"))
        (:link (@ :rel "stylesheet" :type "text/css" :href "/one.css"))
        (:title ,title))
       (:body
        (:div.header (:a (@ :href "/") ,website-name))
        (:div.content
         (:div.title
          ,(when (not (string= path "/"))
             `(:h1 ,title)))
         ,content
         ,nav))))))

(defun one-default-with-toc (page-tree pages _global)
  "Default render function with a table of content.

See `one-is-page', `one-render-pages' and `one-default-css'."
  (let* ((title (org-element-property :raw-value page-tree))
         (path (org-element-property :CUSTOM_ID page-tree))
         (content (org-export-data-with-backend
                   (org-element-contents page-tree)
                   'one-ox nil))
         (website-name (one-default-website-name pages))
         (headlines (cdr (one-default-list-headlines page-tree)))
         (toc (when headlines
                `(:div.toc
                  (:div
                   (:div "Table of content")
                   (:div ,(one-default-toc headlines))))))
         (nav (one-default-nav path pages)))
    (jack-html
     "<!DOCTYPE html>"
     `(:html
       (:head
        (:meta (@ :name "viewport" :content "width=device-width,initial-scale=1"))
        (:link (@ :rel "stylesheet" :type "text/css" :href "/one.css"))
        (:title ,title))
       (:body
        (:div.header (:a (@ :href "/") ,website-name))
        (:div.content
         (:div.title
          ,(when (not (string= path "/"))
             `(:h1 ,title)))
         ,toc
         ,content
         ,nav))))))

(defun one-default-doc (page-tree pages _global)
  "Default render function with a table of content and a sidebar listing pages.

See `one-is-page', `one-render-pages', `one-default-css' and `one-default-pages'."
  (let* ((title (org-element-property :raw-value page-tree))
         (path (org-element-property :CUSTOM_ID page-tree))
         (content (org-export-data-with-backend
                   (org-element-contents page-tree)
                   'one-ox nil))
         (website-name (one-default-website-name pages))
         (pages-list (one-default-pages pages))
         (headlines (cdr (one-default-list-headlines page-tree)))
         (toc (when headlines
                `(:div.toc
                  (:div
                   (:div "Table of content")
                   (:div ,(one-default-toc headlines))))))
         (nav (one-default-nav path pages)))
    (jack-html
     "<!DOCTYPE html>"
     `(:html
       (:head
        (:meta (@ :name "viewport" :content "width=device-width,initial-scale=1"))
        (:link (@ :rel "stylesheet" :type "text/css" :href "/one.css"))
        (:title ,title))
       (:body
        ;; sidebar-left and sidebar-main are for small devices
        (:div/sidebar-left (@ :onclick "followSidebarLink()")
         (:div (:div "Pages"))
         ,pages-list)
        (:div/sidebar-main)
        (:div/header-doc
         (:svg/hamburger (@ :viewBox "0 0 24 24" :onclick "sidebarShow()")
          (:path (@ :d "M21,6H3V5h18V6z M21,11H3v1h18V11z M21,17H3v1h18V17z")))
         (:a (@ :href "/") ,website-name))
        (:div/content-doc
         (:div/sidebar ,pages-list)
         (:article
          (:div.title
           ,(when (not (string= path "/"))
              `(:h1 ,title)))
          ,toc
          ,content
          ,nav)))
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

(defun one-default-pages (pages &optional filter)
  "Return `jack-html' list of PAGES component.

If FILTER is non-nil, a page is listed only when its path (value
of `:one-path' property) matches FILTER regexp.

Evaluating the following form

    (one-default-pages
     \\='((:one-title \"HOME\" :one-path \"/\")
       (:one-title \"FOO-1\" :one-path \"/foo-1/\")
       (:one-title \"FOO-2\" :one-path \"/foo-2/\")))

returns:

    (:ul
     (:li (:a (@ :href \"/\") \"HOME\"))
     (:li (:a (@ :href \"/foo-1/\") \"FOO-1\"))
     (:li (:a (@ :href \"/foo-2/\") \"FOO-2\")))

And evaluating the following form with the filter \"/.+\"

    (one-default-pages
     \\='((:one-title \"HOME\" :one-path \"/\")
       (:one-title \"FOO-1\" :one-path \"/foo-1/\")
       (:one-title \"FOO-2\" :one-path \"/foo-2/\"))
       \"/.+\")

returns a list which doesn't include the home page:

    (:ul
     (:li (:a (@ :href \"/foo-1/\") \"FOO-1\"))
     (:li (:a (@ :href \"/foo-2/\") \"FOO-2\")))"
  (when-let ((li-items
              (delq nil
                    (mapcar
                     (lambda (page)
                       (let ((href (plist-get page :one-path))
                             (title (plist-get page :one-title)))
                         (when (string-match-p (or filter ".*") href)
                           `(:li (:a (@ :href ,href) ,title)))))
                     pages))))
    `(:ul ,@li-items)))

(defun one-default-website-name (pages)
  "Return the website's name.

This corresponds to the title (value of the property `:one-title')
of the page in PAGES whom path is \"/\" (the home page).
Return nil if the home page is not part of PAGES.

See `one-default-home',`one-default',`one-default-with-toc'
and `one-default-doc'."
  (seq-some
   (lambda (page)
     (when (string= (plist-get page :one-path) "/")
       (plist-get page :one-title)))
   pages))

(defun one-default-nav (path pages)
  "Return `jack-html' navigation component.

The component is composed of 3 links:

- \"PREV\": link to the page before the page whose `:one-path' is
  equal to PATH in PAGES,
- \"RANDOM\": link to a random page picked in PAGES whose `:one-path'
  is not equal to PATH,
- \"NEXT\": link to the page after the page whose `:one-path' is
  equal to PATH in PAGES.

For instance, evaluating the following form

    (one-default-nav \"/foo-2/\"
                     \\='((:one-path \"/\")
                       (:one-path \"/foo-1/\")
                       (:one-path \"/foo-2/\")
                       (:one-path \"/foo-3/\")
                       (:one-path \"/foo-4/\")))

returns (the \"RANDOM\" link could have been \"/\", \"/foo-1/\" or \"/foo-3/\")

    (:div.nav
     (:a (@ :href \"/foo-1/\") \"PREV\")
     (:a (@ :href \"/foo-4/\") \"RANDOM\")
     (:a (@ :href \"/foo-3/\") \"NEXT\"))

See `one-default',`one-default-with-toc' and `one-default-doc'."

  (let* ((pages-not-path
          (seq-remove
           (lambda (page) (string= (plist-get page :one-path) path))
           pages)))
    (when (<= 2 (length pages))
      (let (prev
            (tail pages)
            (random (seq-random-elt pages-not-path)))
        (while (not (string= (plist-get (car tail) :one-path) path))
          (setq prev (car tail))
          (setq tail (cdr tail)))
        `(:div.nav
          ,(when prev `(:a (@ :href ,(plist-get prev :one-path)) "PREV"))
          ,(when (<= 3 (length pages))
             `(:a (@ :href ,(plist-get random :one-path)) "RANDOM"))
          ,(when-let ((next (plist-get (cadr tail) :one-path)))
             `(:a (@ :href ,next) "NEXT")))))))

(defun one-default-list-headlines (tree)
  "Return the list in order of the headlines in TREE.

TREE is meant to be the parsed tree of an org buffer of a website
we want to build.  See `one-parse-buffer'.

Each headline in the returned list is a plist with the following
properties `:id',`:level' and `:title'.

We can use the list returned by `one-default-list-headlines' to build
a table of content of TREE using `one-default-toc'.

See `one-default-with-toc' and `one-default-doc'."
  (org-element-map tree 'headline
    (lambda (elt)
      `(:id ,(org-element-property :one-internal-id elt)
        :level ,(org-element-property :level elt)
        :title ,(org-element-property :raw-value elt)))))

(defun one-default-toc (headlines)
  "Return table of content (TOC) as an HTML string.

The TOC returned is computed from the ordered flat list of
headlines in HEADLINES where the level of each
headline is given by the property `:level'.
See `one-default-list-headlines'.

For instance, evaluating the following form

    (one-default-toc
     \\='((:level 1 :title \"foo\"    :id \"id-foo\")
       (:level 1 :title \"bar-1\"  :id \"id-bar-1\")
       (:level 2 :title \"bar-2\"  :id \"id-bar-2\")
       (:level 3 :title \"bar-3\"  :id \"id-bar-3\")
       (:level 2 :title \"bar-22\" :id \"id-bar-22\")
       (:level 1 :title \"baz\"    :id \"id-baz\")))

returns

     \"
     <ul>
     <li><a href=\\\"#id-foo\\\">foo</a></li>
     <li><a href=\\\"#id-bar-1\\\">bar-1</a>
     <ul>
     <li><a href=\\\"#id-bar-2\\\">bar-2</a>
     <ul>
     <li><a href=\\\"#id-bar-3\\\">bar-3</a></li>
     </ul>
     </li>
     <li><a href=\\\"#id-bar-22\\\">bar-22</a></li>
     </ul>
     </li>
     <li><a href=\\\"#id-baz\\\">baz</a></li>
     </ul>
     \"

See `one-default-with-toc' and `one-default-doc'."

  (let* ((prev-level (1- (plist-get (car headlines) :level)))
	       (start-level prev-level)
         (concat-n-times
          (lambda (n str) (apply #'concat (make-list n str)))))
    (concat
     (mapconcat
      (lambda (headline)
	      (let ((title (plist-get headline :title))
              (href (concat "#" (plist-get headline :id)))
	            (level (plist-get headline :level)))
	        (concat
	         (let* ((delta (- level prev-level))
		              (times (if (> delta 0) (1- delta) (- delta))))
	           (setq prev-level level)
	           (concat
	            (funcall concat-n-times
                       times (cond ((> delta 0) "\n<ul>\n<li>")
			                             ((< delta 0) "</li>\n</ul>\n")))
	            (if (> delta 0) "\n<ul>\n<li>" "</li>\n<li>")))
	         (concat "<a href=\"" href "\">" title "</a>"))))
      headlines "")
     (funcall concat-n-times (- prev-level start-level) "</li>\n</ul>\n"))))

;;; one provide

(provide 'one)
;;; one.el ends here
