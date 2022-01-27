;;; osta.el --- few functions to build static websites -*- lexical-binding: t; -*-
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

;;; utils

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

;;; osta-ox

(require 'ox)

;;;; osta backend

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

    (link . osta-ox-link))
  :options-alist
  '((:osta-root "OSTA_ROOT" nil "public")
    (:osta-assets "OSTA_ASSETS" nil "assets")))

;;;; export/rendering

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
                                     (org-export-as 'osta nil nil nil `(:osta-links ,(osta-map-links))))))
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

;;;; headline, section, paragraph, etc.

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

(defun osta-ox-paragraph (_paragraph contents _info)
  (format "<p>%s</p>" contents))

(defun osta-ox-plain-text (text _info)
  (osta-escape text))

(defun osta-ox-bold (_bold contents _info)
  (format "<b>%s</b>" contents))

(defun osta-ox-italic (_italic contents _info)
  (format "<i>%s</i>" contents))

(defun osta-ox-strike-through (_strike-through contents _info)
  (format "<del>%s</del>" contents))

(defun osta-ox-underline (_underline contents _info)
  (format "<u>%s</u>" contents))

(defun osta-ox-code (code _contents _info)
  (format "<code class=\"osta-hl osta-hl-inline\">%s</code>"
          (osta-escape (org-element-property :value code))))

(defun osta-ox-verbatim (verbatim _contents _info)
  (format "<code class=\"osta-hl osta-hl-inline\">%s</code>"
          (osta-escape (org-element-property :value verbatim))))

(defun osta-ox-plain-list (plain-list contents _info)
  (let* ((type (pcase (org-element-property :type plain-list)
                 (`ordered "ol")
                 (`unordered "ul")
                 (other (error "`osta' doesn't support list type: %s" other)))))
    (format "<%s>%s</%s>" type contents type)))

(defun osta-ox-item (_item contents _info)
  (format "<li>%s</li>" contents))

(defun osta-ox-subscript (_subscript contents _info)
  (format "<sub>%s</sub>" contents))

(defun osta-ox-superscript (_superscript contents _info)
  (format "<sup>%s</sup>" contents))

;;;; blocks

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

;;;; links

(defun osta-map-links ()
  "Return an alist of (LINK-EXPANDED . TARGET) in current buffer.

Those links are defined by the org keyword `OSTA_LINK', like this:

  #+OSTA_LINK: link --> target

This osta link is a mapping between a LINK (first part in \"link --> target\")
that org commands (related to visiting links, etc) understand,
and a TARGET that is either:
- the path to an available file in the website (exported by `osta'),
- or a valid URL that point to an existing target on the web.

If LINK in \"#+OSTA_LINK: link --> target\" contains an org
abbreviated link, in the mapping, LINK is replaced by its expanded
version computed by `org-link-expand-abbrev'.  Note: this expansion
works only when the variable `org-link-abbrev-alist-local' is set.
This can be done by the function `org-export-get-environment'.
`osta-map-links' assumes that `org-link-abbrev-alist-local' is
already set.

Here is an example.

In a org-mode buffer with the following content:

#+LINK: abbrev-link /path/to/project/
#+OSTA_LINK: abbrev-link:file-1.clj::(defn func-1 --> https://github.com/user/project/blob/master/file-1.clj#L12
#+OSTA_LINK: abbrev-link:file-2.clj::(defn func-2 --> https://github.com/user/project/blob/master/file-2.clj#L56

`osta-map-links' returns:

 ((\"/path/to/project/file-1.clj::(defn func-1\" . \"https://github.com/user/project/blob/master/file-1.clj#L12\")
  (\"/path/to/project/file-2.clj::(defn func-2\" . \"https://github.com/user/project/blob/master/file-2.clj#L56\"))
"
  (when-let* ((osta-links (cdar (org-collect-keywords '("OSTA_LINK"))))
              (map-link (lambda (osta-link)
                          (and (string-match "\\`\\(.+\\S-\\)[ \t]+-->[ \t]*\\(.+\\)" osta-link)
                               (cons (match-string-no-properties 1 osta-link)
                                     (match-string-no-properties 2 osta-link)))))
              (osta-links-alist (delq nil (mapcar map-link osta-links))))
    (mapcar (lambda (l) (cons (org-link-expand-abbrev (car l)) (cdr l)))
            osta-links-alist)))

(define-error 'osta-link-broken "Unable to resolve link")

(define-error 'osta-options "Option not defined")

(defun osta-ox-link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information."
  (let* ((osta-links (plist-get info :osta-links))
         (osta-root (plist-get info :osta-root))
         (osta-assets (plist-get info :osta-assets))
         ;; (root-assets-re (concat "\\`\\./" "\\(" osta-root "\\|" osta-assets "\\)"))
         (type (org-element-property :type link))
         (path (org-element-property :path link))
         (raw-link (org-element-property :raw-link link))
         (href (cond
                ((string= type "custom-id") path)
                ((string= type "fuzzy")
                 (let ((beg (org-element-property :begin link)))
                   (signal 'osta-link-broken
                           `(,raw-link
                             "fuzzy links not supported"
                             ,(format "goto-char: %s" beg)))))
                ((string= type "file")
                 (or
                  ;; mapped links in `:osta-links' have priority
                  (cdr (assoc raw-link osta-links))
                  ;; for instance, when `:osta-root' is equal to "public",
                  ;; ./public/blog/page-1.md --> /blog/page-1.md
                  (and (or osta-root (signal 'osta-options '(":osta-root")))
                       (string-match (concat "\\`\\./" osta-root) path)
                       (replace-match "" nil nil path))
                  ;; for instance, when `:osta-assets' is equal to "assets",
                  ;; ./assets/images/image-1.png --> /images/image-1.png
                  (and (or osta-assets (signal 'osta-options '(":osta-assets")))
                       (string-match (concat "\\`\\./" osta-assets) path)
                       (replace-match "" nil nil path))
                  ;; any other file link raises an error
                  (let ((beg (org-element-property :begin link)))
                    (signal 'osta-link-broken
                            `(,raw-link ,(format "goto-char: %s" beg))))))

                (t raw-link))))
    (format "<a href=\"%s\">%s</a>" href (or (org-string-nw-p desc) href))))

;;; html templating

(defvar osta-html-raise-error-p nil
  "When `t', `osta-html' raises an error when we pass it a non component object.

For instance, a vector like `[a b c]' can't be a component passed to `osta-html'.
If `nil', which is the default value, `osta-html' process non component object
as the empty string.

For instance,

  (let ((osta-html-raise-error-p nil))
    (osta-html \"foo\" [a b c] \"bar\")) ; \"foobar\"

and,

  (let ((osta-html-raise-error-p t))
    (osta-html \"foo\" [a b c] \"bar\"))

raises the error:

  \"Object '[a b c]' of type 'vector' can't be a component in 'osta-html'\"")

(defun osta-parse-tag-kw (tag-kw)
  "Return a list of (\"tag\" \"id\" \"class\") from a TAG-KW.
If TAG-KW is not a valid tag keyword, return nil.

For instance, `osta-parse-tag-kw' behaves like this:
    :div                    -> (\"div\" nil nil)
    :div/id                 -> (\"div\" \"id\" nil)
    :div.class              -> (\"div\" nil \"class\")
    :div/id.class           -> (\"div\" \"id\" \"class\")
    :div/id.class-1.class-2 -> (\"div\" \"id\" \"class-1 class-2\")"
  (if-let* (((keywordp tag-kw))
            (tag-s (symbol-name tag-kw))
            ((string-match (concat "\\(?::\\)\\([^ /.]+\\)"
                                   "\\(?:/\\([^ /.]+\\)\\)?"
                                   "\\(?:[.]\\([^ /]+\\)\\)?")
                           tag-s)))
      (let* ((tag (match-string 1 tag-s))
             (id (match-string 2 tag-s))
             (class (match-string 3 tag-s))
             (classes (and class (string-replace "." " " class))))
        (if (or tag id classes)
            (list tag id classes)
          (error "Wrong tag keyword: %S" tag-kw)))
    (error "Wrong tag keyword: %S" tag-kw)))

(defun osta-format (tag-kw &optional attributes)
  "..."
  (let ((void-tags '("area" "base" "br" "col" "embed" "hr" "img" "input"   ; https://developer.mozilla.org/en-US/docs/Glossary/Empty_element
                     "keygen" "link" "meta" "param" "source" "track" "wbr")))
    (seq-let (tag id classes) (osta-parse-tag-kw tag-kw)
      (let* ((fmt (if (member tag void-tags) "<%s%s />" "<%s%s>%%s</%s>"))
             (kw->a (lambda (kw) (substring (symbol-name kw) 1))) ; :id -> "id"
             (p->a-v                                              ; (:id "foo") -> "id=\"foo\""
              (lambda (p)
                (let ((attr (funcall kw->a (car p))))
                  (pcase (eval (cadr p))
                    ('t (format "%s=\"%s\"" attr attr))
                    ('nil nil)
                    ((and _ value) (format "%s=\"%s\"" attr (osta-escape value)))))))
             (pairs (seq-partition attributes 2))
             ;; we merge classes from `tag-kw' and `attribute' and add it to the pairs
             (-pairs (if classes
                         (if-let* ((c (assoc :class pairs)))
                             (let* ((pairs-without-class
                                     (seq-remove
                                      (lambda (p) (eq (car p) :class)) pairs))
                                    (class-value-in-pairs (cadr c))
                                    (class `(:class ,(concat classes " " class-value-in-pairs))))
                               (cons class pairs-without-class))
                           (cons `(:class ,classes) pairs))
                       pairs))
             ;; `id' in `attributes' has priority over `id' in `tag-kw'
             (--pairs (if (and id (not (assoc :id -pairs)))
                          (cons `(:id ,id) -pairs)
                        -pairs))
             (attrs (string-join (delq nil (mapcar p->a-v --pairs)) " "))
             (-attrs (if (string-empty-p attrs) "" (concat " " attrs))))
        (format fmt tag -attrs tag)))))

(defun osta-component (component)
  (let ((comp (if (listp component) "" component))
        (comps (and (listp component) (list nil component)))
        (fmt "%s")
        rest)
    (while comp
      (pcase comp
        ;; string component or an integer component
        ((and (or (pred stringp) (pred numberp)))
         (let ((rest-slots (make-list (length rest) "%s"))
               (comp-str+%s (concat (format "%s" comp) "%s")))
           (setq fmt (apply #'format fmt comp-str+%s rest-slots))
           (setq comps (cdr comps))))
        ;; not a tag component but a list of components like '("foo" "bar")
        ((and (pred listp) l (guard (not (keywordp (car l)))))
         (setq comps (append comp (cdr comps))))
        ;; tag component like '(:p "foo") or '(:p/id.class (@ :attr "attr") "foo")
        ((pred listp)
         (let ((rest-slots (make-list (length rest) "%s"))
               (new-rest (cdr comps))
               tag-fmt tag-fmt+new-rest comp-children)
           (seq-let (tag-kw attr) comp
             (pcase attr
               ;; `attr' is attributes plist like '(@ :id "id" :class "class")
               ((and (pred listp) (pred (lambda (l) (equal (car l) '@))))
                (setq tag-fmt (osta-format tag-kw (cdr attr)))
                (setq comp-children (cddr comp)))
               (_ (setq tag-fmt (osta-format tag-kw))
                  (setq comp-children (cdr comp)))))
           ;; update value of `fmt', `comps', `comp', `rest' before looping
           (setq tag-fmt+new-rest (if new-rest (concat tag-fmt "%s") tag-fmt))
           (setq fmt (apply #'format fmt tag-fmt+new-rest rest-slots))
           (setq comps (append comp-children (and new-rest '(:rest))))
           (when new-rest (push new-rest rest))))
        ;; make the latest list of components added to `rest'
        ;; the part of the tree (`component') to be treated in
        ;; the next iteration
        (:rest
         (let ((rest-slots (make-list (length rest) "%s")))
           (setq fmt (apply #'format fmt "" rest-slots))
           (setq comps (pop rest))))
        ;; non component object
        ((and _ obj)
         (when osta-html-raise-error-p
           (error "Object '%S' of type '%s' can't be a component in 'osta-html'"
                  obj (type-of obj)))
         (setq comps (cdr comps))))
      (setq comp (car comps)))
    (format fmt "")))

(defun osta-html (&rest components)
  ""
  (mapconcat #'osta-component components ""))

;;; pages

(defun osta-page-p (element)
  "Return ELEMENT if ELEMENT is an `osta' page.

If ELEMENT isn't an `osta' page, return nil.

A root element (a `headline') is an `osta' page if:
1) it has the org property `OSTA_PAGE' set to `t' and
2) the org property `CUSTOM_ID' set.
The value of `CUSTOM_ID' of an `osta' page is the relative path of
the page from the root of the `osta' website.

An `osta' page with `CUSTOM_ID' set to `/' is the homepage of the
`osta' website.

Assuming that we locally serve our website at `http://localhost:3000',
the following org snippet defines an `osta' page which url is
`http://localhost:3000/2022-01-09/my-page/':

--------------------

* my super cool page
:PROPERTIES:
:OSTA_PAGE: t
:CUSTOM_ID: /2022-01-09/my-page/
:END:

This page contains a list:
- item 1
- item 2
- item 3

--------------------"
  (and (org-element-property :OSTA_PAGE element)
       (org-string-nw-p (org-element-property :CUSTOM_ID element))
       element))

(defun osta-page (element)
  "Return root element (a `headline') that is an `osta' page containing ELEMENT.

If ELEMENT doesn't belong to any page, return nil.

See `osta-page-p'."
  (pcase (org-element-type element)
    (`nil nil)
    (`org-data nil)
    (`headline (or (osta-page-p element)
                   (osta-page (org-element-property :parent element))))
    (_ (osta-page (org-element-property :parent element)))))

(defun osta-page-path (element)
  "Return path of `osta' page if ELEMENT is part of an `osta' page.

Return nil if not.
See `osta-page'."
  (org-element-property :CUSTOM_ID (osta-page element)))

;;; osta provide

(provide 'osta)
;;; org-bars.el ends here
