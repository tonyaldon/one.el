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

(defun osta-ox-subscript (_subscript contents _info) (format "<sub>%s</sub>" contents))
(defun osta-ox-superscript (_superscript contents _info) (format "<sup>%s</sup>" contents))

(ert-deftest osta-ox--subscript-and-superscript ()
  ;; osta-ox-subscript, osta-ox-superscript
  (should (string= (osta-ox-subscript nil "subscript" nil)
                   "<sub>subscript</sub>"))
  (should (string= (osta-ox-superscript nil "superscript" nil)
                   "<sup>superscript</sup>"))

  ;; by default ox.el exports with specific transcode functions
  ;; for the org elements `subscript' and `superscript'.
  ;; This is controlled by the variable `org-export-with-sub-superscripts'.
  ;; See `org-export-options-alist'.
  (let ((backend
         (org-export-create-backend
          :transcoders
          '((section . (lambda (_e c _i) c))
            (paragraph . (lambda (_e c _i) c))
            (subscript . osta-ox-subscript)
            (superscript . osta-ox-superscript)))))
    (let ((org-export-with-sub-superscripts t))
      (should
       (string=
        (org-test-with-temp-text "foo_bar"
          (org-export-as backend))
        "foo<sub>bar</sub>\n"))
      (should
       (string=
        (org-test-with-temp-text "foo^bar"
          (org-export-as backend))
        "foo<sup>bar</sup>\n")))
    (let ((org-export-with-sub-superscripts nil))
      (should
       (string=
        (org-test-with-temp-text "foo_bar"
          (org-export-as backend))
        "foo_bar\n"))
      (should
       (string=
        (org-test-with-temp-text "foo^bar"
          (org-export-as backend))
        "foo^bar\n")))))

(ert-deftest osta-ox-test ()
  ;; osta-ox-headline
  (let ((get-headline
         (lambda (rv tree)
           (car (org-element-map tree 'headline
                  (lambda (e)
                    (when (string= (org-element-property :raw-value e) rv)
                      e)))))))
    ;; headline level 1 with contents
    (should
     (string=
      (org-test-with-parsed-data "* headline 1"
        (osta-ox-headline (funcall get-headline "headline 1" tree)
                          "<div>contents<div>" info))
      "<div><h1>headline 1</h1><div>contents<div></div>"))
    ;; headline level 1 with contents nil
    (should
     (string=
      (org-test-with-parsed-data "* headline 1"
        (osta-ox-headline (funcall get-headline "headline 1" tree)
                          nil info))
      "<div><h1>headline 1</h1></div>"))
    ;; headline level 2 with contents
    (should
     (string=
      (org-test-with-parsed-data "* headline 1\n** headline 2"
        (osta-ox-headline (funcall get-headline "headline 2" tree)
                          "<div>contents<div>" info))
      "<div><h2>headline 2</h2><div>contents<div></div>"))
    ;; headline with CUSTOM_ID without id part
    (should
     (string=
      (org-test-with-parsed-data "* headline 1
:PROPERTIES:
:CUSTOM_ID: /path/to/page/
:END:"
        (osta-ox-headline (funcall get-headline "headline 1" tree)
                          "<div>contents<div>" info))
      "<div><h1>headline 1</h1><div>contents<div></div>"))
    ;; headline with CUSTOM_ID and id part
    (should
     (string=
      (org-test-with-parsed-data "* headline 1\n** headline 2
:PROPERTIES:
:CUSTOM_ID: /path/to/page/#id-test
:END:"
        (osta-ox-headline (funcall get-headline "headline 2" tree) nil info))
      "<div><h2 id=\"id-test\">headline 2</h2></div>")))

  ;; section, paragraph, plain-text, bold, italic, strike-through, underline
  (should (string= (osta-ox-section nil "section" nil) "<div>section</div>"))
  (should (string= (osta-ox-section nil nil nil) ""))
  (should (string= (osta-ox-paragraph nil "paragraph" nil) "<p>paragraph</p>"))
  (should (string= (osta-ox-plain-text "<...>...&" nil) "&lt;...&gt;...&amp;"))
  (should (string= (osta-ox-bold nil "bold" nil) "<b>bold</b>"))
  (should (string= (osta-ox-italic nil "italic" nil) "<i>italic</i>"))
  (should (string= (osta-ox-strike-through nil "strike-through" nil) "<del>strike-through</del>"))
  (should (string= (osta-ox-underline nil "underline" nil) "<u>underline</u>"))

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

(ert-deftest osta-ox-code-and-verbatim-test ()
  ;; code and verbatim nodes
  (let ((code (org-test-with-temp-text "before the ~inline code<point>~"
                (org-element-context)))
        (verbatim (org-test-with-temp-text "before the ~verbatim<point>~"
                    (org-element-context))))
    (should (string= (osta-ox-code code nil nil)
                     "<code class=\"osta-hl osta-hl-inline\">inline code</code>"))
    (should (string= (osta-ox-verbatim verbatim nil nil)
                     "<code class=\"osta-hl osta-hl-inline\">verbatim</code>"))))

(ert-deftest osta-ox-blocks-test ()
  ;; blocks

  ;; `osta-ox-is-results-p'
  (let ((src-block (org-test-with-temp-text "
#+BEGIN_SRC bash
echo \"Hello world!\"
#+END_SRC<point>"
                     (org-element-context)))
        (src-block-results (org-test-with-temp-text "
#+ATTR_OSTA_RESULTS:
#+BEGIN_SRC bash
echo \"Hello world!\"
#+END_SRC<point>"
                             (org-element-context)))
        (example-block (org-test-with-temp-text "
#+BEGIN_EXAMPLE
A simple example
#+END_EXAMPLE<point>"
                         (org-element-context)))
        (example-block-results-1 (org-test-with-temp-text "
#+RESULTS:
#+BEGIN_EXAMPLE
A simple example
#+END_EXAMPLE<point>"
                                   (org-element-context)))
        (example-block-results-2 (org-test-with-temp-text "
#+ATTR_OSTA_RESULTS:
#+BEGIN_EXAMPLE
A simple example
#+END_EXAMPLE<point>"
                                   (org-element-context)))
        (fixed-width (org-test-with-temp-text "
: I'm a multiline fixed width
: yes I am!<point>"
                       (org-element-context)))
        (fixed-width-results-1 (org-test-with-temp-text "
#+RESULTS:
: I'm a multiline fixed width
: yes I am!<point>"
                                 (org-element-context)))
        (fixed-width-results-2 (org-test-with-temp-text "
#+ATTR_OSTA_RESULTS:
: I'm a multiline fixed width
: yes I am!<point>"
                                 (org-element-context))))
    (should-not (osta-ox-is-results-p src-block))
    (should (osta-ox-is-results-p src-block-results))
    (should-not (osta-ox-is-results-p example-block))
    (should (osta-ox-is-results-p example-block-results-1))
    (should (osta-ox-is-results-p example-block-results-2))
    (should-not (osta-ox-is-results-p fixed-width))
    (should (osta-ox-is-results-p fixed-width-results-1))
    (should (osta-ox-is-results-p fixed-width-results-2)))

  ;; `osta-ox-htmlize'
  ;; note that in `sh-mode', `echo' word has the face `font-lock-builtin-face',
  ;; and strings have the faces `font-lock-string-face'.
  ;; normal blocks
  (should (string= (osta-ox-htmlize "echo \"Hello world!\"" "bash")
                   (concat "<pre><code class=\"osta-hl osta-hl-block\">"
                           "<span class=\"osta-hl-builtin\">echo</span> "
                           "<span class=\"osta-hl-string\">\"Hello world!\"</span>"
                           "</code></pre>")))
  ;; results blocks
  (should (string= (osta-ox-htmlize "echo \"Hello world!\"" "bash" t)
                   (concat "<pre><code class=\"osta-hl osta-hl-results\">"
                           "<span class=\"osta-hl-builtin\">echo</span> "
                           "<span class=\"osta-hl-string\">\"Hello world!\"</span>"
                           "</code></pre>")))

  ;; `osta-ox-src-block'
  (let ((src-block (org-test-with-temp-text "
#+BEGIN_SRC bash
echo \"Hello world!\"
#+END_SRC<point>"
                     (org-element-context)))
        (src-block-results (org-test-with-temp-text "
#+ATTR_OSTA_RESULTS:
#+BEGIN_SRC bash
echo \"Hello world!\"
#+END_SRC<point>"
                             (org-element-context))))
    (should (string= (osta-ox-src-block src-block nil nil )
                     (concat "<pre><code class=\"osta-hl osta-hl-block\">"
                             "<span class=\"osta-hl-builtin\">echo</span> "
                             "<span class=\"osta-hl-string\">\"Hello world!\"</span>"
                             "</code></pre>")))
    (should (string= (osta-ox-src-block src-block-results nil nil )
                     (concat "<pre><code class=\"osta-hl osta-hl-results\">"
                             "<span class=\"osta-hl-builtin\">echo</span> "
                             "<span class=\"osta-hl-string\">\"Hello world!\"</span>"
                             "</code></pre>"))))

  ;; `osta-ox-example-block'
  (let ((example-block (org-test-with-temp-text "
#+BEGIN_EXAMPLE
A simple example
#+END_EXAMPLE<point>"
                         (org-element-context)))
        (example-block-results-1 (org-test-with-temp-text "
#+RESULTS:
#+BEGIN_EXAMPLE
A simple example
#+END_EXAMPLE<point>"
                                   (org-element-context)))
        (example-block-results-2 (org-test-with-temp-text "
#+ATTR_OSTA_RESULTS:
#+BEGIN_EXAMPLE
A simple example
#+END_EXAMPLE<point>"
                                   (org-element-context))))
    (should (string= (osta-ox-example-block example-block nil nil)
                     (concat "<pre><code class=\"osta-hl osta-hl-block\">"
                             "A simple example"
                             "</code></pre>")))
    (should (string= (osta-ox-example-block example-block-results-1 nil nil)
                     (concat "<pre><code class=\"osta-hl osta-hl-results\">"
                             "A simple example"
                             "</code></pre>")))
    (should (string= (osta-ox-example-block example-block-results-2 nil nil)
                     (concat "<pre><code class=\"osta-hl osta-hl-results\">"
                             "A simple example"
                             "</code></pre>"))))
  ;; `osta-ox-fixed-width'
  (let ((fixed-width (org-test-with-temp-text "
: I'm a multiline fixed width
: yes I am!<point>"
                       (org-element-context)))
        (fixed-width-results-1 (org-test-with-temp-text "
#+RESULTS:
: I'm a multiline fixed width
: yes I am!<point>"
                                 (org-element-context)))
        (fixed-width-results-2 (org-test-with-temp-text "
#+ATTR_OSTA_RESULTS:
: I'm a multiline fixed width
: yes I am!<point>"
                                 (org-element-context))))
    (should (string= (osta-ox-fixed-width fixed-width nil nil)
                     (concat "<pre><code class=\"osta-hl osta-hl-block\">"
                             "I'm a multiline fixed width\nyes I am!"
                             "</code></pre>")))
    (should (string= (osta-ox-fixed-width fixed-width-results-1 nil nil)
                     (concat "<pre><code class=\"osta-hl osta-hl-results\">"
                             "I'm a multiline fixed width\nyes I am!"
                             "</code></pre>")))
    (should (string= (osta-ox-fixed-width fixed-width-results-2 nil nil)
                     (concat "<pre><code class=\"osta-hl osta-hl-results\">"
                             "I'm a multiline fixed width\nyes I am!"
                             "</code></pre>"))))

  ;; `osta-ox-quote-block'
  (should (string= (osta-ox-quote-block nil "I'm a quote. —Tony Aldon" nil)
                   "<blockquote class=\"osta-blockquote\">I'm a quote. —Tony Aldon</blockquote>")))

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
