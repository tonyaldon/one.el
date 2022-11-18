;;; require

(require 'ert)

;;; macro from org-mode repository

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
         ,@body))))

;;; utils

(ert-deftest one-escape-test ()
  (should (string= (one-escape "<") "&lt;"))
  (should (string= (one-escape ">") "&gt;"))
  (should (string= (one-escape "&") "&amp;"))
  (should (string= (one-escape "\"") "&quot;"))
  (should (string= (one-escape "'") "&apos;"))
  (should (string= (one-escape "regular text") "regular text"))
  (should (string= (one-escape "<...>...&...\"...'") "&lt;...&gt;...&amp;...&quot;...&apos;")))

;;; one-ox tests

;; (global-set-key (kbd "C-<f1>") (lambda () (interactive)(ert "one-ox-test")))

;;;; headline, section, paragraph, etc.

(ert-deftest one-ox-test ()
  ;; one-ox-headline
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
        (one-ox-headline (funcall get-headline "headline 1" tree)
                          "<div>contents<div>" info))
      "<div><h1>headline 1</h1><div>contents<div></div>"))
    ;; headline level 1 with contents nil
    (should
     (string=
      (org-test-with-parsed-data "* headline 1"
        (one-ox-headline (funcall get-headline "headline 1" tree)
                          nil info))
      "<div><h1>headline 1</h1></div>"))
    ;; headline level 2 with contents
    (should
     (string=
      (org-test-with-parsed-data "* headline 1\n** headline 2"
        (one-ox-headline (funcall get-headline "headline 2" tree)
                          "<div>contents<div>" info))
      "<div><h2>headline 2</h2><div>contents<div></div>"))
    ;; headline with CUSTOM_ID without id part
    (should
     (string=
      (org-test-with-parsed-data "* headline 1
:PROPERTIES:
:CUSTOM_ID: /path/to/page/
:END:"
        (one-ox-headline (funcall get-headline "headline 1" tree)
                          "<div>contents<div>" info))
      "<div><h1>headline 1</h1><div>contents<div></div>"))
    ;; headline with CUSTOM_ID and id part
    (should
     (string=
      (org-test-with-parsed-data "* headline 1\n** headline 2
:PROPERTIES:
:CUSTOM_ID: /path/to/page/#id-test
:END:"
        (one-ox-headline (funcall get-headline "headline 2" tree) nil info))
      "<div><h2 id=\"id-test\">headline 2</h2></div>")))

  ;; section, paragraph, plain-text, bold, italic, strike-through, underline
  (should (string= (one-ox-section nil "section" nil) "<div>section</div>"))
  (should (string= (one-ox-section nil nil nil) ""))
  (should (string= (one-ox-paragraph nil "paragraph" nil) "<p>paragraph</p>"))
  (should (string= (one-ox-plain-text "<...>...&" nil) "&lt;...&gt;...&amp;"))
  (should (string= (one-ox-bold nil "bold" nil) "<b>bold</b>"))
  (should (string= (one-ox-italic nil "italic" nil) "<i>italic</i>"))
  (should (string= (one-ox-strike-through nil "strike-through" nil) "<del>strike-through</del>"))
  (should (string= (one-ox-underline nil "underline" nil) "<u>underline</u>"))

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
    (should (string= (one-ox-plain-list ordered-list "contents" nil) "<ol>contents</ol>"))
    (should (string= (one-ox-plain-list unordered-list "contents" nil) "<ul>contents</ul>"))
    (should-error (one-ox-plain-list other-list "contents" nil)))
  (should (string= (one-ox-item nil "item" nil) "<li>item</li>")))

(ert-deftest one-ox-code-and-verbatim-test ()
  ;; code and verbatim nodes
  (let ((code (org-test-with-temp-text "before the ~inline code<point>~"
                (org-element-context)))
        (verbatim (org-test-with-temp-text "before the ~verbatim<point>~"
                    (org-element-context))))
    (should (string= (one-ox-code code nil nil)
                     "<code class=\"one-hl one-hl-inline\">inline code</code>"))
    (should (string= (one-ox-verbatim verbatim nil nil)
                     "<code class=\"one-hl one-hl-inline\">verbatim</code>"))))

(ert-deftest one-ox--subscript-and-superscript ()
  ;; one-ox-subscript, one-ox-superscript
  (should (string= (one-ox-subscript nil "subscript" nil)
                   "<sub>subscript</sub>"))
  (should (string= (one-ox-superscript nil "superscript" nil)
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
            (subscript . one-ox-subscript)
            (superscript . one-ox-superscript)))))
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

;;;; blocks

(ert-deftest one-ox-blocks-test ()
  ;; `one-ox-is-results-p'
  (let ((src-block (org-test-with-temp-text "
#+BEGIN_SRC bash
echo \"Hello world!\"
#+END_SRC<point>"
                     (org-element-context)))
        (src-block-results (org-test-with-temp-text "
#+ATTR_ONE_RESULTS:
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
#+ATTR_ONE_RESULTS:
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
#+ATTR_ONE_RESULTS:
: I'm a multiline fixed width
: yes I am!<point>"
                                 (org-element-context))))
    (should-not (one-ox-is-results-p src-block))
    (should (one-ox-is-results-p src-block-results))
    (should-not (one-ox-is-results-p example-block))
    (should (one-ox-is-results-p example-block-results-1))
    (should (one-ox-is-results-p example-block-results-2))
    (should-not (one-ox-is-results-p fixed-width))
    (should (one-ox-is-results-p fixed-width-results-1))
    (should (one-ox-is-results-p fixed-width-results-2)))

  ;; `one-ox-htmlize'
  ;; note that in `sh-mode', `echo' word has the face `font-lock-builtin-face',
  ;; and strings have the faces `font-lock-string-face'.
  ;; normal blocks
  (should (string= (one-ox-htmlize "echo \"Hello world!\"" "bash")
                   (concat "<pre><code class=\"one-hl one-hl-block\">"
                           "<span class=\"one-hl-builtin\">echo</span> "
                           "<span class=\"one-hl-string\">\"Hello world!\"</span>"
                           "</code></pre>")))
  ;; results blocks
  (should (string= (one-ox-htmlize "echo \"Hello world!\"" "bash" t)
                   (concat "<pre><code class=\"one-hl one-hl-results\">"
                           "<span class=\"one-hl-builtin\">echo</span> "
                           "<span class=\"one-hl-string\">\"Hello world!\"</span>"
                           "</code></pre>")))

  ;; `one-ox-src-block'
  (let ((src-block (org-test-with-temp-text "
#+BEGIN_SRC bash
echo \"Hello world!\"
#+END_SRC<point>"
                     (org-element-context)))
        (src-block-results (org-test-with-temp-text "
#+ATTR_ONE_RESULTS:
#+BEGIN_SRC bash
echo \"Hello world!\"
#+END_SRC<point>"
                             (org-element-context))))
    (should (string= (one-ox-src-block src-block nil nil )
                     (concat "<pre><code class=\"one-hl one-hl-block\">"
                             "<span class=\"one-hl-builtin\">echo</span> "
                             "<span class=\"one-hl-string\">\"Hello world!\"</span>"
                             "</code></pre>")))
    (should (string= (one-ox-src-block src-block-results nil nil )
                     (concat "<pre><code class=\"one-hl one-hl-results\">"
                             "<span class=\"one-hl-builtin\">echo</span> "
                             "<span class=\"one-hl-string\">\"Hello world!\"</span>"
                             "</code></pre>"))))

  ;; `one-ox-example-block'
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
#+ATTR_ONE_RESULTS:
#+BEGIN_EXAMPLE
A simple example
#+END_EXAMPLE<point>"
                                   (org-element-context))))
    (should (string= (one-ox-example-block example-block nil nil)
                     (concat "<pre><code class=\"one-hl one-hl-block\">"
                             "A simple example"
                             "</code></pre>")))
    (should (string= (one-ox-example-block example-block-results-1 nil nil)
                     (concat "<pre><code class=\"one-hl one-hl-results\">"
                             "A simple example"
                             "</code></pre>")))
    (should (string= (one-ox-example-block example-block-results-2 nil nil)
                     (concat "<pre><code class=\"one-hl one-hl-results\">"
                             "A simple example"
                             "</code></pre>"))))
  ;; `one-ox-fixed-width'
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
#+ATTR_ONE_RESULTS:
: I'm a multiline fixed width
: yes I am!<point>"
                                 (org-element-context))))
    (should (string= (one-ox-fixed-width fixed-width nil nil)
                     (concat "<pre><code class=\"one-hl one-hl-block\">"
                             "I'm a multiline fixed width\nyes I am!"
                             "</code></pre>")))
    (should (string= (one-ox-fixed-width fixed-width-results-1 nil nil)
                     (concat "<pre><code class=\"one-hl one-hl-results\">"
                             "I'm a multiline fixed width\nyes I am!"
                             "</code></pre>")))
    (should (string= (one-ox-fixed-width fixed-width-results-2 nil nil)
                     (concat "<pre><code class=\"one-hl one-hl-results\">"
                             "I'm a multiline fixed width\nyes I am!"
                             "</code></pre>"))))

  ;; `one-ox-quote-block'
  (should (string= (one-ox-quote-block nil "I'm a quote. —Tony Aldon" nil)
                   "<blockquote class=\"one-blockquote\">I'm a quote. —Tony Aldon</blockquote>")))

;;;; links

(ert-deftest one-map-links-test ()
  (should
   (equal
    (org-test-with-temp-text "#+LINK: abbrev-link /path/to/project/
#+ONE_LINK: abbrev-link:file-1.clj::(defn func-1 --> https://github.com/user/project/blob/master/file-1.clj#L12
#+ONE_LINK: abbrev-link:file-2.clj::(defn func-2 --> https://github.com/user/project/blob/master/file-2.clj#L56"
      (org-set-regexps-and-options)
      (one-map-links))
    '(("/path/to/project/file-1.clj::(defn func-1" . "https://github.com/user/project/blob/master/file-1.clj#L12")
      ("/path/to/project/file-2.clj::(defn func-2" . "https://github.com/user/project/blob/master/file-2.clj#L56"))))
  (should
   (equal
    (org-test-with-temp-text "#+LINK: abbrev-link /path/to/project/
#+ONE_LINK: abbrev-link:                           --> foo
#+ONE_LINK: /path/to/project/                      --> foo
#+ONE_LINK: abbrev-link:file.clj::(defn func-1      --> bar
#+ONE_LINK: /path/to/project/file.clj::(defn func-1 --> bar
#+ONE_LINK: not a valid ONE_LINK declaration"
      (org-set-regexps-and-options)
      (one-map-links))
    '(("/path/to/project/" . "foo")
      ("/path/to/project/" . "foo")
      ("/path/to/project/file.clj::(defn func-1" . "bar")
      ("/path/to/project/file.clj::(defn func-1" . "bar")))))

(ert-deftest one-ox-link--custom-id-https-mailto-test ()
  "link type: custom-id, https, mailto"
  (let ((backend
         (org-export-create-backend
          :transcoders
          '((section . (lambda (_e c _i) c))
            (paragraph . (lambda (_e c _i) c))
            (link . one-ox-link)))))
    (should
     (string=
      (org-test-with-temp-text "[[#foo][bar]]"
        (org-export-as backend))
      "<a href=\"foo\">bar</a>\n"))
    (should
     (string=
      (org-test-with-temp-text "[[#foo]]"
        (org-export-as backend))
      "<a href=\"foo\">foo</a>\n"))
    (should
     (string=
      (org-test-with-temp-text "https://tonyaldon.com"
        (org-export-as backend))
      "<a href=\"https://tonyaldon.com\">https://tonyaldon.com</a>\n"))
    (should
     (string=
      (org-test-with-temp-text "[[https://tonyaldon.com][Tony Aldon]]"
        (org-export-as backend))
      "<a href=\"https://tonyaldon.com\">Tony Aldon</a>\n"))
    (should
     (string=
      (org-test-with-temp-text "mailto:aldon.tony.adm@gmail.com"
        (org-export-as backend))
      "<a href=\"mailto:aldon.tony.adm@gmail.com\">mailto:aldon.tony.adm@gmail.com</a>\n"))
    (should
     (string=
      (org-test-with-temp-text "[[mailto:aldon.tony.adm@gmail.com][my email]]"
        (org-export-as backend))
      "<a href=\"mailto:aldon.tony.adm@gmail.com\">my email</a>\n"))))

(ert-deftest one-ox-link--fuzzy-test ()
  "link type: fuzzy"
  (let ((backend
         (org-export-create-backend
          :transcoders
          '((section . (lambda (_e c _i) c))
            (paragraph . (lambda (_e c _i) c))
            (link . one-ox-link)))))
    (should-error
     (org-test-with-temp-text "[[fuzzy search]]"
       (org-export-as backend)))
    (should-error
     (org-test-with-temp-text "[[*fuzzy search]]"
       (org-export-as backend)))))

(ert-deftest one-ox-link--file-mapped-links-test ()
  "link type: file (`:one-links')"
  (let ((backend
         (org-export-create-backend
          :transcoders
          '((section . (lambda (_e c _i) c))
            (paragraph . (lambda (_e c _i) c))
            (link . one-ox-link))
          :options
          '((:one-root nil nil "public")
            (:one-assets nil nil "assets")
            (:one-links nil nil
             '(("/tmp/clojurescript/" . "https://github.com/clojure/clojurescript")
               ("./clojure/src/clj/clojure/core.clj::(defn str" .
                "https://github.com/clojure/clojure/blob/abe19832c0294fec4c9c55430c9262c4b6d2f8b1/src/clj/clojure/core.clj#L546")))))))
    ;; link to directory mapped to github repository in `:one-links'
    (should
     (string=
      (org-test-with-temp-text "[[/tmp/clojurescript/][ClojureScript]]"
        (org-export-as backend))
      "<a href=\"https://github.com/clojure/clojurescript\">ClojureScript</a>\n"))
    ;; link to the function `str' in the file `core.clj'
    ;; mapped to the id `L546' on a specific webpages on github
    (should
     (string=
      (org-test-with-temp-text "[[./clojure/src/clj/clojure/core.clj::(defn str][clojure.core/str]]"
        (org-export-as backend))
      (concat "<a href=\"https://github.com/clojure/clojure/blob/abe19832c0294fec4c9c55430c9262c4b6d2f8b1/src/clj/clojure/core.clj#L546\">"
              "clojure.core/str"
              "</a>\n")))
    ;; link to a file that:
    ;;   1) has no mapping in `:one-links',
    ;;   2) is not in the directory `:one-root',
    ;;   3) is not in the directory `:one-assets',
    ;; should return an error.
    (should-error
     (org-test-with-temp-text "[[/path/to/example.txt]]"
       (org-export-as backend))))

  ;; `:one-links' set using org keywords and the function `one-map-links'
  (let ((backend
         (org-export-create-backend
          :transcoders
          '((section . (lambda (_e c _i) c))
            (paragraph . (lambda (_e c _i) c))
            (link . one-ox-link))
          :options
          '((:one-root nil nil "public")
            (:one-assets nil nil "assets")))))
    (should
     (string=
      (org-test-with-temp-text "#+LINK: clj ./clojure/
#+ONE_LINK: clj:src/clj/clojure/core.clj::(defn str --> https://github.com/clojure/clojure/blob/abe19832c0294fec4c9c55430c9262c4b6d2f8b1/src/clj/clojure/core.clj#L546

[[clj:src/clj/clojure/core.clj::(defn str][clojure.core/str]]"
        (org-set-regexps-and-options)
        (org-export-as backend nil nil nil
                       `(:one-links ,(one-map-links))))
      (concat "<a href=\"https://github.com/clojure/clojure/blob/abe19832c0294fec4c9c55430c9262c4b6d2f8b1/src/clj/clojure/core.clj#L546\">"
              "clojure.core/str"
              "</a>\n")))))

(ert-deftest one-ox-link--file-root-and-assets-test ()
  "link type: file (`:one-root', `:one-assets')"

  ;; `:one-root'
  (let ((backend
         (org-export-create-backend
          :transcoders
          '((section . (lambda (_e c _i) c))
            (paragraph . (lambda (_e c _i) c))
            (link . one-ox-link))
          :options
          '((:one-assets "ONE_ASSETS" nil "assets")))))
    (should
     (string=
      (org-test-with-temp-text "[[./public/blog/page-1.md][Page 1 in markdown]]"
        (org-export-as backend nil nil nil '(:one-root "public")))
      "<a href=\"/blog/page-1.md\">Page 1 in markdown</a>\n"))
    (should-error
     (org-test-with-temp-text "[[./build/blog/page-1.md][Page 1 in markdown]]"
       (org-export-as backend nil nil nil '(:one-root "public"))))
    (should
     (string=
      (org-test-with-temp-text "[[./build/blog/page-1.md][Page 1 in markdown]]"
        (org-export-as backend nil nil nil '(:one-root "build")))
      "<a href=\"/blog/page-1.md\">Page 1 in markdown</a>\n"))
    ;; :one-root not defined
    (should-error
     (org-test-with-temp-text "[[./public/blog/page-1.md][Page 1 in markdown]]"
       (org-export-as backend))))

  ;; `:one-assets'
  (let ((backend
         (org-export-create-backend
          :transcoders
          '((section . (lambda (_e c _i) c))
            (paragraph . (lambda (_e c _i) c))
            (link . one-ox-link))
          :options
          '((:one-root "ONE_ROOT" nil "public")))))
    (should
     (string=
      (org-test-with-temp-text "[[./assets/images/one.png][one image]]"
        (org-export-as backend nil nil nil '(:one-assets "assets")))
      "<a href=\"/images/one.png\">one image</a>\n"))
    (should
     (string=
      (org-test-with-temp-text "[[./resources/images/one.png][one image]]"
        (org-export-as backend nil nil nil '(:one-assets "resources")))
      "<a href=\"/images/one.png\">one image</a>\n"))
    (should-error
     (org-test-with-temp-text "[[./resources/images/one.png][one image]]"
       (org-export-as backend nil nil nil '(:one-assets "assets"))))
    ;; :one-assets not defined
    (should-error
     (org-test-with-temp-text "[[./assets/images/one.png][one image]]"
       (org-export-as backend))))

  ;; `:one-root' and `:one-assets' set via org keyword
  (let ((backend
         (org-export-create-backend
          :transcoders
          '((section . (lambda (_e c _i) c))
            (paragraph . (lambda (_e c _i) c))
            (link . one-ox-link))
          :options
          '((:one-root "ONE_ROOT" nil "public")
            (:one-assets "ONE_ASSETS" nil "assets")))))
    (should
     (string=
      (org-test-with-temp-text "#+ONE_ROOT: build
    [[./build/blog/page-1.md][Page 1 in markdown]]"
        (org-set-regexps-and-options)
        (org-export-as backend))
      "<a href=\"/blog/page-1.md\">Page 1 in markdown</a>\n"))
    (should
     (string=
      (org-test-with-temp-text "#+ONE_ASSETS: resources
[[./resources/images/one.png][one image]]"
        (org-set-regexps-and-options)
        (org-export-as backend))
      "<a href=\"/images/one.png\">one image</a>\n"))))


;;; pages

(ert-deftest one-page-p-test ()
  (should-not
   (org-test-with-temp-text "<point>* page 1
:PROPERTIES:
:CUSTOM_ID: /date-1/page-1/
:END:"
     (one-page-p (org-element-context))))
  (should-not
   (org-test-with-temp-text "<point>* page 1
:PROPERTIES:
:ONE_PAGE: t
:END:"
     (one-page-p (org-element-context))))
  (should-not
   (org-test-with-parsed-data "* page 1
:PROPERTIES:
:ONE_PAGE: t
:CUSTOM_ID: /date-1/page-1/
:END:
** headline 1
** headline 2

some text
"
                              (let ((headline-1
                                     (car (org-element-map tree 'headline
                                            (lambda (e)
                                              (when (string= (org-element-property :raw-value e) "headline 1")
                                                e))))))
                                (one-page-p headline-1))))
  (should
   (equal
    (org-test-with-temp-text "<point>* page 1
:PROPERTIES:
:ONE_PAGE: t
:CUSTOM_ID: /date-1/page-1/
:END:"
      (let ((page (one-page-p (org-element-context))))
        `(:raw-value ,(org-element-property :raw-value page)
          :ONE_PAGE ,(org-element-property :ONE_PAGE page)
          :CUSTOM_ID ,(org-element-property :CUSTOM_ID page))))
    '(:raw-value "page 1"
      :ONE_PAGE "t"
      :CUSTOM_ID "/date-1/page-1/"))))

(ert-deftest one-page-test ()
  (should-not
   (org-test-with-parsed-data "* page 1
:PROPERTIES:
:CUSTOM_ID: /date-1/page-1/
:END:
  "
                              (let ((headline (car (org-element-map tree 'headline #'identity))))
                                (one-page headline))))
  (should-not
   (org-test-with-parsed-data "* page 1
:PROPERTIES:
:ONE_PAGE: t
:END:
  "
                              (let ((headline (car (org-element-map tree 'headline #'identity))))
                                (one-page headline))))
  (should
   (equal
    (org-test-with-parsed-data "* page 1
:PROPERTIES:
:ONE_PAGE: t
:CUSTOM_ID: /date-1/page-1/
:END:
** headline 1
** headline 2

some text
  "
                               (let* ((get-headline
                                       (lambda (rv)
                                         (car (org-element-map tree 'headline
                                                (lambda (e)
                                                  (when (string= (org-element-property :raw-value e) rv)
                                                    e))))))
                                      (headline-page-1 (funcall get-headline "page 1"))
                                      (page-headline-page-1 (one-page headline-page-1))
                                      (headline-2 (funcall get-headline "headline 2"))
                                      (page-headline-2 (one-page headline-2))
                                      (paragraph (car (org-element-map tree 'paragraph #'identity)))
                                      (page-paragraph (one-page paragraph))
                                      (content-paragraph (substring-no-properties (nth 2 paragraph))))
                                 `(:headline-page-1
                                   (:raw-value ,(org-element-property :raw-value headline-page-1)
                                    :ONE_PAGE ,(org-element-property :ONE_PAGE page-headline-page-1)
                                    :CUSTOM_ID ,(org-element-property :CUSTOM_ID page-headline-page-1))
                                   :headline-2
                                   (:raw-value ,(org-element-property :raw-value headline-2)
                                    :ONE_PAGE ,(org-element-property :ONE_PAGE page-headline-2)
                                    :CUSTOM_ID ,(org-element-property :CUSTOM_ID page-headline-2))
                                   :paragraph
                                   (:content ,content-paragraph
                                    :ONE_PAGE ,(org-element-property :ONE_PAGE page-paragraph)
                                    :CUSTOM_ID ,(org-element-property :CUSTOM_ID page-paragraph)))))
    '(:headline-page-1
      (:raw-value "page 1"
       :ONE_PAGE "t"
       :CUSTOM_ID "/date-1/page-1/")
      :headline-2
      (:raw-value "headline 2"
       :ONE_PAGE "t"
       :CUSTOM_ID "/date-1/page-1/")
      :paragraph
      (:content "some text\n"
       :ONE_PAGE "t"
       :CUSTOM_ID "/date-1/page-1/")))))

(ert-deftest one-page-path-test ()
  (should
   (string=
    (org-test-with-temp-text "<point>* page 1
:PROPERTIES:
:ONE_PAGE: t
:CUSTOM_ID: /date-1/page-1/
:END:"
      (one-page-path (org-element-context)))
    "/date-1/page-1/"))
  (should-not
   (org-test-with-temp-text "<point>* page 1
:PROPERTIES:
:CUSTOM_ID: /date-1/page-1/
:END:"
     (one-page-path (org-element-context))))
  (should-not
   (org-test-with-temp-text "<point>* page 1
:PROPERTIES:
:ONE_PAGE: t
:END:"
     (one-page-path (org-element-context)))))
