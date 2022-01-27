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

(ert-deftest osta-escape-test ()
  (should (string= (osta-escape "<") "&lt;"))
  (should (string= (osta-escape ">") "&gt;"))
  (should (string= (osta-escape "&") "&amp;"))
  (should (string= (osta-escape "\"") "&quot;"))
  (should (string= (osta-escape "'") "&apos;"))
  (should (string= (osta-escape "regular text") "regular text"))
  (should (string= (osta-escape "<...>...&...\"...'") "&lt;...&gt;...&amp;...&quot;...&apos;")))

;;; osta-ox tests

;; (global-set-key (kbd "C-<f1>") (lambda () (interactive)(ert "osta-ox-test")))

;;;; headline, section, paragraph, etc.

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

;;;; blocks

(ert-deftest osta-ox-blocks-test ()
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

;;;; links

(ert-deftest osta-map-links-test ()
  (should
   (equal
    (org-test-with-temp-text "#+LINK: abbrev-link /path/to/project/
#+OSTA_LINK: abbrev-link:file-1.clj::(defn func-1 --> https://github.com/user/project/blob/master/file-1.clj#L12
#+OSTA_LINK: abbrev-link:file-2.clj::(defn func-2 --> https://github.com/user/project/blob/master/file-2.clj#L56"
      (org-set-regexps-and-options)
      (osta-map-links))
    '(("/path/to/project/file-1.clj::(defn func-1" . "https://github.com/user/project/blob/master/file-1.clj#L12")
      ("/path/to/project/file-2.clj::(defn func-2" . "https://github.com/user/project/blob/master/file-2.clj#L56"))))
  (should
   (equal
    (org-test-with-temp-text "#+LINK: abbrev-link /path/to/project/
#+OSTA_LINK: abbrev-link:                           --> foo
#+OSTA_LINK: /path/to/project/                      --> foo
#+OSTA_LINK: abbrev-link:file.clj::(defn func-1      --> bar
#+OSTA_LINK: /path/to/project/file.clj::(defn func-1 --> bar
#+OSTA_LINK: not a valid OSTA_LINK declaration"
      (org-set-regexps-and-options)
      (osta-map-links))
    '(("/path/to/project/" . "foo")
      ("/path/to/project/" . "foo")
      ("/path/to/project/file.clj::(defn func-1" . "bar")
      ("/path/to/project/file.clj::(defn func-1" . "bar")))))

(ert-deftest osta-ox-link--custom-id-https-mailto-test ()
  "link type: custom-id, https, mailto"
  (let ((backend
         (org-export-create-backend
          :transcoders
          '((section . (lambda (_e c _i) c))
            (paragraph . (lambda (_e c _i) c))
            (link . osta-ox-link)))))
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

(ert-deftest osta-ox-link--fuzzy-test ()
  "link type: fuzzy"
  (let ((backend
         (org-export-create-backend
          :transcoders
          '((section . (lambda (_e c _i) c))
            (paragraph . (lambda (_e c _i) c))
            (link . osta-ox-link)))))
    (should-error
     (org-test-with-temp-text "[[fuzzy search]]"
       (org-export-as backend)))
    (should-error
     (org-test-with-temp-text "[[*fuzzy search]]"
       (org-export-as backend)))))

(ert-deftest osta-ox-link--file-mapped-links-test ()
  "link type: file (`:osta-links')"
  (let ((backend
         (org-export-create-backend
          :transcoders
          '((section . (lambda (_e c _i) c))
            (paragraph . (lambda (_e c _i) c))
            (link . osta-ox-link))
          :options
          '((:osta-root nil nil "public")
            (:osta-assets nil nil "assets")
            (:osta-links nil nil
             '(("/tmp/clojurescript/" . "https://github.com/clojure/clojurescript")
               ("./clojure/src/clj/clojure/core.clj::(defn str" .
                "https://github.com/clojure/clojure/blob/abe19832c0294fec4c9c55430c9262c4b6d2f8b1/src/clj/clojure/core.clj#L546")))))))
    ;; link to directory mapped to github repository in `:osta-links'
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
    ;;   1) has no mapping in `:osta-links',
    ;;   2) is not in the directory `:osta-root',
    ;;   3) is not in the directory `:osta-assets',
    ;; should return an error.
    (should-error
     (org-test-with-temp-text "[[/path/to/example.txt]]"
       (org-export-as backend))))

  ;; `:osta-links' set using org keywords and the function `osta-map-links'
  (let ((backend
         (org-export-create-backend
          :transcoders
          '((section . (lambda (_e c _i) c))
            (paragraph . (lambda (_e c _i) c))
            (link . osta-ox-link))
          :options
          '((:osta-root nil nil "public")
            (:osta-assets nil nil "assets")))))
    (should
     (string=
      (org-test-with-temp-text "#+LINK: clj ./clojure/
#+OSTA_LINK: clj:src/clj/clojure/core.clj::(defn str --> https://github.com/clojure/clojure/blob/abe19832c0294fec4c9c55430c9262c4b6d2f8b1/src/clj/clojure/core.clj#L546

[[clj:src/clj/clojure/core.clj::(defn str][clojure.core/str]]"
        (org-set-regexps-and-options)
        (org-export-as backend nil nil nil
                       `(:osta-links ,(osta-map-links))))
      (concat "<a href=\"https://github.com/clojure/clojure/blob/abe19832c0294fec4c9c55430c9262c4b6d2f8b1/src/clj/clojure/core.clj#L546\">"
              "clojure.core/str"
              "</a>\n")))))

(ert-deftest osta-ox-link--file-root-and-assets-test ()
  "link type: file (`:osta-root', `:osta-assets')"

  ;; `:osta-root'
  (let ((backend
         (org-export-create-backend
          :transcoders
          '((section . (lambda (_e c _i) c))
            (paragraph . (lambda (_e c _i) c))
            (link . osta-ox-link))
          :options
          '((:osta-assets "OSTA_ASSETS" nil "assets")))))
    (should
     (string=
      (org-test-with-temp-text "[[./public/blog/page-1.md][Page 1 in markdown]]"
        (org-export-as backend nil nil nil '(:osta-root "public")))
      "<a href=\"/blog/page-1.md\">Page 1 in markdown</a>\n"))
    (should-error
     (org-test-with-temp-text "[[./build/blog/page-1.md][Page 1 in markdown]]"
       (org-export-as backend nil nil nil '(:osta-root "public"))))
    (should
     (string=
      (org-test-with-temp-text "[[./build/blog/page-1.md][Page 1 in markdown]]"
        (org-export-as backend nil nil nil '(:osta-root "build")))
      "<a href=\"/blog/page-1.md\">Page 1 in markdown</a>\n"))
    ;; :osta-root not defined
    (should-error
     (org-test-with-temp-text "[[./public/blog/page-1.md][Page 1 in markdown]]"
       (org-export-as backend))))

  ;; `:osta-assets'
  (let ((backend
         (org-export-create-backend
          :transcoders
          '((section . (lambda (_e c _i) c))
            (paragraph . (lambda (_e c _i) c))
            (link . osta-ox-link))
          :options
          '((:osta-root "OSTA_ROOT" nil "public")))))
    (should
     (string=
      (org-test-with-temp-text "[[./assets/images/osta.png][osta image]]"
        (org-export-as backend nil nil nil '(:osta-assets "assets")))
      "<a href=\"/images/osta.png\">osta image</a>\n"))
    (should
     (string=
      (org-test-with-temp-text "[[./resources/images/osta.png][osta image]]"
        (org-export-as backend nil nil nil '(:osta-assets "resources")))
      "<a href=\"/images/osta.png\">osta image</a>\n"))
    (should-error
     (org-test-with-temp-text "[[./resources/images/osta.png][osta image]]"
       (org-export-as backend nil nil nil '(:osta-assets "assets"))))
    ;; :osta-assets not defined
    (should-error
     (org-test-with-temp-text "[[./assets/images/osta.png][osta image]]"
       (org-export-as backend))))

  ;; `:osta-root' and `:osta-assets' set via org keyword
  (let ((backend
         (org-export-create-backend
          :transcoders
          '((section . (lambda (_e c _i) c))
            (paragraph . (lambda (_e c _i) c))
            (link . osta-ox-link))
          :options
          '((:osta-root "OSTA_ROOT" nil "public")
            (:osta-assets "OSTA_ASSETS" nil "assets")))))
    (should
     (string=
      (org-test-with-temp-text "#+OSTA_ROOT: build
    [[./build/blog/page-1.md][Page 1 in markdown]]"
        (org-set-regexps-and-options)
        (org-export-as backend))
      "<a href=\"/blog/page-1.md\">Page 1 in markdown</a>\n"))
    (should
     (string=
      (org-test-with-temp-text "#+OSTA_ASSETS: resources
[[./resources/images/osta.png][osta image]]"
        (org-set-regexps-and-options)
        (org-export-as backend))
      "<a href=\"/images/osta.png\">osta image</a>\n"))))

;;; html templating

(ert-deftest osta-parse-tag-kw-test ()
  (should-error (osta-parse-tag-kw "string-is-not-a-valid-tag-keyword"))
  (should-error (osta-parse-tag-kw 'symbol-is-not-a-valid-tag-keyword))
  (should (equal (osta-parse-tag-kw :div) '("div" nil nil)))
  (should (equal (osta-parse-tag-kw :div/id) '("div" "id" nil)))
  (should (equal (osta-parse-tag-kw :div.class) '("div" nil "class")))
  (should (equal (osta-parse-tag-kw :div/id.class) '("div" "id" "class")))
  (should (equal (osta-parse-tag-kw :div/id.class-1.class-2) '("div" "id" "class-1 class-2"))))

(ert-deftest osta-format-test ()

  ;; id and classes in the tag-kw
  (should (string= (osta-format :div) "<div>%s</div>"))
  (should (string= (osta-format :div/id)
                   "<div id=\"id\">%s</div>"))
  (should (string= (osta-format :div.class)
                   "<div class=\"class\">%s</div>"))
  (should (string= (osta-format :div/id.class)
                   "<div id=\"id\" class=\"class\">%s</div>"))
  (should (string= (osta-format :div/id.class-1.class-2)
                   "<div id=\"id\" class=\"class-1 class-2\">%s</div>"))

  ;; void tags
  (should (string= (osta-format :hr) "<hr />%s"))

  ;; tag-kw must be keywords
  (should-error (osta-format 'div))
  (should-error (osta-format "div"))
  (should-error (osta-format 'div '(:id "id")))
  (should-error (osta-format "div" '(:id "id")))

  ;; attributes plist
  (should (string= (osta-format :div '(:id "id")) "<div id=\"id\">%s</div>"))
  (should (string= (osta-format :div '(:id "id" :class "class"))
                   "<div id=\"id\" class=\"class\">%s</div>"))

  ;; values in key/value pairs of attributes plist are evaluated
  (should (string= (osta-format :div '(:id (concat "id-" "123"))) "<div id=\"id-123\">%s</div>"))

  ;; attribute values are escaped
  (should (string= (osta-format :div '(:id "\"")) "<div id=\"&quot;\">%s</div>"))

  ;; `id' in `attributes' has priority over `id' in `tag-kw'
  (should (string= (osta-format :div/id-in-tag '(:id "id-in-plist"))
                   "<div id=\"id-in-plist\">%s</div>"))

  ;; classes in `tag-kw' and `attributes' plist
  (should (string= (osta-format :div.class-in-tag '(:class "class-a class-b"))
                   "<div class=\"class-in-tag class-a class-b\">%s</div>"))

  ;; boolean attributes
  (should (string= (osta-format :input '(:type "checkbox" :checked t))
                   "<input type=\"checkbox\" checked=\"checked\" />%s"))
  (should (string= (osta-format :input '(:type "checkbox" :checked nil))
                   "<input type=\"checkbox\" />%s")))

(ert-deftest osta-component-test ()
  ;; `osta-html-raise-error-p' is set to nil by default
  ;; and affects only objects that can't be components
  ;; in `osta-component' function
  (let ((osta-html-raise-error-p nil))
    (should (string= (osta-component nil) ""))
    (should (string= (osta-component '()) ""))
    (should (string= (osta-component "foo") "foo"))

    ;; numbers are coerced to string
    (should (string= (osta-component 16) "16"))

    ;; empty tags
    (should (string= (osta-component '(:div)) "<div></div>"))

    ;; voids tags
    (should (string= (osta-component '(:hr)) "<hr />"))
    (should (string= (osta-component '(:div (:hr))) "<div><hr /></div>"))
    (should (string= (osta-component '(:div "foo" (:hr) "bar")) "<div>foo<hr />bar</div>"))

    ;; nesting tags
    (should (string= (osta-component '(:p "foo")) "<p>foo</p>"))
    (should (string= (osta-component '(:p "foo" "bar")) "<p>foobar</p>"))
    (should (string= (osta-component '(:section (:div (:p "foo"))))
                     "<section><div><p>foo</p></div></section>"))
    (should (string= (osta-component '(:div (:div "foo" "bar"))) "<div><div>foobar</div></div>"))
    (should (string= (osta-component '(:div "a" (:p "b" (:span "c") "d") "e"))
                     "<div>a<p>b<span>c</span>d</p>e</div>"))

    ;; attributes
    (should (string= (osta-component '(:div (@ :id "id" :class "class") "foo"))
                     "<div id=\"id\" class=\"class\">foo</div>"))
    (should (string= (osta-component '(:p/id-in-tag (@ :id "id-in-plist") (:span "foo")))
                     "<p id=\"id-in-plist\"><span>foo</span></p>"))
    (should (string= (osta-component '(:p.class-in-tag (@ :class "class-in-plist") "foo"))
                     "<p class=\"class-in-tag class-in-plist\">foo</p>"))

    ;; accept list of components
    (should (string= (osta-component '("foo" 1 "bar")) "foo1bar"))
    (should (string= (osta-component '((:li "a") (:li "b")))
                     "<li>a</li><li>b</li>"))

    ;; tag content can be lists of components
    (should (string= (osta-component '(:ul ((:li "1") (:li "2"))))
                     "<ul><li>1</li><li>2</li></ul>"))
    (should (string= (osta-component '(:ul (@ :id "id") ((:li "1") (:li "2"))))
                     "<ul id=\"id\"><li>1</li><li>2</li></ul>"))

    ;; tag content can be forms
    (should (string=
             (osta-component `(:ul ,(mapcar (lambda (n) `(:li ,n)) '(1 2))))
             "<ul><li>1</li><li>2</li></ul>"))
    (should (string=
             (osta-component `(:ul (@ :id "id") ,(mapcar (lambda (n) `(:li ,n)) '(1 2))))
             "<ul id=\"id\"><li>1</li><li>2</li></ul>"))

    ;; components can be generated by a form
    (should (string= (osta-component `(:p ,(concat "foo-" "bar")))
                     "<p>foo-bar</p>"))

    ;; tag content and attributes can be vars
    (should (string= (let ((x "foo") (y "bar"))
                       (osta-component `(:p (@ :id ,x) ,y)))
                     "<p id=\"foo\">bar</p>"))
    (should (string= (osta-component
                      (let ((x "foo") (y "bar"))
                        `(:p (@ :id ,x) ,y)))
                     "<p id=\"foo\">bar</p>")))

  ;; objects that can't be components exported as empty string
  ;; when `osta-html-raise-error-p' is `nil' (which is the default)
  (let ((osta-html-raise-error-p nil))
    (should (string= (osta-component []) ""))
    (should (string= (osta-component t) "")))

  ;; objects that can't be components raise an error when
  ;; when `osta-html-raise-error-p' is `t'
  (let ((osta-html-raise-error-p t))
    (should-error (string= (osta-component []) ""))
    (should-error (string= (osta-component t) ""))
    ;; `nil' and '() which are `eq' always return empty string ""
    (should (string= (osta-component nil) ""))
    (should (string= (osta-component '()) ""))))

;; (global-set-key (kbd "C-<f1>") (lambda () (interactive)(ert "osta-html-test")))

(ert-deftest osta-html-test ()
  ;; `osta-html-raise-error-p' is set to nil by default
  ;; and affects only object that can't be components
  ;; in `osta-html' function
  (let ((osta-html-raise-error-p nil))
    (should (string= (osta-html nil) ""))
    (should (string= (osta-html '()) ""))
    (should (string= (osta-html "foo") "foo"))
    (should (string= (osta-html "foo" nil "bar") "foobar"))

    ;; numbers are coerced to string
    (should (string= (osta-html 16) "16"))

    (should (string= (osta-html "foo" "bar") "foobar"))
    (should (string= (osta-html '(:p "foo") "bar") "<p>foo</p>bar"))
    (should (string= (osta-html '(:p "foo") '(:p "bar")) "<p>foo</p><p>bar</p>"))
    (should (string= (osta-html '(:div)) "<div></div>"))
    (should (string= (osta-html '(:div "foo")) "<div>foo</div>"))
    (should (string= (osta-html '(:div (:p "foo"))) "<div><p>foo</p></div>"))
    (should (string= (osta-html '(:section (:div (:p "foo"))))
                     "<section><div><p>foo</p></div></section>"))
    (should (string= (osta-html '(:div (:p "foo") (:p "bar")))
                     "<div><p>foo</p><p>bar</p></div>"))

    ;; voids tags
    (should (string= (osta-html '(:hr)) "<hr />"))

    ;; attributes
    (should (string= (osta-html '(:div (@ :id "id" :class "class") "foo"))
                     "<div id=\"id\" class=\"class\">foo</div>"))
    (should (string= (osta-html '(:p/id-in-tag (@ :id "id-in-plist") (:span "foo")))
                     "<p id=\"id-in-plist\"><span>foo</span></p>"))

    (should (string= (osta-html '(:p.class-in-tag (@ :class "class-in-plist") "foo"))
                     "<p class=\"class-in-tag class-in-plist\">foo</p>"))

    ;; accept list of components
    (should (string= (osta-html '("foo" 1 "bar")) "foo1bar"))
    (should (string= (osta-html '((:li "a") (:li "b")))
                     "<li>a</li><li>b</li>"))
    (should (string= (osta-html '(:li "a") '((:li "b") (:li "c")) '(:li "d"))
                     "<li>a</li><li>b</li><li>c</li><li>d</li>"))

    ;; tag content can be lists of components
    (should (string= (osta-html '(:ul ((:li "1") (:li "2"))))
                     "<ul><li>1</li><li>2</li></ul>"))
    (should (string= (osta-html '(:ul (@ :id "id") ((:li "1") (:li "2"))))
                     "<ul id=\"id\"><li>1</li><li>2</li></ul>"))

    ;; tag content can be forms
    (should (string=
             (osta-html `(:ul ,(mapcar (lambda (n) `(:li ,n)) '(1 2))))
             "<ul><li>1</li><li>2</li></ul>"))
    (should (string=
             (osta-html `(:ul (@ :id "id") ,(mapcar (lambda (n) `(:li ,n)) '(1 2))))
             "<ul id=\"id\"><li>1</li><li>2</li></ul>"))

    ;; components can be generated by a form
    (should (string= (osta-html (mapcar (lambda (n) `(:p ,n)) '(1 2 3)))
                     "<p>1</p><p>2</p><p>3</p>"))

    ;; tag content and attributes can be vars
    (should (string= (let ((x "foo") (y "bar"))
                       (osta-html `(:p (@ :id ,x) ,y)))
                     "<p id=\"foo\">bar</p>"))
    (should (string= (osta-html
                      (let ((x "foo") (y "bar"))
                        `(:p (@ :id ,x) ,y)))
                     "<p id=\"foo\">bar</p>")))

  ;; objects that can't be components exported as empty string
  ;; when `osta-html-raise-error-p' is `nil' (which is the default)
  (let ((osta-html-raise-error-p nil))
    (should (string= (osta-html []) ""))
    (should (string= (osta-html t) ""))
    (should (string= (osta-html [] "foo" t "bar") "foobar")))

  ;; objects that can't be components raise an error when
  ;; when `osta-html-raise-error-p' is `t'
  (let ((osta-html-raise-error-p t))
    (should-error (string= (osta-html []) ""))
    (should-error (string= (osta-html t) ""))
    (should-error (string= (osta-html [] "foo" t "bar") "foobar"))))

;;; pages

(ert-deftest osta-page-p-test ()
  (should-not
   (org-test-with-temp-text "<point>* page 1
:PROPERTIES:
:CUSTOM_ID: /date-1/page-1/
:END:"
     (osta-page-p (org-element-context))))
  (should-not
   (org-test-with-temp-text "<point>* page 1
:PROPERTIES:
:OSTA_PAGE: t
:END:"
     (osta-page-p (org-element-context))))
  (should-not
   (org-test-with-parsed-data "* page 1
:PROPERTIES:
:OSTA_PAGE: t
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
       (osta-page-p headline-1))))
  (should
   (equal
    (org-test-with-temp-text "<point>* page 1
:PROPERTIES:
:OSTA_PAGE: t
:CUSTOM_ID: /date-1/page-1/
:END:"
      (let ((page (osta-page-p (org-element-context))))
        `(:raw-value ,(org-element-property :raw-value page)
          :OSTA_PAGE ,(org-element-property :OSTA_PAGE page)
          :CUSTOM_ID ,(org-element-property :CUSTOM_ID page))))
    '(:raw-value "page 1"
      :OSTA_PAGE "t"
      :CUSTOM_ID "/date-1/page-1/"))))

(ert-deftest osta-page-test ()
  (should-not
   (org-test-with-parsed-data "* page 1
:PROPERTIES:
:CUSTOM_ID: /date-1/page-1/
:END:
  "
     (let ((headline (car (org-element-map tree 'headline #'identity))))
       (osta-page headline))))
  (should-not
   (org-test-with-parsed-data "* page 1
:PROPERTIES:
:OSTA_PAGE: t
:END:
  "
     (let ((headline (car (org-element-map tree 'headline #'identity))))
       (osta-page headline))))
  (should
   (equal
    (org-test-with-parsed-data "* page 1
:PROPERTIES:
:OSTA_PAGE: t
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
             (page-headline-page-1 (osta-page headline-page-1))
             (headline-2 (funcall get-headline "headline 2"))
             (page-headline-2 (osta-page headline-2))
             (paragraph (car (org-element-map tree 'paragraph #'identity)))
             (page-paragraph (osta-page paragraph))
             (content-paragraph (substring-no-properties (nth 2 paragraph))))
        `(:headline-page-1
          (:raw-value ,(org-element-property :raw-value headline-page-1)
           :OSTA_PAGE ,(org-element-property :OSTA_PAGE page-headline-page-1)
           :CUSTOM_ID ,(org-element-property :CUSTOM_ID page-headline-page-1))
          :headline-2
          (:raw-value ,(org-element-property :raw-value headline-2)
           :OSTA_PAGE ,(org-element-property :OSTA_PAGE page-headline-2)
           :CUSTOM_ID ,(org-element-property :CUSTOM_ID page-headline-2))
          :paragraph
          (:content ,content-paragraph
           :OSTA_PAGE ,(org-element-property :OSTA_PAGE page-paragraph)
           :CUSTOM_ID ,(org-element-property :CUSTOM_ID page-paragraph)))))
    '(:headline-page-1
      (:raw-value "page 1"
       :OSTA_PAGE "t"
       :CUSTOM_ID "/date-1/page-1/")
      :headline-2
      (:raw-value "headline 2"
       :OSTA_PAGE "t"
       :CUSTOM_ID "/date-1/page-1/")
      :paragraph
      (:content "some text\n"
       :OSTA_PAGE "t"
       :CUSTOM_ID "/date-1/page-1/")))))

(ert-deftest osta-page-path-test ()
  (should
   (string=
    (org-test-with-temp-text "<point>* page 1
:PROPERTIES:
:OSTA_PAGE: t
:CUSTOM_ID: /date-1/page-1/
:END:"
      (osta-page-path (org-element-context)))
    "/date-1/page-1/"))
  (should-not
   (org-test-with-temp-text "<point>* page 1
:PROPERTIES:
:CUSTOM_ID: /date-1/page-1/
:END:"
     (osta-page-path (org-element-context))))
  (should-not
   (org-test-with-temp-text "<point>* page 1
:PROPERTIES:
:OSTA_PAGE: t
:END:"
     (osta-page-path (org-element-context)))))
