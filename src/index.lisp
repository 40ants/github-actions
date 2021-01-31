(defpackage #:github-matrix/index
  (:use #:cl)
  (:import-from #:spinneret)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:github-matrix/utils
                #:get-base-url)
  (:import-from #:cl-ppcre
                #:register-groups-bind)
  (:export
   #:render))
(in-package github-matrix/index)


(defvar *analytics-code*
  "<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src=\"https://www.googletagmanager.com/gtag/js?id=G-Z6GYGEXD6N\"></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'G-Z6GYGEXD6N');
</script>")


(defvar *metrika-code*
  "<!-- Yandex.Metrika counter -->
<script type=\"text/javascript\" >
   (function(m,e,t,r,i,k,a){m[i]=m[i]||function(){(m[i].a=m[i].a||[]).push(arguments)};
   m[i].l=1*new Date();k=e.createElement(t),a=e.getElementsByTagName(t)[0],k.async=1,k.src=r,a.parentNode.insertBefore(k,a)})
   (window, document, \"script\", \"https://mc.yandex.ru/metrika/tag.js\", \"ym\");

   ym(71694151, \"init\", {
        clickmap:true,
        trackLinks:true,
        accurateTrackBounce:true
   });
</script>
<noscript><div><img src=\"https://mc.yandex.ru/watch/71694151\" style=\"position:absolute; left:-9999px;\" alt=\"\" /></div></noscript>
<!-- /Yandex.Metrika counter -->"
  )

(defun extract-user-and-project (url)
  (register-groups-bind (user project)
      ("^https://github.com/(.*?)/([^/?]*)" url)
    (values user project)))


(defun make-badge-url-from-github-url (env url)
  (multiple-value-bind (user project)
      (extract-user-and-project url)
    (when (and user project)
      (let ((base-url (get-base-url env)))
        (fmt "~A/~A/~A/matrix.svg"
             base-url
             user
             project)))))


(defun render (env &key url &allow-other-keys)
  (spinneret:with-html-string
    (:html
     (:head
      (:title "Github Actions Matrix Badger!")
      (:raw *analytics-code*)
      (:raw *metrika-code*)
      (:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/github-fork-ribbon-css/0.2.3/gh-fork-ribbon.min.css"))
     (:body
      (:a :href "https://lisp-lang.org"
          :style "display: block; position: absolute; right: 0; margin-top: -8px;"
          (:img :src "https://40ants.com/img/made-with-lisp.svg"
                :alt "Lisp Logo"
                :width "70"))
      ;; Ribbon from:
      ;; https://github.com/simonwhitaker/github-fork-ribbon-css
      (:a :class "github-fork-ribbon"
          :href "https://github.com/40ants/github-matrix"
          :data-ribbon "Fork me on GitHub"
          :title "Fork me on GitHub"
          "Fork me on GitHub")
      (:h1 "Github Actions Matrix Badger!"
           (:a :href "https://www.patreon.com/40ants"
               :style "display: inline-block; position: relative; top: 0.2em;"
               (:img :width "160"
                     :src "https://40ants.com/lisp-project-of-the-day/media/images/patreon-btn.png")))
      ;; (:h2 "Support it "
      ;;      (:a :href "https://www.patreon.com/40ants"
      ;;          "at Patreon!"))
      (:p "Enter the URL of a project to render it's action's matrix:")
      (:div
       (:form :method :get
              :action "/"
              (:input :type "text"
                      :name "url"
                      :placeholder "https://github.com/40ants/cl-info"
                      :style "width: 50%"
                      :value url)
              (:input :type "submit"))

       (when url
         (log:debug "Creating preview for" url)

         (let ((badge-url (make-badge-url-from-github-url env url)))
           (:h2 "Preview")
           (cond
             (badge-url
              (:p (:img :src badge-url))
              (:h2 "Insert this code to README")

              (:h3 "Markdown")
              (:code
               (:pre
                (fmt "[![](~A)](~A)"
                     badge-url
                     url)))
              (:h3 "reStructured Text")
              (:code
               (:pre
                (fmt "
.. image:: ~A
    :target: ~A
"
                     badge-url
                     url)))

              (:h3 "Raw HTML")
              (:code
               (:pre
                (fmt "
<a href=\"~A\">
  <img src=\"~A\"/>
</a>"
                     url
                     badge-url))))
             (t (:p "ERROR: Unable to parse URL"))))))))))
