(require 'org)
; TODO fucking hell, it doesn't seem capable of resolving symlinks
(setq
 org-publish-project-alist
 '(("blog"
    :base-directory "content/misc"
    :base-extension "org"
    :auto-index t
    :index-filename "sitemap.org"
    :index-title "Sitemap"
    :publishing-directory "public/blog"
    :recursive t
    :publishing-function org-html-publish-to-html
    :exclude "org.org")
   ("website" :components ("blog")))) ; TODO????

; TODO shit. refuses to work.
(setq org-html-postamble-format "")


; TODO https://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html
