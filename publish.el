(require 'org)
; TODO fucking hell, it doesn't seem capable of resolving symlinks
(defun my-org-list-to-org (list &optional params)
  "my list to org"
  (let* ((make-item
          (lambda (type _depth &optional c)
            (concat (if (eq type 'ordered) "1. " "* ")
                    (and c (format "[@%d] " c)))))
         (defaults
           (list :istart make-item
                 :icount make-item
                 :ifmt (lambda (_type contents)
                         (replace-regexp-in-string "\n" "\n  " contents))
                 :dtend " :: "
                 :cbon "[X] "
                 :cboff "[ ] "
                 :cbtrans "[-] ")))
    (org-list-to-generic list (org-combine-plists defaults params))))
(defun my-org-publish-sitemap (title list)
  "my render sitemap"
  (concat "#+TITLE: " title "\n\n"
          (my-org-list-to-org list)))

(setq
 org-publish-project-alist
 '(("blog"
    :base-directory "content"
    :base-extension "org"
    ; :makeindex
    ; :auto-index t
    :auto-sitemap t
    :sitemap-filename "SUMMARY.org"
    ;; :sitemap-function my-org-publish-sitemap
    ; :index-filename "sitemap.org"
    ; :index-title "Sitemap"
    :publishing-directory "public/blog"
    :recursive t
    :publishing-function org-md-publish-to-md
    :exclude "org.org")
   ("website" :components ("blog")))) ; TODO????

; TODO shit. refuses to work.
(setq org-html-postamble-format "")


; TODO https://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html
