(require 'org)
(require 'subr-x)

; TODO fucking hell, it doesn't seem capable of resolving symlinks


;; disable ~ files
(setq make-backup-files nil)

;; TODO share with compile-org?
(setq org-export-with-author nil)


;; examples of sitemap formatting
;; https://github.com/nanjj/nanjj.github.io/blob/4338fa60b07788885d3d4c8b2c684360a67e8098/org-publish.org


(defun my/org-publish-sitemap-entry (entry style project)
  ;; mdbook doesn't like list item not being a link
  ;; and default sitemap entry function explicitly ignores directories
  (if (directory-name-p entry)

      ;; README.md got some special handling, so we abuse that
      ;; https://rust-lang.github.io/mdBook/format/config.html?highlight=readme#configuring-preprocessors
      (format "[[file:%sREADME.org][%s]]" entry (directory-file-name entry))
      (org-publish-sitemap-default-entry entry style project)))


;; TODO ugh. not all timestamps are detected correctly??
;; TODO instead, map to dates only? check as well
(defun my-timestamp (timestamp _contents _info)
  ;; (message "TIMESTAMP!!! %s" timestamp)
  "TS!!")

(require 'ox)
(require 'ox-org)
(org-export-define-derived-backend
 'my-org 'org
 :translate-alist
 '(
   (timestamp . my-timestamp)))

(message "%s" (org-export-get-backend 'my-org))


(defun org-org-publish-to-my-org (plist filename pub-dir)
  (org-publish-org-to 'my-org filename ".org" plist pub-dir))


(setq
 org-publish-project-alist
 '(("exobrain"
    :base-directory "content"
    :base-extension "org"
    :publishing-directory "intermediate"
    :recursive t
    :publishing-function org-org-publish-to-my-org

    :auto-sitemap t
    :sitemap-format-entry my/org-publish-sitemap-entry
    :sitemap-filename "SUMMARY.org"


    ;; :makeindex
    ;; :auto-index t
    ; :index-filename "sitemap.org"
    ; :index-title "Sitemap"

    :exclude "org.org")
   ("exobrain2"
    :base-directory "intermediate"
    :base-extension "org"
    :publishing-directory "markdown"
    :recursive t
    :publishing-function org-md-publish-to-md)))

    ; TODO????

; TODO shit. refuses to work.
(setq org-html-postamble-format "")


; TODO https://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html


;; TODO after intermediate, run santity check
