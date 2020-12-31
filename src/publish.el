(require 'org)
(require 'subr-x)
(require 's)
(require 'dash)

; TODO fucking hell, it doesn't seem capable of resolving symlinks

(setq   exobrain/rootdir    default-directory)
(defvar exobrain/input-dir  nil)
(defvar exobrain/public-dir nil)
(defvar exobrain/output-dir nil)


;; disable ~ files
(setq make-backup-files nil)


(require 'ox)
(require 'ox-org)
(require 'ox-md)
(require 'ox-html)

;; see https://github.com/emacsmirror/advice-patch
(require 'advice-patch)
;; whoa nice
(advice-patch 'org-html-timestamp
              "<span class=\"timestamp\">%s</span>"
              "<span class=\"timestamp-wrapper\"><span class=\"timestamp\">%s</span></span>")



;; TODO share with compile-org?
(setq org-export-with-author nil)
(setq org-export-preserve-breaks t) ;; by default it collapses consecutive lines.. usually undesirable

;; TODO shit. filetags don't get inherited??
;; ugh... maybe could write a script to hack them back somehow..


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

;; fuck. default org-mode ids are non-deterministic (and even change inbetween emacs invocations)
;; https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors looks really good
;; it worked, but then I moved some code around and it stopped for some reason...
;; too exhausted to debug it, so will use it late
;; sometimes I fucking hate emacs.
(defun exobrain/org-export-get-reference (datum info)
  (let* ((title (org-element-property :raw-value datum)))
    (md5 title)))
(advice-add #'org-export-get-reference :override #'exobrain/org-export-get-reference)

;; (defun exobrain/before-org-org-headline (a b c)
;;   (message "BEFORE HEADLINE"))

;; (defun exobrain/around-org-org-section (orig &rest args)
;;   ;; (message "YYYYY %s XXXX %d" args (length args))
;;   ;; fucking elisp, what's wrong with it??? why didn't (apply orig section contents info) work???
;;   (cl-destructuring-bind (section contents info) args
;;     (let* ((parent (org-export-get-parent-headline section))
;;            (ref    (org-export-get-reference parent info)))
;;       ;; TODO handle no parent?? (before first headline)
;;       (progn (message "BEFORE SECTION %s" ref)
;;              (if (and ref parent) (org-element-put-property parent :alalaa ref))
;;              (apply orig args)))))
;; ;; TODO plist-put info :key value ???


(defun exobrain/org-org-property-drawer (drawer contents info)
  ;; FIXME fuck, it seems that it doesn't get called if the property drawer is missing altoghether??. shit.
  (let* ((parent (org-export-get-parent-headline drawer))
         (ref    (org-export-get-reference parent info))
         (res    (org-org-identity drawer contents info))
         ;; fuck nested let statements too.
         (cont2  (if (s-contains? ":ID:" contents)
                     ;; if already has id don't do anything
                     ;; I dunno why thought such tabulation is a good idea for if-else.. fucking hate this.
                     contents
                   ;; otherwise generate id
                   (format "%s:ID: %s\n" contents ref))))
    (org-org-identity drawer cont2 info)))

;; (advice-add #'org-org-headline        :before #'exobrain/before-org-org-headline)
;; (advice-add #'org-org-section         :around #'exobrain/around-org-org-section)
;; (advice-add #'org-org-property-drawer :around #'exobrain/around-org-org-property-drawer)

(org-export-define-derived-backend
 'my-org
 'org
 :translate-alist '((property-drawer . exobrain/org-org-property-drawer))) ;; default is identity


(defun org-org-publish-to-my-org (plist filename pub-dir)
  (org-publish-org-to 'my-org filename ".org" plist pub-dir))


;; https://orgmode.org/manual/Publishing-options.html#Publishing-options
;; TODO exclude-tags
;; with-author? with-timestamps? with-date?

(defun exobrain/extra-filter (output backend info)
  (check-output
   '("python3" "src/filter_org.py")
   :input output
   :cwd exobrain/rootdir))

(defun exobrain/md-org-make-tag-string (tags)
  (apply #'s-concat
   (-map
    (lambda (tag) (s-wrap tag "<span class='tag'>" "</span>"))
    tags)))


;; fucking hell, it's defsubst https://www.gnu.org/software/emacs/manual/html_node/elisp/Inline-Functions.html
;; that's why advice doesn't work
;; I hate elisp.
;; (advice-add #' org-element-property :around #'exobrain/md-org-element-property)

; (defun exobrain/org-md-headline (orig headline contents info)
;   (cl-letf (((symbol-function 'org-element-property) 'exobrain/md-org-element-property))
;     (funcall orig headline contents info)))

;; TODO share with rest of the system..
(setq exobrain/state-keywords
      '(("TODO"   . "todo")
        ("START"  . "todo") ;; TODO start?
        ("STRT"   . "todo") ;; TODO start?
        ("DONE"   . "done")
        ("CANCEL" . "cancel")))


;; TODO done keywords should be marked separately..
(defun exobrain/org-md--headline-title (orig style level title &optional anchor tags)
  ;; TODO todo keywords are at the very beginning? so should work?
  (let* ((spl (s-split-up-to " " title 1))
         (fst (car spl))
         (snd (cadr spl))
         (kwd (let ((cls (cdr (assoc fst exobrain/state-keywords))))
                (if cls
                    (format "<span class='state %s'>%s</span>" cls fst)
                  fst)))
         (title (concat kwd " " snd)))
    (funcall orig style level title anchor tags)))

(advice-add #'org-md--headline-title :around #'exobrain/org-md--headline-title)

(defun exobrain/org-md-publish-to-md (orig-fun plist filename pub-dir)
  ;; fucking hell. I just hate elisp so much
  (cl-letf (((symbol-function 'org-make-tag-string) 'exobrain/md-org-make-tag-string))
    (funcall orig-fun plist filename pub-dir)))

(advice-add #'org-md-publish-to-md :around #'exobrain/org-md-publish-to-md)


;; TODO reuse some props??
(setq exobrain-md
      `("exobrain"
        :base-directory ,exobrain/public-dir
        :base-extension "org"
        :publishing-directory ,exobrain/output-dir
        :recursive t
        :publishing-function org-md-publish-to-md

        :with-tags          t
        :with-todo-keywords t
        :with-priority      t))

(setq exobrain-html
      `("exobrain-html"
        :base-directory ,exobrain/public-dir
        :base-extension "org"
        :publishing-directory ,exobrain/output-dir
        :recursive t
        :publishing-function org-html-publish-to-html

        :with-tags          t
        :with-todo-keywords t
        :with-priority      t

        :html-head "
<style>
#sidebar {
  position: fixed;
  left: 0;
  top: 0;
  bottom: 0;

  /* todo scroll? */
  padding-right: 1em;
  border: 2px solid;
}
body {
  width: 1000px;
  margin: auto;
}
</style> 
"))


(setq
 org-publish-project-alist
 `(("exobrain-inputs-public"
    :base-directory ,exobrain/input-dir
    :base-extension "org"
    :publishing-directory ,exobrain/public-dir
    :recursive t
    :publishing-function org-org-publish-to-my-org

    :auto-sitemap t
    :sitemap-format-entry my/org-publish-sitemap-entry
    ;; TODO maybe won't be needed if I use my own exporter?
    :sitemap-filename "SUMMARY.org"
    ;; TODO maybe include summary into org-mode file directly? & wrap somehow...

    ;; shit. only isolated timestamps work...
    ;; https://github.com/bzg/org-mode/blob/817c0c81e8f6d1dc387956c8c5b026ced62c157c/lisp/ox.el#L1896
    ;; :with-timestamps nil

    :with-date nil
    :with-properties t
    :time-stamp-file nil

    ;; TODO not sure if should use final-output??
    :filter-body ,(cons #'exobrain/extra-filter org-export-filter-body-functions)
    ;; :filter-final-output ,(cons #'filt org-export-filter-final-output-functions)
    ;; TODO body??

    ;; :makeindex
    ;; :auto-index t
    ; :index-filename "sitemap.org"
    ; :index-title "Sitemap"

    :with-todo-keywords t
    :with-tags          t

    ;; TODO want to exclude certain tags from displaying in export
    ;; not sure if that's possible without patching org-mode functions :(
    ;; :exclude-tags       ("gr" "graspw")

    :exclude "org.org")
   ,exobrain-md))



    ; TODO????

; TODO shit. refuses to work.
(setq org-html-postamble-format "")


; TODO https://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html


;; TODO after intermediate, run santity check
;;
;;TODO for exobrain, makes sense to generate all ids automatically? just warn they aren't really stable
