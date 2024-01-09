; ugh. fucking hell, it doesn't seem capable of resolving symlinks
(setq   exobrain/rootdir    default-directory)
(defvar exobrain/input-dir     nil)
(defvar exobrain/public-dir    nil)
(defvar exobrain/html-dir      nil)

;; docs: https://orgmode.org/manual/Publishing-options.html#Publishing-options

(require 'org)
(require 'ox)
(require 'ox-html)

(require 's)
(require 'dash) ;; used for -difference

;; disable ~ files
(setq make-backup-files nil)


(defun exobrain/hack-property-block (fun &rest args)
  (let* ((res (apply fun args)))
    ;; no clue why default class is "example" :shrug: (using same format as in blog)
    (if res (s-replace
             "<pre class=\"example\">"
             "<div class='properties'>"
             (s-replace
              "</pre>"
              "</div>"
              res)))))
(advice-add #'org-html-property-drawer :around #'exobrain/hack-property-block)

;; TODO could force timestamps to emit <time> element

;; todo copied from blog
;; FIXME why did I do this???
;; would be nice to just keep original tags I think
(add-to-list 'org-html-text-markup-alist '(verbatim . "<samp class='inline'>%s</samp>"))
(add-to-list 'org-html-text-markup-alist '(code     . "<code class='inline'>%s</code>"))


(defun exobrain/org-export-get-reference (datum info)
  (let* ((elem_custom_id (org-element-property :CUSTOM_ID datum))
         (elem_id        (org-element-property :ID        datum)))
    (or elem_custom_id elem_id)))
;; NOTE: needed for html export as well?
(advice-add #'org-export-get-reference :override #'exobrain/org-export-get-reference)

;;; for html, we want inherited tags to be displayed within the headlines (since otherwise it's too opaque)
;;; the easiest way seems to mark them in a special way and then handle during rendering...
(setq exobrain/inh-prefix "INHERITED_")

(defun exobrain/hack-html-tags (headline contents info)
  (let* ((selftags (org-export-get-tags headline info nil nil))
         (partags  (org-export-get-tags (org-export-get-parent-element headline) info nil t))
         (selftags  (-difference selftags partags))) ;; if parent includes the tag, treat it as inherited
    (org-element-put-property headline
                              :tags
                              (-concat selftags
                                       (--map (s-concat exobrain/inh-prefix it) partags)))))
(advice-add #'org-html-headline :before #'exobrain/hack-html-tags)


;; FIXME just backwards compat with old export
;; can be removed later
(defun --set-empty-tags (fun &rest args)
  (let ((res (apply fun args)))
    (if res res "<span class=\"tag\"></span>")))
(advice-add #'org-html--tags  :around #'--set-empty-tags)


;; TODO done keywords should be marked separately..
(setq org-todo-keywords '((sequence "TODO" "NEXT" "STRT" "START" "WIP" "WAIT" "|" "CNCL" "CANCEL" "DONE")))

;; used during exporting regular timestamps (default includes day of week)
(setq org-time-stamp-formats '("<%Y-%m-%d>"))


(setq org-export-with-broken-links t) ;; TODO mm not ideal
;;
(setq org-confirm-babel-evaluate nil)
(setq org-export-preserve-breaks t) ;; by default it collapses consecutive lines.. usually undesirable
(setq org-export-with-section-numbers nil)


;; TODO share with org-mode export?
(setq org-export-with-priority   t)
(setq org-export-time-stamp-file nil)
;; (setq org-export-with-properties '("ID" "CUSTOM_ID" "CREATED" "PUBLISHED"))
(setq org-export-with-properties '("CREATED"))


;; eh. a bit hacky, but does the job
;; doesn't seem that org-mode exposes filetags properly
(defun org-html--build-pre/postamble (type info)
  (if (eq type 'preamble)
      (let* ((filetags (plist-get info :filetags)))
        (format "<div class='filetags'>%s</div>" (s-join " " filetags)))
    nil))


; I'm using my own styles
(setq org-html-head-include-default-style nil)

(setq org-html-head "
<link href='https://beepb00p.xyz/assets/css/default.css'     rel='stylesheet'>
<link href='https://beepb00p.xyz/assets/css/links.css'       rel='stylesheet'>
<link href='https://beepb00p.xyz/assets/css/htmlize.css'     rel='stylesheet'>
<link href='https://beepb00p.xyz/assets/css/org-default.css' rel='stylesheet'>
<link href='https://beepb00p.xyz/assets/css/org-extra.css'   rel='stylesheet'>
<link  href='/exobrain.css'                                  rel='stylesheet'>
<script src='/settings.js'                                   rel='stylesheet'></script>
")

(setq exobrain/project-org2html
      `("exobrain-html"
        :base-directory ,exobrain/public-dir
        :base-extension "org"
        :publishing-directory ,exobrain/html-dir
        :publishing-function org-html-publish-to-html

        ;; todo ugh. seems that it's dumping sitemap to the source dir, and it can't be changed?
        :auto-sitemap nil

        :recursive t
        ))
