; ugh. fucking hell, it doesn't seem capable of resolving symlinks
(setq   exobrain/rootdir    default-directory)
(defvar exobrain/input-dir     nil)
(defvar exobrain/public-dir    nil)
(defvar exobrain/md-dir        nil)
(defvar exobrain/html-dir      nil)
(defvar exobrain/filter        nil)

;; docs: https://orgmode.org/manual/Publishing-options.html#Publishing-options

(require 'org)
(require 'ox)
(require 'ox-html)

(require 'subr-x)
(require 's)
(require 'dash)

;; disable ~ files
(setq make-backup-files nil)


;; see https://github.com/emacsmirror/advice-patch
(require 'advice-patch)
;; whoa advice-patch is really nice

;; no clue why default class is "example" :shrug: (using same format as in blog)
(advice-patch 'org-html-property-drawer
              "<div class='properties'>\n%s</div>"
              "<pre class=\"example\">\n%s</pre>")

;; TODO could force timestamps to emit <time> element

;; these ids are relaly unnecessary, just littering the anchors
(advice-patch 'org-html-headline
              "<%1$s class=\"%3$s\">%4$s%5$s</%6$s>\n"
              "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n")
;; (advice-patch 'org-html-headline
;;               "f"
;;               '(concat "outline-container-" (org-export-get-reference headline info)))
;; TODO shit. it can't override multiple definitions at once...
;; so have to choose one here.. or could replace larger form


;; fucking hell. seriosly?????
(setq cmp-ignore-first-unicode
      (lambda (sa sb)
        (let* ((aspair (lambda (s)
                         (let* ((title (if (funcall org-file-p s)
                                           (org-publish-find-title s project)
                                         " "))
                                (c (substring title 0 1))
                                (cp (get-char-code-property (string-to-char c) 'general-category))
                                (mt (if (eq cp 'So) title (concat (char-to-string 150000) title))) ;; add extra big unicode character
                                (res (if (funcall org-file-p s)
                                         (concat (file-name-directory s) mt)
                                       s)))
                           res))))
          (string< (funcall aspair sa) (funcall aspair sb)))))

;; I want the ones with emoji to go first (they are more important)
(advice-patch 'org-publish-sitemap
              `(sort files ,cmp-ignore-first-unicode)
              ;; fucking annoying... why does it complain about "unused lexical variable"???
              '(sort files sort-predicate))
;; TODO hmm sometimes it's not dumping sitemap... for no apparent reason?


;; TODO shit. filetags don't get inherited??
;; ugh... maybe could write a script to hack them back somehow..
;; todo copied from blog
(add-to-list 'org-html-text-markup-alist '(verbatim . "<samp class='inline'>%s</samp>"))
(add-to-list 'org-html-text-markup-alist '(code     . "<code class='inline'>%s</code>"))


;; examples of sitemap formatting
;; https://github.com/nanjj/nanjj.github.io/blob/4338fa60b07788885d3d4c8b2c684360a67e8098/org-publish.org

(defun exobrain/org-publish-sitemap-entry (entry style project)
  ;; mdbook doesn't like list item not being a link
  ;; and default sitemap entry function explicitly ignores directories
  (if (directory-name-p entry)
      ;; README.md got some special handling, so we abuse that
      ;; https://rust-lang.github.io/mdBook/format/config.html?highlight=readme#configuring-preprocessors
      (format "[[file:%sREADME.org][%s]]" entry (directory-file-name entry))
      (org-publish-sitemap-default-entry entry style project)))

(defun exobrain/org-export-get-reference (datum info)
  (let* ((elem_custom_id (org-element-property :CUSTOM_ID datum))
         (elem_id        (org-element-property :ID        datum)))
    (or elem_custom_id elem_id)))
;; NOTE: needed for html export as well?
(advice-add #'org-export-get-reference :override #'exobrain/org-export-get-reference)

;; todo what is org-element-map?
;; todo plist-get vs org-element-property??? I guess plist is for pure plists?

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


;; note: uncomment to emit org-mode with inherited tags (eh, )
;; (setq exobrain/inh-prefix "")
;; (advice-add #'org-org-headline :before #'exobrain/hack-html-tags)

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


;; TODO share with compile-org?
(setq org-export-exclude-tags '("noexport" "hide"))
(setq org-export-with-broken-links t) ;; TODO mm not ideal
;;
(setq org-confirm-babel-evaluate nil)
(setq org-export-preserve-breaks t) ;; by default it collapses consecutive lines.. usually undesirable
(setq org-export-with-section-numbers nil)
(setq exobrain/export-settings
      '(:recursive          t
        :with-priority      t
        ;; todo eh, not sure if I need anything else?
        :with-properties    ("ID" "CUSTOM_ID" "CREATED" "PUBLISHED")
        :with-tags          t
        ;; shit. only impacts isolated timestamps... (i.e. not next to TODO keywords etc)
        ;; https://github.com/bzg/org-mode/blob/817c0c81e8f6d1dc387956c8c5b026ced62c157c/lisp/ox.el#L1896
        ;; or maybe doesn't impact anything at all?? has no effect if set to t, same in html export
        :with-timestamps    t
        :with-todo-keywords t

        :time-stamp-file    nil))

;; eh. a bit hacky, but does the job
;; doesn't seem that org-mode exposes filetags properly
(defun org-html--build-pre/postamble (type info)
  (if (eq type 'preamble)
      (let* ((filetags (plist-get info :filetags)))
        (format "<div class='filetags'>%s</div>" (s-join " " filetags)))
    nil))

(setq exobrain/project-org2html
      `("exobrain-html"
        :base-directory ,exobrain/public-dir
        :base-extension "org"
        :publishing-directory ,exobrain/html-dir
        :publishing-function org-html-publish-to-html

        ;; todo ugh. seems that it's dumping sitemap to the source dir, and it can't be changed?
        :auto-sitemap nil

        :html-preamble t

        ,@exobrain/export-settings
        :with-properties    ("CREATED") ;; todo maybe published too? or stuff "created" into the heading??

        ; I'm using my own styles
        :html-head-include-default-style nil
        ;; todo not sure if I need org-html-scripts? adds CodeHighlightOn thing
        ;; TODO get rid of this, I don't need it
        ;; :html-head-include-scripts t

        :html-head "
<link href='https://beepb00p.xyz/assets/css/default.css'     rel='stylesheet'>
<link href='https://beepb00p.xyz/assets/css/links.css'       rel='stylesheet'>
<link href='https://beepb00p.xyz/assets/css/htmlize.css'     rel='stylesheet'>
<link href='https://beepb00p.xyz/assets/css/org-default.css' rel='stylesheet'>
<link href='https://beepb00p.xyz/assets/css/org-extra.css'   rel='stylesheet'>
<link  href='/exobrain.css'                                  rel='stylesheet'>
<script src='/settings.js'                                   rel='stylesheet'></script>
"

        :html-postamble     nil))


;; TODO after intermediate, run santity check
