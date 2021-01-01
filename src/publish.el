(require 'org)
(require 'subr-x)
(require 's)
(require 'dash)

; TODO fucking hell, it doesn't seem capable of resolving symlinks

(setq   exobrain/rootdir    default-directory)
(defvar exobrain/input-dir     nil)
(defvar exobrain/public-dir    nil)
(defvar exobrain/output-dir    nil)


;; disable ~ files
(setq make-backup-files nil)


(require 'ox)
(require 'ox-org)
(require 'ox-md)
(require 'ox-html)

(defun pp-org (thing)
  ;; TODO shit. it modifies the thing...
  (org-element-put-property thing :parent nil) ;; otherwise too spammy
  (pp thing))

;; see https://github.com/emacsmirror/advice-patch
(require 'advice-patch)
;; whoa advice-patch is really nice

 ;; remove useless timestmap-wrapper class
(advice-patch 'org-html-timestamp
              "<span class=\"timestamp\">%s</span>"
              "<span class=\"timestamp-wrapper\"><span class=\"timestamp\">%s</span></span>")

;; no clue why default class is "example" :shrug: (using same format as in blog)
(advice-patch 'org-html-property-drawer
              "<div class='properties'>\n%s</div>"
              "<pre class=\"example\">\n%s</pre>")

;; same, the default format for html property export is quite dull
(advice-patch 'org-html-node-property
              ;; borrowed from blog
              "<div class='property'><span class='property-name'>%s:</span> <span class='property-value'>%s</span></div>"
              "%s:%s")


;; TODO could force timestamps to emit <time> element

;; these ids are relaly unnecessary, just littering the anchors
(advice-patch 'org-html-section
              "<div class=\"outline-text-%1$d\">\n%3$s</div>\n"
              "<div class=\"outline-text-%d\" id=\"text-%s\">\n%s</div>\n")
(advice-patch 'org-html-headline
              "<%1$s class=\"%3$s\">%4$s%5$s</%6$s>\n"
              "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n")
;; (advice-patch 'org-html-headline
;;               "f"
;;               '(concat "outline-container-" (org-export-get-reference headline info)))
;; TODO shit. it can't override multiple definitions at once...
;; so have to choose one here.. or could replace larger form
(advice-patch 'org-html-headline
              "\n<h%1$d id=\"%2$s\"%3$s><a class='headerlink' href='#%2$s'>¶</a>%4$s</h%5$d>\n"
              "\n<h%d id=\"%s\"%s>%s</h%d>\n")
;; TODO ok, really awesome that I basically replaced a huge chunk of blog with tuny elisp snippets..

;; TODO share with compile-org?
(setq org-export-with-author nil)
(setq org-export-preserve-breaks t) ;; by default it collapses consecutive lines.. usually undesirable

;; TODO shit. filetags don't get inherited??
;; ugh... maybe could write a script to hack them back somehow..


;; examples of sitemap formatting
;; https://github.com/nanjj/nanjj.github.io/blob/4338fa60b07788885d3d4c8b2c684360a67e8098/org-publish.org

;; todo use advice instead
(defun exobrain/org-publish-sitemap-entry (entry style project)
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
    (cl-assert title)
    (md5 title)))
(advice-add #'org-export-get-reference :override #'exobrain/org-export-get-reference)

(defun exobrain/org-org-property-drawer (drawer contents info)
  (unless (-any?
           (lambda (x) (-contains? '("CUSTOM_ID" "ID") (org-element-property :key x)))
           (org-element-contents drawer))
    (let* ((parent (org-export-get-parent-headline drawer))
           (_      (cl-assert parent)) ;; fucking hell..
           (ref    (org-export-get-reference parent info)))
      (org-element-adopt-elements drawer
        (org-element-create 'node-property
                            `(:key "ID" :value ,ref)))))
  (let* ((children (org-element-contents drawer))
         (result   (mapconcat
                    ;; TODO I wonder if this should happen by default in ox-org.el instead of identity 'plaintext' mapping
                    ;; e.g. by deafult node-property never gets translated
                    (lambda (e) (org-export-data e info))
                    (org-element-contents drawer)
                    "")))
    (format ":PROPERTIES:\n%s:END:" result)))

;; todo what is org-element-map?
;; todo plist-get vs org-element-property??? I guess plist is for pure plists?

(defun exobrain/ensure-properties-drawer (section)
  (let* ((hasproperties (-any?
                         (lambda (x) (eq 'property-drawer (org-element-type x)))
                         (org-element-contents section))))
    (unless hasproperties
      (org-element-set-contents section
                                (cons (org-element-create 'property-drawer
                                                          `(:begin 0
                                                            :end 0
                                                            :contents-begin 0
                                                            :contents-end 0
                                                            :post-blank 0
                                                            :post-affiliated 0
                                                            :parent ,section))
                                      (org-element-contents section))))))

;; right. so, we can't use normal org-publish hooks for this..
;; if you look at org-export-data code, it seems that traverses the tree at first
;; and only then starts combining etc
;; so by the time the hook fires, we don't get a chance to modify the org abstract tree
(defun exobrain/before-org-export-data (data info)
  (if (and (org-export-get-parent-headline data) ;; should have parent, otherwise it's the 0 level heading?
           (eq 'section (org-element-type data)))
      (exobrain/ensure-properties-drawer data)))

(advice-add #'org-export-data :before #'exobrain/before-org-export-data)


(org-export-define-derived-backend
 'my-org
 'org
 :translate-alist '((property-drawer . exobrain/org-org-property-drawer))) ;; default is identity, so can't hack via advice


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

(setq org-todo-keywords '((sequence "TODO" "STRT" "START" "WIP" "|" "CNCL" "CANCEL" "DONE")))
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


(defun exobrain/add-nav-sidebar (contents _backend info)
  ;; ugh. what a mess
  (if (string= (plist-get info :input-buffer) "SUMMARY.org")
      contents
    (let ((relroot (file-name-directory (file-relative-name exobrain/input-dir (plist-get info :input-file)))))
      ;; eh. a bit crap that it ends up in the very end of the file, but whatever
      (format "%s\n#+HTML: <nav id='sidebar'>\n#+INCLUDE: %s\n#+HTML: </nav>"
              contents
              ;; for fuck's sake, expand-file-name resolves the .. relatieve link, and there doesn't seem any other path combining function??
              (format "%s%s" (if relroot (concat relroot "/") "") "SUMMARY.org")))))

;; ugh. seems that it works during html conversion, but fails during org-org :(
;; (let ((org-time-stamp-custom-formats
;;        '("<%A, %B %d, %Y>" . "<%A, %B %d, %Y %H:%M:%S>"))
;;       (org-display-custom-times 't))
;;   (org-publish-all))
;; TODO fuck. here as well, timestamps are only translated if they are not within the heading???
(defun exobrain/override-org-timestamp-translate (timestamp &optional boundary)
  "sets custom format to all my timestamps (strips off time, it's just too spammy)"
  (let ((res (org-timestamp-format timestamp "[%Y-%m-%d]")))
    (if boundary
        (error "wtf if boundary?? %s %s" timestamp boundary)
      res)))
(advice-add #'org-timestamp-translate :override #'exobrain/override-org-timestamp-translate)


(defun exobrain/hack-timestamp (ts _)
  (org-element-put-property ts :minute-start nil)
  (org-element-put-property ts :minute-end   nil)
  (org-element-put-property ts :hour-start   nil)
  (org-element-put-property ts :hour-end     nil))
(advice-add #'org-element-timestamp-interpreter :before #'exobrain/hack-timestamp)


(setq exobrain/export-settings
      '(:with-priority      t
        :with-properties    t
        ;; TODO want to exclude certain tags from displaying in export
        ;; not sure if that's possible without patching org-mode functions :(
        ;; :exclude-tags       ("gr" "graspw")
        :with-tags          t
        ;; shit. only impacts isolated timestamps... (i.e. not next to TODO keywords etc)
        ;; https://github.com/bzg/org-mode/blob/817c0c81e8f6d1dc387956c8c5b026ced62c157c/lisp/ox.el#L1896
        ;; or maybe doesn't impact anything at all?? has no effect if set to t, same in html export
        :with-timestamps    t
        :with-todo-keywords t

        :time-stamp-file    nil))

(setq exobrain/project-preprocess-org
      `("exobrain-preprocess-org"
        :base-directory ,exobrain/input-dir
        :base-extension "org" ;; do I even need base-extension?
        :publishing-directory ,exobrain/public-dir
        :recursive t
        :publishing-function org-org-publish-to-my-org

        :auto-sitemap t
        :sitemap-format-entry exobrain/org-publish-sitemap-entry
        ;; TODO maybe won't be needed if I use my own exporter?
        :sitemap-filename "SUMMARY.org"

        ;; TODO not sure if I want to publish all properties here?
        ,@exobrain/export-settings

        :filter-final-output ,(cons #'exobrain/add-nav-sidebar org-export-filter-final-output-functions)))

(setq exobrain/project-org2md
      `("exobrain-org2md"
        :base-directory ,exobrain/public-dir
        :base-extension "org"
        :publishing-directory ,exobrain/output-dir
        :recursive t
        :publishing-function org-md-publish-to-md

        ,@exobrain/export-settings))

(setq exobrain/project-org2html
      `("exobrain-html"
        :base-directory ,exobrain/public-dir
        :base-extension "org"
        :publishing-directory ,exobrain/output-dir
        :recursive t
        :publishing-function org-html-publish-to-html

        ,@exobrain/export-settings
        :with-properties    ("CREATED") ;; todo maybe published too? or stuff "created" into the heading??

        ; I'm using my own styles
        :html-head-include-default-style nil
        ;; todo not sure if I need org-html-scripts? adds CodeHighlightOn thing

        :html-head "\
<style>
/* todo need to make responsive */
:root {
  --sidebar-width: 20rem;
}

#sidebar {
  position: fixed;
  left: 0;
  top: 0;
  bottom: 0;

  min-width: var(--sidebar-width);

  /* todo scroll? */
  padding-right: 1em;
  border: 2px solid;
}
</style>
"
        :html-head-extra "\
<link href='https://beepb00p.xyz/assets/css/default.css'     rel='stylesheet'>
<link href='https://beepb00p.xyz/assets/css/links.css'       rel='stylesheet'>
<link href='https://beepb00p.xyz/assets/css/htmlize.css'     rel='stylesheet'>
<link href='https://beepb00p.xyz/assets/css/org-default.css' rel='stylesheet'>
<link href='https://beepb00p.xyz/assets/css/org-extra.css'   rel='stylesheet'>
<style>
/* ugh. need to override default.css */
body {
  /* 3 rem just in case, otherwise overlaps sidebard sometimes for some reason */
  margin-left: calc(var(--sidebar-width) + 3rem);
}
/* eh. some links are way too long and break the reponsive view... I guess ok for now */
a {
  word-break: break-word;
}
</style>
"

        :html-postamble     nil))


;; TODO after intermediate, run santity check
