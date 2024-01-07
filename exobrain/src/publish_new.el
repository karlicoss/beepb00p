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


(defun pp-org (thing)
  ;; FIXME shit. it modifies the thing...
  (org-element-put-property thing :parent nil) ;; otherwise too spammy
  (pp thing))

;; see https://github.com/emacsmirror/advice-patch
(require 'advice-patch)
;; whoa advice-patch is really nice

;; no clue why default class is "example" :shrug: (using same format as in blog)
(advice-patch 'org-html-property-drawer
              "<div class='properties'>\n%s</div>"
              "<pre class=\"example\">\n%s</pre>")

;; same, the default format for html property export is quite dull
(advice-patch 'org-html-node-property
              ;; borrowed from blog (a bit modified though)
              "<div class='property' data-property-name='%1$s'><span class='property-name'>%1$s</span>: <span class='property-value'>%2$s</span></div>"
              "%s:%s")

;; super annoying (is this nbsp?), should be via css
(advice-patch 'org-html-format-headline-default-function
              ;;
              " "
              "&#xa0;&#xa0;&#xa0;")

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

;; fuck. default org-mode ids are non-deterministic (and even change inbetween emacs invocations)
;; https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors looks really good
;; it worked, but then I moved some code around and it stopped for some reason...
;; too exhausted to debug it, so will use it late
;; sometimes I fucking hate emacs.
;; md5: nice that it has fixed length, but not very reversible
;; base64: might be arbirary long?
(defun exobrain/org-export-get-reference (datum info)
  (let* ((title (org-element-property :raw-value datum))
         (begin (org-element-property :begin     datum))
         (end   (org-element-property :end       datum))
         (_     (cl-assert title))
         (res   (replace-regexp-in-string
                 ;; remove days of week, since the date is sometimes part of title...
                 " \\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\) "
                 ""
                 title))
         (res   (s-downcase res))
         (res   (replace-regexp-in-string "[^[:ascii:]]" "" (s-downcase res))) ;; remove all non-ascii.. it's too problematic to handle
         (res   (replace-regexp-in-string
                 ;; drop all vowels, can read without it...
                 "[aeoiu]\\|[^[:alpha:]]\\|http\\|https"
                 ""
                 res))
                  ;; TODO and then sample characters? not sure
         (res (if (<= (length res) 50)
                  res
                ;; if it's too long, only keep head & tail
                (concat (s-left 25 res) (s-right 25 res))))
         (res (if (> (length res) 0)
                  res
                ;; not much we can do in this case
                ;; could use heading number maybe? dunno
                nil)))
    res))
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

;; annoying, but seems the easiest is to simply override...
(defun exobrain/org-html--tags (tags info)
  (let* ((inhtags  (--filter (s-starts-with? exobrain/inh-prefix it) tags))
         (selftags (-difference tags inhtags))
         (inhtags  (--map    (s-chop-prefix exobrain/inh-prefix it) inhtags)))
    ;; ugh. if you type ::tag: it parses it as empty tag?? must be a bug in org?
    (cl-assert (--none? (s-blank? it) (-concat inhtags selftags)))
    ;; not sure why everythin wrapped in 'tag' in ox-html (instead of, say 'tags')
    (format "<span class=\"tag\">%s</span>"
            (s-join "" (--map (format "<span class=\"%1$s%2$s\">%1$s</span>" (nth 0 it) (nth 1 it))
                              ;; not so sure about the order.. but I guess it's nice when all visible tags are aligned
                              (-concat (--map (list it " tag-inherited") inhtags)
                                       (--map (list it " tag-self"     ) selftags)))))))


(advice-add #'org-html--tags  :override #'exobrain/org-html--tags)


(defun exobrain/org-html-inner-template-reorder-toc (contents info)
  "Pretty annoying when the TOC is very long and it shows up on the very top after the introduction in root block"
  ;; also tried with flexbox and order:, but the problem it doesn't support collapsing margins
  (let* ((footnote (org-html-footnote-section info))
         (depth (plist-get info :with-toc))
         (toc (when depth (org-html-toc depth info)))
         (contents (if (not toc) contents
                        ;; find first actual outline and reorder TOC before it
                        ;; for fucks sake, s-slice-at is recursive and hits elisp recursion limit... https://github.com/magnars/s.el/pull/125
                        (let* ((slices (s-split-up-to "<div .* class=.outline-2" contents 1)) ;; split no more than once
                               (before (nth 0 slices)) ;; should always be present
                               (after  (s-chop-prefix before contents)))
                          (concat before toc after)))))
    (concat contents footnote)))
(advice-add #'org-html-inner-template :override #'exobrain/org-html-inner-template-reorder-toc)


;; fucking hell, it's defsubst https://www.gnu.org/software/emacs/manual/html_node/elisp/Inline-Functions.html
;; that's why advice doesn't work
;; I hate elisp.
;; (advice-add #' org-element-property :around #'exobrain/md-org-element-property)

;; (defun exobrain/org-md-headline (orig headline contents info)
;;   (cl-letf (((symbol-function 'org-element-property) 'exobrain/md-org-element-property))
;;     (funcall orig headline contents info)))

;; TODO done keywords should be marked separately..
(setq org-todo-keywords '((sequence "TODO" "NEXT" "STRT" "START" "WIP" "WAIT" "|" "CNCL" "CANCEL" "DONE")))
;; TODO share with rest of the system..
(setq exobrain/state-keywords
      '(("TODO"   . "todo")
        ("NEXT"   . "todo")
        ("START"  . "todo") ;; TODO start?
        ("STRT"   . "todo") ;; TODO start?
        ("WAIT"   . "todo")
        ("DONE"   . "done")
        ("CNCL"   . "cancel")
        ("CANCEL" . "cancel")))

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
      (let* ((filetags (plist-get info :filetags))
             (tagss    (exobrain/org-html--tags filetags info)))
        (format "<div class='filetags'>%s</div>" tagss))
    nil))

(setq exobrain/project-org2html
      `("exobrain-html"
        :base-directory ,exobrain/public-dir
        :base-extension "org"
        :publishing-directory ,exobrain/html-dir
        :publishing-function org-html-publish-to-html

        ;; todo ugh. seems that it's dumping sitemap to the source dir, and it can't be changed?
        :auto-sitemap t

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
