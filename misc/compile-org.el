;; disable paredit so it doesn't mess with formatting


;; require these straightaway so we don't need with-eval-after-load
(require 'org)
(require 'ox-html)

(require 's)
(require 'dash)


(defvar compileorg/throw-on-babel-errors t)
(defvar compileorg/output-file nil)
(defvar compileorg/output-format "html")

;; TODO allow defensive behaviour when error is propagated up?
(defun --throw-babel-error (oldfun &rest args)
  (let* ((code (buffer-string))
         (error-buffer (nth 3 args))
         (exit-code (apply oldfun args)))
    (if (or (not (numberp exit-code)) (> exit-code 0))
        (progn
          (message "************FAILED****************************")
          (message "%s" code)
          (message "%s" (with-current-buffer error-buffer (buffer-string)))
          (message "**********************************************")
          (error "failed to execute a code block!"))
      exit-code)))

(if compileorg/throw-on-babel-errors
    (advice-add #'org-babel--shell-command-on-region :around #'--throw-babel-error))


;; TODO ok, so maybe do not export CREATED property, but show it as a tooltip?
;; TODO hmm. find another way to configure these...
;; TODO use STRT?
(setq org-todo-keywords '((sequence "TODO" "STRT" "START" "DONE")))

(setq org-time-stamp-custom-formats
      '("[%Y-%m-%d]" . "[%Y-%m-%d %H:%M]"))
(setq org-display-custom-times 't)


;; TODO hmm, not sure if I should use INCLUDE for these instead?
;; I guess that is friendlier if other people try to use this
(setq org-export-global-macros '(("aside"    . "@@html:<aside>@@$0@@html:</aside>@@")
                                 ("anchor"   . "@@html:<a name='$0'></a>@@")
                                 ;; TODO use css instead?
                                 ("question" . "@@html:<span style='color:darkorange'><strong>$0</strong></span>@@")))
(setq org-export-with-author nil)


;; necessary so it doesn't prompt us
(setq org-confirm-babel-evaluate nil)

;; default references are some sort of hashes, which is very confusing when you are trying to track changes
;; TODO shit, quadratic time! perhaps start with max??
(defun org-export-deterministic-reference (references)
  (let ((new 0))
    (while (rassq new references)
      (setq new (+ new 1)))
    new))
(advice-add #'org-export-new-reference :override #'org-export-deterministic-reference)


;;; for internal links (<<target>>), generate #target anchor instead of random numbers
(defun org-export-get-reference-internal (orig-fun tg info &rest args)
  ;; fucking hell. that's got to be easier? pcase didn't work though
  ;; I can't believe that's the way to go in elisp. What if someone renames 'target', then it just breaks?
  ;; in other dynamic languages there is at least linting
  (let ((symb (symbol-name (car tg))))
       (cond ((string= symb "target") (org-element-property :value tg))
             ((string= symb "table")  (org-element-property :name tg))
             (t (apply orig-fun tg info args)))))
(advice-add #'org-export-get-reference :around #'org-export-get-reference-internal)
;;;

;; https://emacs.stackexchange.com/questions/24960/org-mode-reference-remote-table-from-a-different-file

; TODO give tags different colors depending on whether it actually exists or not?
;; (defun org-blog-tag-follow (path)) TODO ?

;; TODO asssert fmt == html?
(defun org-blog-tag-export (path desc fmt)
  "Link to one of my tags"
  (let ((href  (format "/tags.html#%s" path))
        (title (or desc (format "#%s" path)))
        (class (if (not desc) "class='post-tag'" "")))
       (format "<a %s href='%s'>%s</a>" class href title)))
(org-add-link-type "tag" nil 'org-blog-tag-export)

(defun org-blog-github-export (path desc fmt)
  "Link to a github repo"
  (let* ((path  (if (s-contains? "/" path) path (format "karlicoss/%s" path)))
         (href  (format "https://github.com/%s" path))
         (title (or desc path)))
        (format "<a href='%s'>%s</a>" href title)))
(org-add-link-type "gh"  nil 'org-blog-github-export)

(defun org-blog-github-topic-export (path desc fmt)
  "Link to a github topic search"
  (let* ((user  "karlicoss")
         (href  (concat "https://github.com/search?type=Repositories&q=user%3A" user "+++topic%3A" path))
         (title desc))
    (format "<a href='%s'>%s</a>" href title)))
(org-add-link-type "ght"  nil 'org-blog-github-topic-export)


(defun org-blog-sidenote (path desc fmt)
  ;; TODO ugh. can't nest link inside the sidenote content??
  ;; and writing that on elisp is gonna suck. really need python exporting backend...
  ;; (message (format "HELLOO  =================== %s" desc))
  (format "<aside class='sidenote'>%s</aside>" desc))
(org-add-link-type "sidenote" nil 'org-blog-sidenote)



;;; HTML specific export settings
;; https://github.com/gongzhitaao/orgcss#code-highlight
(setq org-html-htmlize-output-type 'css)


(add-to-list 'org-html-text-markup-alist '(verbatim . "<samp class='inline'>%s</samp>"))
(add-to-list 'org-html-text-markup-alist '(code     . "<code class='inline'>%s</code>"))


;;; see ox-html.el for original functions
;; by default uses <pre> and 'example' class (not configurable!)
(defun org-html-property-drawer (_property-drawer contents _info)
  (and (org-string-nw-p contents)
       (format "<div class='properties'>\n%s</div>" contents)))
(defun org-html-node-property (node-property _contents _info)
  (let ((key (org-element-property :key node-property))
        (val (org-element-property :value node-property)))
   (format "<div class='property %s'><span class='property-name'>%s:</span> <span class='property-value'>%s</span></div>"
           key
           key
           ; hack to display CREATED properties without the weekday in export
           (if (string= key "CREATED") ; meh.
               (format-time-string "[%Y-%m-%d %H:%M]" (org-read-date t t val))
               (val)))))
;;;
;; (format-time-string (org-time-stamp-format 'long 'inactive) org-log-note-effective-time)

;;; python specific stuff
(setq org-babel-python-command "python3")
;;; mypy stuff
;; you can remove it if you're not plainning on using it
;; treat mypy highlight as python. see org-html-fontify-code

;; without require,
;; org-babel-exp-results ignores it on line:
;;      (when (fboundp (intern (concat "org-babel-execute:" lang)))
;; TODO perhaps good idea at least to sugggest to emit a warning?
(require 'ob-python)

(add-to-list 'org-src-lang-modes '("mypy" . python))

(require 'subr-x) ; ugh, for string-join...
;; see https://github.com/karlicoss/dotfiles-emacs
(load-file "~/dotfiles-emacs/babel-mypy.el")
;;;


;; patch so noweb (i.e. <<code>>) references are stripped off correctly, otherwise you end up with wrong line numbering
;; the only difference is in (concat (org-babel-noweb-wrap) "\n")
(defun org-babel-exp-code (info type)
  "Return the original code block formatted for export."
  (setf (nth 1 info)
        (if (string= "strip-export" (cdr (assq :noweb (nth 2 info))))
            (replace-regexp-in-string
             (concat (org-babel-noweb-wrap) "\n") "" (nth 1 info))
          (if (org-babel-noweb-p (nth 2 info) :export)
              (org-babel-expand-noweb-references
               info org-babel-exp-reference-buffer)
            (nth 1 info))))
  (org-fill-template
   (if (eq type 'inline)
       org-babel-exp-inline-code-template
     org-babel-exp-code-template)
   `(("lang"  . ,(nth 0 info))
     ("body"  . ,(org-escape-code-in-string (nth 1 info)))
     ("switches" . ,(let ((f (nth 3 info)))
                      (and (org-string-nw-p f) (concat " " f))))
     ("flags" . ,(let ((f (assq :flags (nth 2 info))))
                   (and f (concat " " (cdr f)))))
     ,@(mapcar (lambda (pair)
                 (cons (substring (symbol-name (car pair)) 1)
                       (format "%S" (cdr pair))))
               (nth 2 info))
     ("name"  . ,(or (nth 4 info) "")))))
;; (setq org-babel-noweb-wrap-end ">>\n") doesn't work
;;


;; TODO not sure..
;; (with-eval-after-load 'python
;;             (setq python-indent-guess-indent-offset nil))


;; t is for BODY-ONLY
;; finally, export (it dumps output to the current buffer)
(pcase compileorg/output-format
  ("html" (org-html-export-as-html nil nil nil t))
  ("org"  (org-org-export-as-org   nil nil nil t)))

;; NOTE: python format pattern
;; write output to the target file
(write-file compileorg/output-file)


;; (with-current-buffer " *Org-Babel Error*") ;; TODO fucking hell..
;; (with-current-buffer org-babel-error-buffer-name
