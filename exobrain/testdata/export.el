;; usage: emacs --batch -l export.el test.org /tmp/test.org

(require 'cl-lib)
(require 'org)
(require 'ox-org)

(setq source (nth 0 command-line-args-left))
(setq dest   (nth 1 command-line-args-left))

(cl-assert (file-exists-p source))
(cl-assert (not (file-exists-p dest)))  ;; not sure about this?

;; meh
;; (setq org-export-options-alist
;;   (append org-export-options-alist '(
;;     (:with-priority      t)
;;     ;; todo eh, not sure if I need anything else?
;;     ;; (:with-properties    '("ID" "CUSTOM_ID" "CREATED" "PUBLISHED"))
;;     ;; (:with-tags          t)
;;     ;; shit. only impacts isolated timestamps... (i.e. not next to TODO keywords etc)
;;     ;; https://github.com/bzg/org-mode/blob/817c0c81e8f6d1dc387956c8c5b026ced62c157c/lisp/ox.el#L1896
;;     ;; or maybe doesn't impact anything at all?? has no effect if set to t, same in html export
;;     ;; (:with-timestamps    t)
;;     ;; (:with-todo-keywords t)
;;     (:time-stamp-file    nil)
;; )))

(setq org-export-exclude-tags '("noexport" "hide"))

(setq org-export-with-priority   t)
(setq org-export-time-stamp-file nil)
(setq org-export-with-properties '("ID" "CUSTOM_ID" "CREATED" "PUBLISHED"))
;; (add-to-list 'org-export-options-alist '(:time-stamp-file    nil))
;; (add-to-list 'org-export-options-alist '(:with-priority      t))


;; TODO not sure about that..
(setq org-confirm-babel-evaluate nil)


;; FIXME get rid of this... without it it's adding an extra space between the timestamp and the following text??
;; TODO fuck. here as well, timestamps are only translated if they are not within the heading???
(defun exobrain/override-org-timestamp-translate (timestamp &optional boundary)
  "sets custom format to all my timestamps (strips off time, it's just too spammy)"
  (let ((res (org-timestamp-format timestamp "[%Y-%m-%d]")))
    (if boundary
        (error "wtf if boundary?? %s %s" timestamp boundary)
      res)))
(advice-add #'org-timestamp-translate :override #'exobrain/override-org-timestamp-translate)
;;


(find-file source)
(org-export-to-file 'org dest)
