;; usage: emacs --batch -l export.el test.org /tmp/test.org

(require 'cl-lib)
(require 'org)
(require 'ox-org)

(setq source (nth    0 command-line-args-left))
(setq target (nth    1 command-line-args-left))
(setq rpaths (nthcdr 2 command-line-args-left))

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


(cl-loop for rpath in rpaths
         do (progn
              (message (format "exporting %s" rpath))
              (cl-assert      (file-exists-p (expand-file-name rpath source)))
              ;; (message (format "checking %s" (expand-file-name rpath target)))
              (cl-assert (not (file-exists-p (expand-file-name rpath target))))
              (find-file                     (expand-file-name rpath source))
              (org-export-to-file 'org       (expand-file-name rpath target))
))
