;;; de-org.el
;; Org Config for Denver Ellis
 
;; Copyright (c) 2015 Denver S. Ellis

;;Author: Denver Ellis <ellis.denver@yahoo.com>
;;Maintained By: Denver Ellis <ellis.denver@yahoo.com>
;;Created: 05 Mar 2019
;;Last Updated: 05 Mar 2019

;; Keywords: configuration
;; Homepage: https://github.com/....

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.


(require 'org-agenda)
(require 'org-clock)
(require 'org-habit)
(require 'org-id)
(require 'org)

;;General Settings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-startup-indented t)
(add-hook 'org-mode-hook #'visual-line-mode)
(setq org-directory "~/org")
(setq org-default-notes-file "~/org/notes.org")
(defvar org-default-journal-file "~/org/journal.org")
(setq org-agenda-files (list "~/org"
			     "~/org/archive"
			     "~/org/work.org"
			     "~/org/school.org"
			     "~/org/home.org"))

;; === Display ===
(setq org-cycle-separator-lines 0)
(setq org-tags-column 80)
(setq org-agenda-tags-column org-tags-column)
(setq org-agenda-sticky t)

;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%50ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %16TIMESTAMP_IA")


;; === bh/helper-functions ===
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))


;; === Custome State Keywords ===
;; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key 'expert)

;; Include the todo keywords
(setq org-fast-tag-selection-include-todo t)

(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	(sequence "WAITING(w@/!)" "INACTIVE(i)" "|" "CANCELLED(c@/!)" "MEETING")))

;; Custom colors for keywords
(setq org-todo-keword-faces
      '(("TODO" :foreground "red" :weight bold)
	("NEXT" :foreground "blue" :weight bold)
	("DONE" :foreground "forest green" :weight bold)
	("WAITING" :foreground "orange" :weight bold)
	("INACTIVE" :foreground "magenta" :weight bold)
	("CANCELLED" :foreground "medium purple" :weight bold)
	("MEETING" :foreground "medium sea green" :weight bold)))


;;Auto-update tags whenever the state is changed
(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
	("WAITING" ("WAITING" . t))
	("INACTIVE" ("WAITING") ("INACTIVE" . t))
	(done ("WAITING") ("INACTIVE"))
	("TODO" ("WAITING") ("CANCELLED") ("INACTIVE"))
	("NEXT" ("WAITING") ("CANCELLED") ("INACTIVE"))
	("DONE" ("WAITING") ("CANCELLED") ("INACTIVE"))))


(defun de/mark-next-done-parent-tasks-todo ()
  "Visit each parent task and change NEXT (or DONE) states to TODO."
  ;; Don't change the value if new state is "DONE"
  (let ((mystate (or (and (fboundp 'org-state)
                          (member state
				  (list "NEXT" "TODO")))
                     (member (nth 2 (org-heading-components))
			     (list "NEXT" "TODO")))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT" "DONE"))
            (org-todo "TODO")))))))
(add-hook 'org-after-todo-state-change-hook 'de/mark-next-done-parent-tasks-todo 'append)


;; === Capture Mode Settings ===
;; Define the custum capture templates
(defvar org-capture-templates
       '(("t" "todo" entry (file org-default-notes-file)
	  "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
	 ("b" "Blank" entry (file org-default-notes-file)
	  "* %?\n%u")
	 ("m" "Meeting" entry (file org-default-notes-file)
	  "* MEETING with %? :MEETING:\n" :clock-in t :clock-resume t)
	 ("d" "Diary" entry (file+datetree "~/org/diary.org")
	  "* %?\n%U\n" :clock-in t :clock-resume t)
	 ("D" "Daily Log" entry (file "~/org/daily-log.org")
	  "* %u %?\n#+BEGIN: gjs-daily-clocktable :maxlevel 4 :date \"%u\" :link t :compact t \n#+END:\n\n*Summary*: \n\n*Problem*: \n\n*Insight*: \n\n*Tomorrow*: " :clock-in t :clock-resume t)
	 ("i" "Idea" entry (file org-default-notes-file)
	  "* %? :IDEA: \n%u" :clock-in t :clock-resume t)
	 ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
	  "** NEXT %? \nDEADLINE: %t")))


;; === Archive ===
(setq org-archive-location "archive/%s_archive::")
(defvar org-archive-file-header-format "#+FILETAGS: ARCHIVE\nArchived entries from file %s\n")


;; === Habits ===
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 48)
(setq org-habit-show-habits-only-for-today t)


;; === Clocking Functions ===
;; If not a project, clocking-in changes TODO to NEXT
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (not (bh/is-project-p)))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))


(add-hook 'org-mode-hook
    (lambda ()
      (define-key org-mode-map (kbd "C-c C-.") 'org-time-stamp-inactive)))


;; Also ensure that NEXT projects are switched to TODO when clocking in
(add-hook 'org-clock-in-hook 'gs/mark-next-done-parent-tasks-todo 'append)



;; === Agenda ===
;; Dim blocked tasks (and other settings)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-inhibit-startup nil)
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view (disabled)
(setq org-agenda-compact-blocks nil)

;; Set the times to display in the time grid
(setq org-agenda-time-grid
      (quote
       ((daily today remove-match)
        (800 1200 1600 2000)
        "......" "----------------")))


;; === gs/helper-functions ===
(defun gs/select-with-tag-function (select-fun-p)
  (save-restriction
    (widen)
    (let ((next-headline
	   (save-excursion (or (outline-next-heading)
			       (point-max)))))
      (if (funcall select-fun-p) nil next-headline))))


(defun gs/select-projects ()
  "Selects tasks which are project headers"
  (gs/select-with-tag-function #'bh/is-project-p))


(defun gs/select-project-tasks ()
  "Skips tags which belong to projects (and is not a project itself)"
  (gs/select-with-tag-function
   #'(lambda () (and
		 (not (bh/is-project-p))
		 (bh/is-project-subtree-p)))))


(defun gs/select-standalone-tasks ()
  "Skips tags which belong to projects. Is neither a project, nor does it blong to a project"
  (gs/select-with-tag-function
   #'(lambda () (and
		 (not (bh/is-project-p))
		 (not (bh/is-project-subtree-p))))))


(defun gs/select-projects-and-standalone-tasks ()
  "Skips tags which are not projects"
  (gs/select-with-tag-function
   #'(lambda () (or
		 (bh/is-project-p)
		 (bh/is-project-subtree-p)))))


(defun gs/org-agenda-project-warning ()
  "Is a project stuck or waiting. If the project is not stuck,
show nothing. However, if it is stuck and waiting on something,
show this warning instead."
  (if (gs/org-agenda-project-is-stuck)
    (if (gs/org-agenda-project-is-waiting) " !W" " !S") ""))


(defun gs/org-agenda-project-is-stuck ()
  "Is a project stuck"
  (if (bh/is-project-p) ; first, check that it's a project
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
	     (has-next))
	(save-excursion
	  (forward-line 1)
	  (while (and (not has-next)
		      (< (point) subtree-end)
		      (re-search-forward "^\\*+ NEXT " subtree-end t))
	    (unless (member "WAITING" (org-get-tags-at))
	      (setq has-next t))))
	(if has-next nil t)) ; signify that this project is stuck
    nil)) ; if it's not a project, return an empty string


(defun gs/org-agenda-project-is-waiting ()
  "Is a project stuck"
  (if (bh/is-project-p) ; first, check that it's a project
      (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
	(save-excursion
	  (re-search-forward "^\\*+ WAITING" subtree-end t)))
    nil)) ; if it's not a project, return an empty string


;; Some helper functions for agenda views
(defun gs/org-agenda-prefix-string ()
  "Format"
  (let ((path (org-format-outline-path (org-get-outline-path))) ; "breadcrumb" path
	(stuck (gs/org-agenda-project-warning))) ; warning for stuck projects
       (if (> (length path) 0)
	   (concat stuck ; add stuck warning
		   " [" path "]") ; add "breadcrumb"
	 stuck)))


(defun gs/org-agenda-add-location-string ()
  "Gets the value of the LOCATION property"
  (let ((loc (org-entry-get (point) "LOCATION")))
    (if (> (length loc) 0)
	(concat "{" loc "} ")
      "")))


;; Variables for ignoring tasks with deadlines
(defvar gs/hide-deadline-next-tasks t)
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-deadline-warning-days 10)


;; === Custom Agenda Commands (gs) ===
;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("h" "Habits" agenda "STYLE=\"habit\""
	 ((org-agenda-overriding-header "Habits")
	  (org-agenda-sorting-strategy
	   '(todo-state-down effort-up category-keep))))
	(" " "Export Schedule" (
				(agenda "" ((org-agenda-overriding-header "Today's Schedule:")
					    (org-agenda-span 'day)
					    (org-agenda-ndays 1)
					    (org-agenda-start-on-weekday nil)
					    (org-agenda-start-day "+0d")
					    (org-agenda-todo-ignore-deadlines nil)))
				(tags "REFILE-ARCHIVE-REFILE=\"nil\""
				      ((org-agenda-overriding-header "Tasks to Refile:")
				       (org-tags-match-list-sublevels nil)))
				(tags-todo "-INACTIVE-CANCELLED-ARCHIVE/!NEXT"
					   ((org-agenda-overriding-header "Next Tasks:")
					    ))
				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILEr/!"
					   ((org-agenda-overriding-header "Active Projects:")
					    (org-agenda-skip-function 'gs/select-projects)))
				(tags "ENDOFAGENDA"
				      ((org-agenda-overriding-header "End of Agenda")
				       (org-tags-match-list-sublevels nil)))
				)
	 ((org-agenda-start-with-log-mode t)
	  (org-agenda-log-mode-items '(clock))
	  (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
				      (timeline . "  % s")
				      (todo . "  %-12:c %(gs/org-agenda-prefix-string) ")
				      (tags . "  %-12:c %(gs/org-agenda-prefix-string) ")
				      (search . "  %i %-12:c")))
	  (org-agenda-todo-ignore-deadlines 'near)
	  (org-agenda-todo-ignore-scheduled t)))
	("b" "Agenda Review" (
				(tags "REFILE-ARCHIVE-REFILE=\"nil\""
				      ((org-agenda-overriding-header "Tasks to Refile:")
				       (org-tags-match-list-sublevels nil)))
				(tags-todo "-INACTIVE-CANCELLED-ARCHIVE/!NEXT"
					   ((org-agenda-overriding-header "Next Tasks:")
					    ))
				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVEr/!"
					   ((org-agenda-overriding-header "Active Projects:")
					    (org-agenda-skip-function 'gs/select-projects)))
				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVE-STYLE=\"habit\"/!-NEXT"
					   ((org-agenda-overriding-header "Standalone Tasks:")
					    (org-agenda-skip-function 'gs/select-standalone-tasks)))
				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVE/!-NEXT"
					   ((org-agenda-overriding-header "Remaining Project Tasks:")
					    (org-agenda-skip-function 'gs/select-project-tasks)))
				(tags-todo "-INACTIVE-CANCELLED-ARCHIVE/!WAITING"
					   ((org-agenda-overriding-header "Waiting Tasks:")
					    ))
				(tags "INACTIVE-ARCHIVE-CANCELLED"
				      ((org-agenda-overriding-header "Inactive Projects and Tasks")
				       (org-tags-match-list-sublevels nil)))
				(tags "ENDOFAGENDA"
				      ((org-agenda-overriding-header "End of Agenda")
				       (org-tags-match-list-sublevels nil)))
				)
	 ((org-agenda-start-with-log-mode t)
	  (org-agenda-log-mode-items '(clock))
	  (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
				      (timeline . "  % s")
				      (todo . "  %-12:c %(gs/org-agenda-prefix-string) ")
				      (tags . "  %-12:c %(gs/org-agenda-prefix-string) ")
				      (search . "  %i %-12:c")))
	  (org-agenda-todo-ignore-deadlines 'near)
	  (org-agenda-todo-ignore-scheduled t)))

	("O" "Export Schedule (old)" ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
					    (org-agenda-span 'day)
					    (org-agenda-ndays 1)
					    (org-agenda-start-on-weekday nil)
					    (org-agenda-start-day "+0d")
					    (org-agenda-todo-ignore-deadlines nil)))
				(tags-todo "-INACTIVE-CANCELLED-ARCHIVE/!NEXT"
					   ((org-agenda-overriding-header "Next Tasks:")
					    ))
				(tags "REFILE-ARCHIVE-REFILE=\"nil\""
				      ((org-agenda-overriding-header "Tasks to Refile:")
				       (org-tags-match-list-sublevels nil)))
				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVEr/!"
					   ((org-agenda-overriding-header "Active Projects:")
					    (org-agenda-skip-function 'gs/select-projects)))
				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVE-STYLE=\"habit\"/!-NEXT"
					   ((org-agenda-overriding-header "Standalone Tasks:")
					    (org-agenda-skip-function 'gs/select-standalone-tasks)))
				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVE/!-NEXT"
					   ((org-agenda-overriding-header "Remaining Project Tasks:")
					    (org-agenda-skip-function 'gs/select-project-tasks)))
				(tags "INACTIVE-ARCHIVE-CANCELLED"
				      ((org-agenda-overriding-header "Inactive Projects and Tasks")
				       (org-tags-match-list-sublevels nil)))
				(tags "ENDOFAGENDA"
				      ((org-agenda-overriding-header "End of Agenda")
				       (org-tags-match-list-sublevels nil))))
	 ((org-agenda-start-with-log-mode t)
	  (org-agenda-log-mode-items '(clock))
	  (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
				      (timeline . "  % s")
				      (todo . "  %-12:c %(gs/org-agenda-prefix-string) ")
				      (tags . "  %-12:c %(gs/org-agenda-prefix-string) ")
				      (search . "  %i %-12:c")))
	  (org-agenda-todo-ignore-deadlines 'near)
	  (org-agenda-todo-ignore-scheduled t)))
	("X" "Agenda" ((agenda "") (alltodo))
	 ((org-agenda-ndays 10)
	  (org-agenda-start-on-weekday nil)
	  (org-agenda-start-day "-1d")
	  (org-agenda-start-with-log-mode t)
	  (org-agenda-log-mode-items '(closed clock state)))
	 )))


;; === Agenda Post-processing ===
;; Highlight the "!!" for stuck projects (for emphasis)
(defun gs/org-agenda-project-highlight-warning ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "!W" nil t)
      (progn
	(add-face-text-property
	 (match-beginning 0) (match-end 0)
	 '(bold :foreground "orange"))
	))
    (goto-char (point-min))
    (while (re-search-forward "!S" nil t)
      (progn
	(add-face-text-property
	 (match-beginning 0) (match-end 0)
	 '(bold :foreground "white" :background "red"))
	))
    ))
(add-hook 'org-agenda-finalize-hook 'gs/org-agenda-project-highlight-warning)


;; Remove empty agenda blocks
(defun gs/remove-agenda-regions ()
  (save-excursion
    (goto-char (point-min))
    (let ((region-large t))
      (while (and (< (point) (point-max)) region-large)
	(set-mark (point))
	(gs/org-agenda-next-section)
	(if (< (- (region-end) (region-beginning)) 5) (setq region-large nil)
	  (if (< (count-lines (region-beginning) (region-end)) 4)
	      (delete-region (region-beginning) (region-end)))
	  )))))
(add-hook 'org-agenda-finalize-hook 'gs/remove-agenda-regions)


;; === Clocktable ===
(defun gjs-org-clocktable-filter-empty-tables (ipos tables params)
  "Removes all empty tables before printing the clocktable"
  (org-clocktable-write-default ipos
				(seq-filter
				 (lambda (tbl)
				   (not (null (nth 2 tbl))))
				 tables)
				params)
  )

(defun org-dblock-write:gjs-daily-clocktable (params)
  "Custom clocktable command for my daily log"
  (let ((local-params params)
	(date-str (substring
		   (plist-get params ':date)
		   1 11))
	)
    (plist-put params ':block date-str)
    (plist-put params ':formatter 'gjs-org-clocktable-filter-empty-tables)
    (plist-put params ':scope 'agenda)
    (org-dblock-write:clocktable params)
    )
  )


(provide 'de-org)
;;; de-org ends here
