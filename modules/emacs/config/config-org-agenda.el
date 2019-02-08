;;                                                    -*- emacs-lisp -*-
;;
;; Copyright (C) 2017, 2019 by Peder Stray <peder@ninja.no>
;;

(with-demoted-errors
    "Error: init-org-agenda: %s"

  (message "++ Setting up org-agenda")

  (eval-after-load 'org
    '(progn
       (message "+++ Configuring org-agenda for org")
       ;; variables in org
       (setq

	;; where to store the list, relative to org-directory
	org-agenda-files
	(mapcar (lambda (file) (expand-file-name file org-directory))
		(list
		 "agenda"
     "~/Dropbox/org/prv_todo.org"
		 "~/Dropbox/org/agenda"
		 ))

	;; just skip unreadable files
	org-agenda-skip-unavailable-files		t
	)

       ;; Remove these since org-agenda-files above is a list of dirs
       (org-defkey org-mode-map "\C-c[" nil)
       (org-defkey org-mode-map "\C-c]" nil)
       ))

  ;; variables in org-agenda
  (setq

   ;; use colors
   org-agenda-with-colors			t

   ;;org-agenda-custom-commands

   ;; don't include archived items, or comments
   org-agenda-archives-mode			nil
   org-agenda-skip-comment-trees		t
   ;; include sublevels of a TODO
   org-agenda-todo-list-sublevels		t

   org-agenda-todo-ignore-with-date		nil
   org-agenda-todo-ignore-timestamp		nil
   org-agenda-todo-ignore-scheduledr		nil
   org-agenda-todo-ignore-deadlines		nil

   ;; skip TODOS that done
   org-agenda-skip-scheduled-if-done		t
   org-agenda-skip-deadline-if-done		t
   org-agenda-skip-timestamp-if-done		t

   ;; include both scheduled or timestamp and deadline
   org-agenda-skip-scheduled-if-deadline-is-shown nil
   org-agenda-skip-timestamp-if-deadline-is-shown nil

   ;; semihide todos that can't be done yet
   org-agenda-dim-blocked-tasks			t

   ;; How to display the agenda
   org-agenda-window-setup			'reorganize-frame
   org-agenda-restore-windows-after-quit	t

   ;; 14 days is good ;)
   org-agenda-span				'fortnight

   ;; the only sane choice
   org-agenda-start-on-weekday			1

   ;; only show dates with items, not empty ones
   org-agenda-show-all-dates			nil

   ;; show times as 09:30, not 9:30
   org-agenda-time-leading-zero			t

   ;; skip diary entries in the agenda
   org-agenda-include-diary			nil

   ;; include entries within deadline period
   org-agenda-include-deadlines			t

   ;; include repeated entries
   org-agenda-show-future-repeats		t
   org-agenda-prefer-last-repeat		nil

   ;;org-agenda-clock-consistency-checks

   ;; show first line of notes
   org-agenda-log-mode-add-notes		t

   ;; Show time-grid, and current time
   org-agenda-use-time-grid			t
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     "......"
     "----------------")
   org-agenda-show-current-time-in-grid		t
   org-agenda-current-time-string
   "now - - - - - - - - - - - - - - - - - - - - - - - - -"

   ;; select how agenda views sort
   org-agenda-sorting-strategy
   '((agenda	habit-down time-up priority-down category-keep)
     (todo	category-keep priority-down)
     (tags	category-keep priority-down)
     (search	category-keep))

   ;; untimed entries sort later than timed
   org-sort-agenda-notime-is-late		t

   org-sort-agenda-noeffort-is-high		t

   ;; How the agenda looks...
   org-agenda-prefix-format
   '(;(agenda  . " %i%(ps:align 3) %-12:c%?-12t%?-12:s")
     (agenda  . " %1i %-12:c%?-12t%?-12:s")
     (todo    . " %1i %-12:c")
     (tags    . " %1i %-12:c")
     (search  . " %1i %-12:c"))
   org-agenda-todo-keyword-format		"%-1s"
   org-agenda-scheduled-leaders
   '("Scheduled" "Sched.%3dx")
   org-agenda-inactive-leader			"["
   org-agenda-deadline-leaders
   '("Deadline" "In %3d d." "%d d. ago")

   ;; show tags flush right
   org-agenda-tags-column			'auto

   ;;org-agenda-fontify-priorities		; remove the annoying underline
   ;;'((?A (:bold t)))

   org-agenda-category-icon-alist
   '(
     ;;  f1fd |   | fa  | birthday_cake |
     ;;  f5e8 |   | mdi | cake
     ;;  f5e9 |   | mdi | cake_layered
     ;;  f5ea |   | mdi | cake_variant
     ;; 1F382 | 🎂 |     | BIRTHDAY CAKE
     ("Birthday" (""))
     ;;   U+f186 moon_o (nerd font)
     ;; 🌝 U+1F31D FULL MOON WITH FACE
     ("Lunar" (""))
     ;; ➜ U+279C HEAVY ROUND-TIPPED RIGHT-WARDS ARROW
     ("Refile" ("➜"))
     ;; 🗹 U+1F5F9 BALLOT BOX WITH BOLD CHECK
     ;;  f00c fa_check
     ;;  f046 fa_check_square_o
     ("Todo" (""))
     )

   ;; remove agenad buffer, not bury
   org-agenda-sticky				nil

   )

  (defun ps:align (hpos)
    "Return a space with align-to display property"
    (propertize " " 'display (list 'space :align-to hpos)))

  (eval-after-load 'org-agenda
    '(progn
       (message "+++ Configuring org-agenda")

       (setq
	org-agenda-custom-commands
	(append
	 org-agenda-custom-commands
	 '(("w" "Work TODO" tags-todo "work"
	    ((org-agenda-sorting-strategy '(priority-down))))
	   ("W" "Work Agenda" agenda "+work"
	    ((org-agenda-skip-function
	      '(org-agenda-skip-entry-if 'notregexp ":work:"))))))
	)
       ))

  )

(provide 'config-org-agenda)
