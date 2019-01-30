;;                                                    -*- emacs-lisp -*-
;;
;; Copyright (C) 2019 by Peder Stray <peder@wintermute>
;;

(with-demoted-errors
    "Error: init-lunar: %s"

  ;;(require 'init-org-agenda)

  (message "++ Setting up lunar")

  (setq lunar-phase-names
	'(
	  ;; "🌑 New Moon"
	  ;; "🌒 Waxing Crescent"
	  ;; "🌓 First Quarter Moon" ;; ☽
	  ;; "🌔 Waxing Gibbous Moon"
	  ;; "🌕 Full Moon"
	  ;; "🌖 Waning Gibbous Moon"
	  ;; "🌗 Last Quarter Moon" ;; ☾
	  ;; "🌘 Waning Crescent Moon"
	  "🌚︎ New Moon"
	  "🌛︎ First Quarter Moon"
	  "🌝︎ Full Moon"
	  "🌜︎ Last Quarter Moon"
	  ))

  (eval-after-load 'org-agenda
    '(require 'lunar))

  (eval-after-load 'lunar
    '(progn
       (message "++ Configuring lunar")

       (require 'cl-lib)

       (defun ps:org-agenda-lunar-phases ()
	 "Show lunar phases in agenda buffer"
	 (let* ((phase-list (lunar-phase-list (nth 0 date) (nth 2 date)))
		(phase (cl-find-if (lambda (phase) (equal (car phase) date))
				   phase-list)))
	   (when phase
	     (setq ret (concat (lunar-phase-name (nth 2 phase)) " "
			       (substring (nth 1 phase) 0 5))))))

       ))
  )

(provide 'config-org-lunar)
