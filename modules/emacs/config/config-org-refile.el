;;                                                    -*- emacs-lisp -*-
;;
;; Copyright (C) 2019 by Peder Stray <peder@continuity>
;;

(with-demoted-errors
    "Error: init-org-refile: %s"

  (message "++ Setting up org-refile")

  (setq
   org-refile-use-outline-path	'file
   org-refile-targets		'((nil :maxlevel . 9)
				  (org-agenda-files :maxlevel . 9))

   )
  )

(provide 'config-org-refile)
