;;                                                    -*- emacs-lisp -*-
;;
;; Copyright (C) 2011-2019 by Peder Stray <peder@ninja.no>
;;

(with-demoted-errors
    "Error: init-org-protocol: %s"

  (message "++ Setting up org-protocol")

  (require 'org-protocol)

  (eval-after-load 'org-protocol
    '(progn
       (message "++ Configuring org-protocol")

       ;; (setq org-protocol-default-template-key nil)

       ))
  )

(provide 'config-org-protocol)
