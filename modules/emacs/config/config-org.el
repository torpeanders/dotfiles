(use-package org
  :ensure org-plus-contrib
  :pin org
  :defer t
  :config
  (setq org-src-fontify-natively t)
  (setq
   org-outline-path-complete-in-steps nil
   org-goto-interface 'outline-path-completion

   org-src-fontify-natively t
                                        ; clock
   org-clock-into-drawer  "CLOCK"
   org-clock-out-when-done t
   org-clock-in-switch-to-state nil
                                        ; log
   org-log-note-clock-out t
   org-duration-format (quote h:mm)
                                        ; time
   org-time-clocksum-use-fractional t
   org-todo-keywords
   '((sequence "TODO(t)" "INPROGRESS(i@)" "|" "DONE(f@)" "DELEGATED(d@)" "CANCELLED(c@)"))
   org-todo-keyword-faces
   '(("TODO" . org-warning)
     ("DONE" . (:foreground "green" :weight bold))
     ("DELEGATED" . (:foreground "yellow" :weight bold))
     ("CANCELLED" . (:foreground "red" :weight bold))
     ))
  ;; Location of (most) my org files
  (setq org-directory "~/org")
  ;; Don't split line when creating new org heading with <M-return>
  (setq org-M-RET-may-split-line '((item . nil)))
  ;; hydra - headings
  (key-chord-define-global "OM"
                           (defhydra hydra-org (:color red :columns 3)
                             "Org Mode Movements"
                             ("n" outline-next-visible-heading "next heading")
                             ("p" outline-previous-visible-heading "prev heading")
                             ("N" org-forward-heading-same-level "next heading at same level")
                             ("P" org-backward-heading-same-level "prev heading at same level")
                             ("u" outline-up-heading "up heading")
                             ("g" org-goto "goto" :exit t)))
  )

(require 'config-org-agenda)
(require 'config-org-capture)
(require 'config-org-contacts)
(require 'config-org-protocol)
(require 'config-org-refile)
(require 'config-org-lunar)

(provide 'config-org)
