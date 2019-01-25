;;                                                    -*- emacs-lisp -*-
;;
;; Copyright (C) 2018, 2019 by Peder Stray <peder@ninja.no>
;;

(with-demoted-errors
    "Error: init-org-contacts: %s"

  (message "+++ Setting up org-contacts")

  (setq
   ;; org-contacts-birthday-format	"Fødselsdag: %l (%y år)"
   )

  (eval-after-load 'org-agenda
    '(progn
       (require 'org-contacts)
       ;; no setup needed, just loading
       ))

  (eval-after-load 'org-capture
    '(progn
       (require 'org-contacts)
       (message "+++ Configuring org-contacts for org-capture")
       (add-to-list 'org-capture-templates
		    `("c" "Contacts" entry (file "~/Dropbox/org/agenda/contacts.org")
		      ,(concat "* %(org-contacts-template-name)\n"
			       "  :PROPERTIES:\n"
			       "  :" org-contacts-email-property ": %(org-contacts-template-email)\n"
			       "  :" org-contacts-tel-property   ": \n"
			       "  :" org-contacts-address-property ": \n"
			       "  :" org-contacts-birthday-property ": %^{Birthday: yyyy-mm-dd}\n"
			       "  :" org-contacts-note-property ": \n"
			       "  :" org-contacts-alias-property ": \n"
			       "  :" org-contacts-icon-property ": \n"
			       "  :" org-contacts-nickname-property ": \n"
			       "  :END:\n")))

       (with-no-warnings (defvar date)) ;; unprefixed, from calendar.el
       (defun ps:org-contacts-anniversaries (&optional field format format0)
  "Compute FIELD anniversary for each contact, returning FORMAT.
Default FIELD value is \"BIRTHDAY\".

Format is a string matching the following format specification:

  %h - Heading name
  %l - Link to the heading
  %y - Number of year
  %Y - Number of year (ordinal)"
  (let ((calendar-date-style 'american)
        (entry ""))
    (unless format (setq format org-contacts-birthday-format))
    (cl-loop for contact in (org-contacts-filter)
	     for anniv = (let ((anniv (cdr (assoc-string
					    (or field org-contacts-birthday-property)
					    (nth 2 contact)))))
			   (when anniv
			     (calendar-gregorian-from-absolute
			      (org-time-string-to-absolute anniv))))
	     ;; Use `diary-anniversary' to compute anniversary.
	     if (and anniv
		     (or (apply 'diary-anniversary anniv)
			 (and format0
			      (calendar-date-equal date anniv))))
	     collect (let* ((years (- (calendar-extract-year date)
				      (calendar-extract-year anniv)))
			    (fmt (if (= 0 years)
				     (or format0 format)
				   format)))
		       (format-spec fmt
				  `((?l . ,(org-with-point-at (cadr contact) (org-store-link nil)))
				    (?h . ,(car contact))
				    (?y . ,years)
				    (?Y . ,(format "%d%s" years (diary-ordinal-suffix years)))))
		       ))))



       )) ; org-capture

  (eval-after-load 'org-contacts
    '(progn

       )) ;; org-contacts
  )

(provide 'config-org-contacts)
