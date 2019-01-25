;;                                                    -*- emacs-lisp -*-
;;
;; Copyright (C) 2017, 2018, 2019 by Peder Stray <peder@ninja.no>
;;

(with-demoted-errors
    "Error: init-org-capture: %s"

  ;; (eval-after-load 'org '(require 'org-capture))

  (message "+++ Setting up org-capture")

  (setq
   ;; where the different templates should be used
   ;;org-capture-templates-contexts
   ;;'(("c" ((in-mode . "message-mode"))))

     ;; store a position to where capture were done
   org-capture-bookmark		t
   org-default-notes-file	"agenda/refile.org"	; notes-file

   org-capture-templates
   '(
     ;; (keys = "string"
     ;;  descriptionn = "string"
     ;;  type = entry
     ;;       | item
     ;;       | checkitem
     ;;       | table-line
     ;;       | plain
     ;;  target = (file "FILE")
     ;;         | (id "ID-of-org-entry")
     ;;         | (file+headline "FILE" "UNIQUE HEADLINE")
     ;;         | (file+olp "FILE" "LEVEL1" "LEVEL2" ...)
     ;;         | (file+regexp "FILE" "REGEXP")
     ;;         | (file+olp+datetree "FILE" "LEVEL1" ...)
     ;;         | (file+function "FILE" location-function)
     ;;         | (clock)
     ;;         | (function location-function)
     ;; template = "TEMPLATE"
     ;;          | (file "TEMPLATEFILE")
     ;;          | (function template-function)
     ;; OPTIONS+
     ("t" "Todo" entry (file "agenda/refile.org")
      "* TODO %?\n  SCHEDULED: %T\n\n  %i\n  %a\n"
      :clock-in t :clock-resume t :empty-lines 1
      ;; :kill-buffer
      )

     ("n" "Note" entry (file "agenda/refile.org")
      "* %? :NOTE:\n\  %U\n  %a\n"
      :clock-in t :clock-resume t :empty-lines 1
      )

     ("j" "Journal" entry (file+datetree "agenda/diary.org")
      "* %?\n  %U\n"
      :clock-in t :clock-resume t :empty-lines 1
      )

     ;; templates for org-protocol
     ("p" "Link with selection" entry (file "agenda/refile.org")
      "* %? :url:\n\n  Source: [[%:link][%(ps:tanslate-square-to-round \"%:description\")]\n\n  #+BEGIN_QUOTE\n  %:initial\n  #+END_QUOTE\n"
      :clock-in t :clock-resume t :empty-lines 1
      ;; :prepend t
      )

     ("L" "Plain Link" entry (file "agenda/refile.org")
      "* %? :url:\n\n  Source: [[%:link][%(ps:tanslate-square-to-round \"%:description\")]]\n"
      :clock-in t :clock-resume t :empty-lines 1
      ;; :prepend t
      ;; :immediate-finish t
      )

     ;; %c - an Org-link pointing to the location of the page
     ;; %i - selected text
     ;; %:link -
     ;; %:description -
     ;; %:initial -

     )
   ) ; setq

  (eval-after-load 'org-capture
    '(progn
       (message "+++ Configuring org-capture")

       (defun ps:tanslate-square-to-round (string-to-transform)
	 "Transforms [] into ()."
	 (concat (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform)))

       ;; start emacsclient with -c -F'((name . "org-capture"))'
       (defun ps:org-capture-delete-capture-frame ()
	 (let ((name (frame-parameter nil 'name)))
	   ;; (message "ps:org-capture-delete-capture-frame [%s]" name)
	   (if (equal name "org-capture")
	       (delete-frame))))

       (add-hook 'org-capture-after-finalize-hook
		 #'ps:org-capture-delete-capture-frame)

       (defun ps:org-remove-empty-properties ()
	 (interactive)
	 (dolist (prop (org-entry-properties))
	   (if (equal (cdr prop) "")
	       (org-delete-property (car prop)))))

       (add-hook 'org-capture-prepare-finalize-hook
		 #'ps:org-remove-empty-properties)

       (defun ps:org-add-created-property ()
	 (interactive)
	 (save-excursion
	   (or (org-entry-get nil "CREATED")
	       (org-entry-put nil "CREATED"
		(concat "[" (substring
			     (format-time-string
			      (cdr org-time-stamp-formats)) 1 -1) "]")))))

       (add-hook 'org-capture-mode-hook
		 #'ps:org-add-created-property)

       ))
  )

(provide 'config-org-capture)
