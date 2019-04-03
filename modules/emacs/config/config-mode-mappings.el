;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install modes not bundled with emacs
(use-package bitbake :ensure t :defer t)
(use-package dockerfile-mode :ensure t :defer t)
(use-package dts-mode :ensure t :defer t)
(use-package haskell-mode :ensure t :defer t)
(use-package jade-mode :ensure t :defer t)
(use-package js2-mode :ensure t :defer t)
(use-package nsis-mode :ensure t :defer t)
(use-package qml-mode :ensure t :defer t)
(use-package qt-pro-mode :ensure t :defer t)
(use-package rainbow-mode :ensure t :delight :defer t)
(use-package sws-mode :ensure t :defer t)
(use-package systemd :ensure t :defer t)

(load-file
 (concat (file-name-as-directory site-lisp-dir) "robot-mode.el"))

(define-derived-mode soong-mode
  javascript-mode "Soong"
  "Major mode for soong files."
  :syntax-table nil
  :abbrev-table nil
  (setq-local indent-tabs-mode nil)
  (setq-local js-indent-level 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map extensions to modes
(add-to-list 'auto-mode-alist '("Carton$" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tag$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vm$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.watchr$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.robot$" . robot-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
(add-to-list 'magic-mode-alist '("# -*-robot-*" . robot-mode))
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))
(add-to-list 'auto-mode-alist '("SConstruct$" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript$" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript.*$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bb$" . bitbake-mode))
(add-to-list 'auto-mode-alist '("\\.bbappend$" . bitbake-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . bitbake-mode))
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))
(add-to-list 'auto-mode-alist '("\\.qbs$" . qml-mode))
(add-to-list 'auto-mode-alist '("\\.dts$" . dts-mode))
(add-to-list 'auto-mode-alist '("\\.dtsi$" . dts-mode))
(add-to-list 'auto-mode-alist '("\\.pro$" . qt-pro-mode))
(add-to-list 'auto-mode-alist '("\\.pri$" . qt-pro-mode))
(add-to-list 'auto-mode-alist '("\\.bp\\'" . soong-mode))

(provide 'config-mode-mappings)
