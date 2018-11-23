;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install modes not bundled with emacs

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(global-subword-mode 1) ;; navigate sillycased words
(global-auto-revert-mode 1) ;; auto-revert buffers on background changes
(delete-selection-mode 1)
(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setq global-auto-revert-non-file-buffers t) ;; auto-revert dired
(setq auto-revert-verbose nil)
(setq delete-by-moving-to-trash t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default truncate-lines t)
(set-default 'sentence-end-double-space nil)
(setq compilation-scroll-output t)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq js-indent-level 4)

(windmove-default-keybindings)
(winner-mode)
(auto-compression-mode t)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-right-option-modifier 'none)
  (setq mac-command-modifier 'meta)
  (setq mac-function-modifier 'hyper))


(provide 'config-sane-defaults)
