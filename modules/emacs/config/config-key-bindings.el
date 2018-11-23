(use-package general
  :ensure t
  :config
  (global-set-key (kbd "C-z") nil)
  (global-set-key (kbd "M-m") nil)
  (setq leader-of-the-pack "M-m")


  (general-define-key "<f2>" 'bm-toggle
                      "<C-S-return>" 'open-line-above
                      "<C-return>" 'open-line-below
                      "C-'" 'er/expand-region
                      "<C-tab>" 'company-complete
                      "C-<f2>" 'bm-next
                      "C-S-<f4>" 'previous-error
                      "C-S-c C-S-c" 'mc/edit-lines
                      "C-S-c C-a" 'mc/edit-beginnings-of-lines
                      "C-S-c C-e" 'mc/edit-ends-of-lines
                      "C-c C-f" 'fzf
                      "C-c C-r" 'ivy-resume
                      "C-c b" 'create-scratch-buffer
                      "C-c d" 'duplicate-current-line-or-region
                      "C-c f" 'fasd-find-file
                      "C-c m" 'which-key-show-major-mode
                      "C-c n" 'cleanup-buffer
                      "C-c c" 'comment-or-uncomment-region
                      "C-c u" 'uncomment-region
                      "C-o" 'open-line-and-indent
                      "C-s" 'swiper
                      "C-x 3" 'split-window-right-and-move-there
                      "C-x M-w" 'copy-current-file-path
                      "C-x C-r" 'rename-current-buffer-file
                      "C-x C-k" 'delete-current-buffer-file
                      "M-/" 'company-yasnippet
                      "M-<down>" 'smart-down
                      "M-<left>" 'smart-backward
                      "M-<right>" 'smart-forward
                      "M-<up>" 'smart-up
                      "M-Z" (lambda (char) (interactive "cZap to char: ") (zap-to-char 1 char))
                      "M-i" 'back-to-indentation
                      "M-j" (λ (join-line -1))
                      "M-o" 'ace-window
                      "M-s-i" 'change-inner
                      "M-s-o" 'change-outer
                      "M-w" 'save-region-or-current-line
                      "M-z" 'zap-up-to-char
                      "M-p" 'backward-paragraph
                      "M-n" 'forward-paragraph
                      "S-<f2>" 'bm-previous
                      "S-<f4>" 'next-error
                      "S-<f7>" 'compile
                      "S-M-<f4>" 'first-error
                      "s-Z" (lambda (char) (interactive "cZap to char backwards: ") (zap-to-char -1 char))
                      "s-i" 'copy-inner
                      "s-o" 'copy-outer
                      "s-z" (lambda (char) (interactive "cZap up to char backwards: ") (zap-up-to-char -1 char))
                      )
  (general-define-key :keymaps 'c-mode-base-map
                      "M-j" (λ (join-line -1))
                      "M-*" 'rtags-location-stack-back
                      "M-," 'rtags-find-references-at-point
                      "M-." 'rtags-find-symbol-at-point
                      )
  (general-define-key :keymaps 'counsel-gtags-mode-map
                      "C-*" 'counsel-gtags-go-backward
                      "C-:" 'counsel-gtags-dwim
                      "C-."  'counsel-gtags-find-definition
                      "C-," 'counsel-gtags-find-reference
                      "C-s-s" 'counsel-gtags-find-symbol
                      )

  (general-define-key :prefix leader-of-the-pack
                      :keymaps 'pdf-view-mode-map
                      ;; Navigation
                      "xj"  'pdf-view-next-line-or-next-page
                      "xk"  'pdf-view-previous-line-or-previous-page
                      "xJ"  'pdf-view-next-page
                      "xK"  'pdf-view-previous-page
                      "xu"  'pdf-view-scroll-down-or-previous-page
                      "xd"  'pdf-view-scroll-up-or-next-page
                      ;; Scale/Fit
                      "xW"  'pdf-view-fit-width-to-window
                      "xH"  'pdf-view-fit-height-to-window
                      "xP"  'pdf-view-fit-page-to-window
                      "xzr" 'pdf-view-scale-reset
                      ;; Actions
                      "xs" 'pdf-occur
                      "xO" 'pdf-outline
                      "xw" 'kill-ring-save
                      "xgg"  'pdf-view-first-page
                      "xG" 'pdf-view-last-page
                      "x/" 'isearch-forward
                      "x?" 'isearch-backward)

  (general-define-key :keymaps 'occur-mode-map
                      "v" 'occur-mode-display-occurence
                      "n" 'next-line
                      "p" 'previous-line
                      )

  (general-define-key :prefix leader-of-the-pack
                      ;; applications
                      "a" '(nil :wk "applications")
                      "as" 'eshell
                      "au" 'undo-tree-visualize
                      ;; buffer
                      "b" '(nil :wk "buffer")
                      "bc" 'cleanup-buffer
                      "bs" 'create-scratch-buffer
                      ;; compile
                      "c" '(nil :wk "compile")
                      "cc" 'compile
                      "cf" 'first-error
                      "cn" 'next-error
                      "cp" 'previous-error
                      "cr" 'recompile
                      ;; errors
                      "e" '(nil :wk "errors")
                      "ed" 'flycheck-display-error-at-point
                      "ee" 'flycheck-explain-error-at-point
                      "el" 'flycheck-list-errors
                      "en" 'flycheck-next-error
                      "ep" 'flycheck-previous-error
                      ;; git
                      "g" '(nil :wk "git")
                      "gj" 'counsel-git-grep
                      "gs" 'magit-status
                      ;; jump
                      "j" '(nil :wk "jump")
                      "jg" 'goto-line-with-feedback
                      "ji" 'counsel-imenu
                      "jl" 'avy-goto-line
                      ;; projects
                      "p" '(nil :wk "projects")
                      "p<SPC>" 'counsel-projectile
                      "pD" 'projectile-dired
                      "pI" 'projectile-invalidate-cache
                      "pf" 'projectile-find-file
                      "pp" 'projectile-switch-project
                      ;; rings/resume
                      "r" '(nil :wk "rings/resume")
                      "rl" 'ivy-resume
                      "ry" 'counsel-yank-pop
                      ;; search
                      "s" '(nil :wk "search")
                      "sf" 'fasd-find-file
                      "sg" 'counsel-git
                      "si" 'counsel-imenu
                      "sj" 'counsel-git-grep
                      "sr" 'counsel-rg
                      "sz" 'fzf
                      ;; text
                      "t" '(nil :wk "text")
                      "tc" 'comment-region
                      "tl" 'sort-lines
                      "tu" 'uncomment-region
                      "tz" 'zap-to-char
                      ;; windows
                      "w" '(nil :wk "windows")
                      "wo" 'ace-delete-window
                      "ww" 'ace-window
                      )
  )


(provide 'config-key-bindings)
