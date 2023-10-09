(defvar elpaca-installer-version 0.5)
  (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
  (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
  (defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
  (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
				:ref nil
				:files (:defaults (:exclude "extensions"))
				:build (:not elpaca--activate-package)))
  (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
	 (build (expand-file-name "elpaca/" elpaca-builds-directory))
	 (order (cdr elpaca-order))
	 (default-directory repo))
    (add-to-list 'load-path (if (file-exists-p build) build repo))
    (unless (file-exists-p repo)
      (make-directory repo t)
      (when (< emacs-major-version 28) (require 'subr-x))
      (condition-case-unless-debug err
	  (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
		   ((zerop (call-process "git" nil buffer t "clone"
					 (plist-get order :repo) repo)))
		   ((zerop (call-process "git" nil buffer t "checkout"
					 (or (plist-get order :ref) "--"))))
		   (emacs (concat invocation-directory invocation-name))
		   ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
					 "--eval" "(byte-recompile-directory \".\" 0 'force)")))
		   ((require 'elpaca))
		   ((elpaca-generate-autoloads "elpaca" repo)))
	      (progn (message "%s" (buffer-string)) (kill-buffer buffer))
	    (error "%s" (with-current-buffer buffer (buffer-string))))
	((error) (warn "%s" err) (delete-directory repo 'recursive))))
    (unless (require 'elpaca-autoloads nil t)
      (require 'elpaca)
      (elpaca-generate-autoloads "elpaca" repo)
      (load "./elpaca-autoloads")))
  (add-hook 'after-init-hook #'elpaca-process-queues)
  (elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
    :init      ;; tweak evil's configuration before loading it
    (setq evil-want-integration t  ;; This is optional since it's already set to t by default.
          evil-want-keybinding nil
          evil-vsplit-window-right t
          evil-split-window-below t
          evil-undo-system 'undo-redo)  ;; Adds vim-like C-r redo functionality
    (evil-mode))

(use-package evil-collection
  :after evil
  :config
  ;; Do not uncomment this unless you want to specify each and every mode
  ;; that evil-collection should works with.  The following line is here 
  ;; for documentation purposes in case you need it.  
  ;; (setq evil-collection-mode-list '(calendar dashboard dired ediff info magit ibuffer))
  (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode
  (evil-collection-init))

(use-package evil-tutor)

;; Using RETURN to follow links in Org/Evil 
;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))
;; Setting RETURN key in org-mode to follow links
  (setq org-return-follows-link  t)

(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

(use-package general
    :config
    (general-evil-setup)
  ;; setting up 'SPC' as the leader key
  (general-create-definer rgrs/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "C-SPC") ;; access leader in insert mode

  (rgrs/leader-keys
    "b" '(:ignore t :wk "buffer")
    "b b" '(consult-buffer :wk "Switch buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b R" '(rename-buffer :wk "rename the current buffer")
    "b k" '(kill-this-buffer :wk "Kill this buffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer"))

  (rgrs/leader-keys
   "e" '(:ignore t :wk "Evaluate")    
   "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
   "e d" '(eval-defun :wk "Evaluate defun containing or after point")
   "e e" '(eval-expression :wk "Evaluate and elisp expression")
   "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
   "e r" '(eval-region :wk "Evaluate elisp in region"))

(rgrs/leader-keys
  "." '(find-file :wk "Find file")
  "f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Edit emacs config"))

(rgrs/leader-keys
  "h" '(:ignore t :wk "Help")
  "h f" '(describe-function :wk "Describe function")
  "h v" '(describe-variable :wk "Describe variable")
  "h r r" '((lambda () (interactive) 
	      (load-file "~/.config/emacs/init.el")
	      (ignore (elpaca-process-queues))) :wk "Reload emacs config")
  )

(rgrs/leader-keys
  "t" '(:ignore t :wk "Toggle")
  "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
  "t w" '(visual-line-mode :wk "word wrap"))

(rgrs/leader-keys
  "w" '(:ignore t :wk "Windows")
  ;; Window splits
  "w c" '(evil-window-delete :wk "Close window")
  "w q" '(evil-window-delete :wk "Close window")
  "w n" '(evil-window-new :wk "New window")
  "w s" '(evil-window-split :wk "Horizontal split window")
  "w v" '(evil-window-vsplit :wk "Vertical split window")
  ;; Window motions
  "w h" '(evil-window-left :wk "Window left")
  "w j" '(evil-window-down :wk "Window down")
  "w k" '(evil-window-up :wk "Window up")
  "w l" '(evil-window-right :wk "Window right")
  "w w" '(evil-window-next :wk "Goto next window")
  ;; Move Windows
  "w H" '(windmove-swap-states-left :wk "Buffer move left")
  "w J" '(windmove-swap-states-down :wk "Buffer move down")
  "w K" '(windmove-swap-states-up :wk "Buffer move up")
  "w L" '(windmove-swap-states-right :wk "Buffer move right")
  ;;Window Size
  "w |" '(evil-window-set-width :wk "Maximize Veritcal Window")
  "w _" '(evil-window-set-height :wk "Maximize Horizontal Window")
  ;; Replace with windresize package
  "w =" '(evil-window-increase-height :wk "Increase Window Height")
  "w -" '(evil-window-decrease-height :wk "Decrease Window Height")
  "w >" '(evil-window-increase-width :wk "Increase Window Width")
  "w <" '(evil-window-decrease-width :wk "Decrease Window Width"))

(rgrs/leader-keys
  "v" '(:ignore t :wk "Vterm")
  "v n" '(multi-vterm :wk "Create new Vterm buffer")
  "v f" '(multi-vterm-next :wk "Move to next vterm buffer")
  "v p" '(multi-vterm-prev :wk "Move to previous vterm buffer")
  "v r" '(multi-vterm-rename-buffer :wk "Rename vterm buffer"))

(rgrs/leader-keys
  "s" '(:ignore t :wk "Search")
  "s s" '(consult-line :wk "interactive search a line in the buffer")
  "s S" '(consult-line-multi :wk "interactive search a line in multiple buffer"))

(rgrs/leader-keys
  "g" '(:ignore t :wk "Git")
  "g g" '(magit-status :wk "Magit-Status")
  "g C" '(magit-clone :wk "Magit clone"))

(general-define-key 
:keymaps 'minibuffer-local-map (kbd "C-v") 'yank)

)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package consult)

(use-package dashboard
  :elpaca t
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

;; Set the title
(setq dashboard-banner-logo-title "Life is all about MinMacs")
;; Set the banner
(setq dashboard-startup-banner "/home/rohit/.config/emacs/images/ascii-text-art.txt")
;; Value can be
;; - nil to display no banner
;; - 'official which displays the official emacs logo
;; - 'logo which displays an alternative emacs logo
;; - 1, 2 or 3 which displays one of the text banners
;; - "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
;; - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)

;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts t)

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
(setq dashboard-display-icons-p t)
(setq dashboard-icon-type 'nerd-icons)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(setq doom-modeline-project-detection 'auto)

;; Specification of \"percentage offset\" of window through buffer.
(setq doom-modeline-percent-position '(-3 "%p"))

;; ;; Format used to display line numbers in the mode line. Also used to display column for some reason
(setq doom-modeline-position-line-format '("L%l:C%c"))
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-enable-word-count nil)

(set-face-attribute 'default nil
  :font "JetBrains Mono"
  :height 120
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Ubuntu"
  :height 130
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrains Mono"
  :height 120
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-12"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(use-package magit)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package marginalia
:bind (:map minibuffer-local-map
("M-A" . marginalia-cycle))
:init
(marginalia-mode))

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(custom-set-faces
'(org-level-1 ((t (:inherit outline-1 :height 1.7))))
'(org-level-2 ((t (:inherit outline-2 :height 1.6))))
'(org-level-3 ((t (:inherit outline-3 :height 1.5))))
'(org-level-4 ((t (:inherit outline-4 :height 1.4))))
'(org-level-5 ((t (:inherit outline-5 :height 1.3))))
'(org-level-6 ((t (:inherit outline-5 :height 1.2))))
'(org-level-7 ((t (:inherit outline-5 :height 1.1)))))

(require 'org-tempo)

(electric-indent-mode -1)
(setq org-edit-src-content-indentation 0)

(use-package projectile
:config
(projectile-mode))

(use-package rainbow-delimiters
:config
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
:hook org-mode prog-mode)

(use-package restart-emacs)

(use-package vterm
:config
(setq shell-file-name "/usr/bin/bash")
(add-to-list 'vterm-tramp-shells '("ssh" "/bin/bash"))
(add-to-list 'vterm-tramp-shells '("sudo" "/bin/bash"))
)

(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.3))))

(use-package multi-vterm 
:after vterm    
:ensure t)

(use-package smartparens
:config
(smartparens-global-mode))

(use-package evil-smartparens
:config
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package sudo-edit
:config
(rgrs/leader-keys
  "f u" '(sudo-edit-find-file :wk "Sudo find file")
  "f U" '(sudo-edit :wk "Sudo edit file")))

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")

(use-package doom-themes
:ensure t
:config
(setq doom-themes-enabled-bold t
      doom-themes-enable-italic t))

(setq custom-safe-themes t)
(add-hook 'elpaca-after-init-hook (lambda() (load-theme 'doom-nord)))
;; (load-theme 'doom-nord)

(use-package vertico
  :init
  (vertico-mode)
;; (use-package savehist
;;   :init
;;   (savehist-mode))
)
(setq enable-recursive-minibuffers t)

(use-package winum
:config
(winum-mode))

(global-set-key (kbd "M-0") 'winum-select-window-0)
(global-set-key (kbd "M-1") 'winum-select-window-1)
(global-set-key (kbd "M-2") 'winum-select-window-2)
(global-set-key (kbd "M-3") 'winum-select-window-3)
(global-set-key (kbd "M-4") 'winum-select-window-4)
(global-set-key (kbd "M-5") 'winum-select-window-5)
(global-set-key (kbd "M-6") 'winum-select-window-6)
(global-set-key (kbd "M-7") 'winum-select-window-7)
(global-set-key (kbd "M-8") 'winum-select-window-8)

(use-package which-key
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit nil
        which-key-separator " → " ))
