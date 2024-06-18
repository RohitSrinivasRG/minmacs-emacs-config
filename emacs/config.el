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

(use-package evil-visualstar
:init
(global-evil-visualstar-mode))

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
    "b k" '(tabspaces-remove-current-buffer :wk "Kill this buffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer-quick :wk "Reload buffer"))

  (rgrs/leader-keys
   "e" '(:ignore t :wk "Evaluate")    
   "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
   "e d" '(eval-defun :wk "Evaluate defun containing or after point")
   "e e" '(eval-expression :wk "Evaluate and elisp expression")
   "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
   "e r" '(eval-region :wk "Evaluate elisp in region"))

(rgrs/leader-keys
  "." '(find-file :wk "Find file")
  "f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Edit emacs config")
  "f a" '((lambda () (interactive) (find-file "/home/rohit/org/Tasks.org")) :wk "Open Tasks file")
  "f y" '(put-file-name-on-clipboard :wk "Copy current working directory onto the clipboard"))

(rgrs/leader-keys
  "h" '(:ignore t :wk "Help")
  "h f" '(describe-function :wk "Describe function")
  "h v" '(describe-variable :wk "Describe variable")
  "h k" '(describe-key :wk "Describe keybindings")
  "h r r" '((lambda () (interactive) 
	      (load-file "~/.config/emacs/init.el")
	      (ignore (elpaca-process-queues))) :wk "Reload emacs config")
  )

(rgrs/leader-keys
  "t" '(:ignore t :wk "Toggle")
  "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
  "t r" '(rgrs/toggle-line-numbering :wk "Toggle between absolute and relative line numbers")
  "t v" '(vterm-toggle :wk "Toggle vterm")
  "t w" '(visual-line-mode :wk "word wrap"))

(rgrs/leader-keys
  "w" '(:ignore t :wk "Windows")
  ;; Window splits
  "w c" '(evil-window-delete :wk "Close window")
  "w q" '(evil-window-delete :wk "Close window")
  "w n" '(evil-window-new :wk "New window")
  "w s" '(evil-window-split :wk "Horizontal split window")
  "w v" '(evil-window-vsplit :wk "Vertical split window")
  "w u" '(winner-undo :wk "Undo Window configuration")
  "w r" '(winner-redo :wk "Redo Window configuration")
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
  "w =" '(windresize-balance-windows :wk "Maximize Horizontal Window")
  ;; Replace with windresize package
  ;; "w =" '(evil-window-increase-height :wk "Increase Window Height")
  ;; "w -" '(evil-window-decrease-height :wk "Decrease Window Height")
  ;; "w >" '(evil-window-increase-width :wk "Increase Window Width")
  ;; "w <" '(evil-window-decrease-width :wk "Decrease Window Width")
)

(rgrs/leader-keys
  "v" '(:ignore t :wk "Vterm")
  "v n" '(multi-vterm :wk "Create new Vterm buffer")
  "v f" '(multi-vterm-next :wk "Move to next vterm buffer")
  "v p" '(multi-vterm-prev :wk "Move to previous vterm buffer")
  "v r" '(multi-vterm-rename-buffer :wk "Rename vterm buffer"))

(rgrs/leader-keys
  "s" '(:ignore t :wk "Search")
  "s s" '(consult-line :wk "interactive search a line in the buffer")
  "s i" '(consult-imenu :wk "interactive search a line in the buffer")
  "s f" '(consult-projectile-find-dir :wk "interactive search a line in the buffer")
  "s g" '(consult-grep :wk "interactive search a line in the buffer")
  "s j" '(consult-goto-line :wk "interactive search a line in the buffer")
  "s S" '(consult-line-multi :wk "interactive search a line in multiple buffer")
  "s c" '(avy-goto-char :wk "jump to character with using avy")
  "s C" '(avy-goto-char-timer :wk "jump to character but with multiple character search option using avy"))

(rgrs/leader-keys
  "g" '(:ignore t :wk "Git")
  "g g" '(magit-status :wk "Magit-Status")
  "g C" '(magit-clone :wk "Magit clone")
  "g i" '(magit-init :wk "Magit init repo"))

(general-define-key 
:keymaps 'minibuffer-local-map (kbd "C-v") 'yank)

;; (rgrs/leader-keys
;;   "TAB" '(:ignore t :wk "Perspective")
;;   "TAB s" '(persp-switch :wk "Create or Switch perspectives")
;;   "TAB r" '(persp-rename :wk "Rename perspectives")
;;   "TAB c c" '(persp-kill :wk "Kill the perspective")
;;   "TAB n" '(persp-next :wk "Switch to next perspective")
;;   "TAB p" '(persp-prev :wk "Switch to prev perspective")
;;   "TAB m" '(persp-merge :wk "Temporarily merge two perspectives")
;;   "TAB u" '(persp-unmerge :wk "Undo persp-merge")
;;   "TAB a" '(persp-add-buffer :wk "Add open buffer to current perspective")
;;   "TAB A" '(persp-set-buffer :wk "Add buffer to current but delete from all others")
;;   "TAB 1" '(rgrs/persp-switch-to-1 :wk "Quick Switch to perspective 1")
;;   "TAB 2" '(rgrs/persp-switch-to-2 :wk "Quick Switch to perspective 2")
;;   "TAB 3" '(rgrs/persp-switch-to-3 :wk "Quick Switch to perspective 3")
;;   "TAB 4" '(rgrs/persp-switch-to-4 :wk "Quick Switch to perspective 4")
;;   "TAB 5" '(rgrs/persp-switch-to-5 :wk "Quick Switch to perspective 5")
;;   "TAB 6" '(rgrs/persp-switch-to-6 :wk "Quick Switch to perspective 6")
;;   "TAB 7" '(rgrs/persp-switch-to-7 :wk "Quick Switch to perspective 7")
;;   "TAB 8" '(rgrs/persp-switch-to-8 :wk "Quick Switch to perspective 8")
;;   "TAB 9" '(rgrs/persp-switch-to-9 :wk "Quick Switch to perspective 9")
;;   "TAB 0" '(rgrs/persp-switch-to-0 :wk "Quick Switch to perspective 0")
;;   "TAB TAB" '(persp-switch-by-number :wk "switch to perspective by number"))

(rgrs/leader-keys
  "TAB" '(:ignore t :wk "Tabspaces")
  "TAB s" '(tabspaces-switch-or-create-workspace :wk "Create or Switch tabspaces")
  "TAB r" '(tab-bar-rename-tab :wk "Rename tab")
  "TAB c c" '(tabspaces-kill-buffers-close-workspace :wk "Kill the tab workspace")
  "TAB S" '(tabspaces-open-or-create-project-and-workspace :wk "Open a project as new tab ")
  "TAB b" '(tabspaces-switch-buffer-and-tab :wk "Switch to Buffer present in another Tab")
  "TAB 1" '(move-to-tab-1 :wk "Quick Switch to tab 1")
  "TAB 2" '(move-to-tab-2 :wk "Quick Switch to tab 2")
  "TAB 3" '(move-to-tab-3 :wk "Quick Switch to tab 3")
  "TAB 4" '(move-to-tab-4 :wk "Quick Switch to tab 4")
  "TAB 5" '(move-to-tab-5 :wk "Quick Switch to tab 5")
  "TAB 6" '(move-to-tab-6 :wk "Quick Switch to tab 6")
  "TAB 7" '(move-to-tab-7 :wk "Quick Switch to tab 7")
  "TAB 8" '(move-to-tab-8 :wk "Quick Switch to tab 8")
  "TAB 9" '(move-to-tab-9 :wk "Quick Switch to tab 9")
  ;; "TAB 0" '(move-to-tab-0 :wk "Quick Switch to tab 0")
  ;; "TAB TAB" '(persp-switch-by-number :wk "switch to perspective by number")
)

(rgrs/leader-keys
  "o" '(:ignore t :wk "Org-Mode")
  "o e" '(rgrs/org-mode-empahsis-toggle :wk "toggle emphasis marks ")
  "o p" '(org-tree-slide-mode :wk "Start org presentation
"))

(rgrs/leader-keys
  "r" '(:ignore t :wk "Org-Roam-Mode")
  "r i" '(org-roam-node-insert :wk "roam insert node ")
  "r g" '(org-roam-graph :wk "roam display graph")
  "r f" '(org-roam-node-find :wk "roam find node")
  "r c" '(org-roam-capture :wk "roam capture"))

(rgrs/leader-keys
  "SPC" '(projectile-find-file :wk "Find File in the current project"))

(general-define-key "C-u" 'evil-scroll-up)

)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package avy)

(use-package anzu
:config
(global-anzu-mode 1)
(general-define-key [remap query-replace] 'anzu-query-replace)
(general-define-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package consult)
;; (add-to-list 'consult-buffer-sources persp-consult-source))
(use-package consult-projectile)
;; (use-package consult-eglot)

(use-package company
:config
(setq company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.2))))

(add-hook 'elpaca-after-init-hook 'global-company-mode)

(defun put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(use-package corral
:config
(global-set-key (kbd "M-9") 'corral-parentheses-backward)
(global-set-key (kbd "M-0") 'corral-parentheses-forward)
(global-set-key (kbd "M-[") 'corral-brackets-backward)
(global-set-key (kbd "M-]") 'corral-brackets-forward)
(global-set-key (kbd "M-{") 'corral-braces-backward)
(global-set-key (kbd "M-}") 'corral-braces-forward)
(global-set-key (kbd "M-\"") 'corral-double-quotes-backward))

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
(setq dashboard-startup-banner "/home/rohit/.config/emacs/images/Final_Splash_screen.txt")
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

(use-package dired
  :elpaca nil
  :ensure nil
  :commands (dired dired-jump)
  :config
  (setq insert-directory-program "ls" dired-use-ls-dired nil)
  (setq dired-listing-switches "-agho --group-directories-first")
  )

(use-package dired-hide-dotfiles
  :config
  (general-define-key :keymaps 'dired-mode-map (kbd "C-x C-.") 'dired-hide-dotfiles-mode)
  )
(defun my-dired-mode-hook ()
  "My `dired' mode hook."
  (interactive)
  ;; To hide dot-files by default
  (dired-hide-dotfiles-mode))

;; (define-key dired-mode-map "." #'dired-hide-dotfiles-mode)
(add-hook 'dired-mode-hook #'my-dired-mode-hook)

(use-package dired-open
  :config
  (setq dired-open-extensions '(("vcd" . "gtkwave")
				  ("fst" . "gtkwave"))))
(use-package peep-dired
  :after dired
  :config
    (general-evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
    (general-evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
    (general-evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
    (general-evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
    (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
)
(setq dired-dwim-target t)

(use-package dimmer
:config
(dimmer-configure-which-key)
(dimmer-mode t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(setq doom-modeline-project-detection 'auto)

;; Specification of \"percentage offset\" of window through buffer.
(setq doom-modeline-percent-position '(-3 "%p"))

;; ;; Format used to display line numbers in the mode line. Also used to display column for some reason
(setq doom-modeline-position-line-format '("%l:%c"))
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-enable-word-count nil)

(use-package drag-stuff
:init
(drag-stuff-global-mode)
:config
(drag-stuff-define-keys))

(defun rgrs/spc_4_indent ()
 "Updates the indent tabs mode to nil"
(interactive)
(setq indent-tabs-mode nil))

(defun rgrs/test_print ()
 "Updates the indent tabs mode to nil"
(interactive)
(message "Mode loaded;LMAO bsv-mode-hook working"))
(add-hook 'bsv-mode-hook #'rgrs/spc_4_indent)
(add-hook 'bsv-mode-hook 'rgrs/test_print)
(add-hook 'prog-hook #'rgrs/spc_4_indent)

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

;; Creating a face for bluepsec blue color
;;bluespec blue #0082f1
(defface nerd-icons-bluespec-blue
  '((((background dark)) :foreground "#0082f1")
    (((background light)) :foreground "#0082f1"))
  "Face for bluespec blue."
  :group 'nerd-icons-faces)

;; custom 
(defcustom fontello-font-family "fontello"
  "The Nerd Font for display icons."
  :group 'nerd-icons
  :type 'string)

(require 'fontello  "~/.config/emacs/fonts/fontello.el")
;; (add-hook 'elpaca-after-init-hook (lambda() (add-to-list 'nerd-icons-extension-icon-alist `("bsv"   nerd-icons-fontello "nf-bluespec"    :face nerd-icons-bluespec-blue))))
;; (add-hook 'elpaca-after-init-hook (lambda() (add-to-list 'nerd-icons-mode-icon-alist `(bsv-mode   nerd-icons-fontello "nf-bluespec"    :face nerd-icons-bluespec-blue))))
;; (add-hook 'elpaca-after-init-hook (lambda() (nerd-icons-define-icon fontello nerd-icons/fontello-alist fontello-font-family "Fontello")))
;; (add-to-list 'nerd-icons-extension-icon-alist `("bsv"   nerd-icons-fontello "nf-bluespec"    :face nerd-icons-bluespec-blue))
;; (add-to-list 'nerd-icons-mode-icon-alist `(bsv-mode   nerd-icons-fontello "nf-bluespec"    :face nerd-icons-bluespec-blue))
;; (nerd-icons-define-icon fontello nerd-icons/fontello-alist fontello-font-family "Fontello")

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package magit)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package hl-todo
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package highlight-indent-guides
:ensure t
:config
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-responsive 'stack)

)

(use-package keycast)

(add-to-list `load-path (org-babel-load-file (expand-file-name "~/.config/emacs/scripts/custom_language.org" "~/.config/emacs/scripts/")))

(use-package marginalia
:bind (:map minibuffer-local-map
("M-A" . marginalia-cycle))
:init
(marginalia-mode))

(use-package evil-multiedit
:config
(evil-multiedit-default-keybinds)
;; (general-define-key :keymap `evil-visual-state-map "R" 'evil-multiedit-match-all)
(general-define-key :keymap `evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
(general-define-key :keymap `evil-visual-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
(general-define-key :keymap `evil-insert-state-map (kbd "M-d") 'evil-multiedit-toggle-marker-here)
(general-define-key :keymap `evil-normal-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
(general-define-key :keymap `evil-visual-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
(general-define-key :keymap `evil-visual-state-map (kbd "C-M-D") 'evil-multiedit-restore)'
(general-define-key :keymap 'evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
(general-define-key :keymap 'evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
(general-define-key :keymap 'evil-multiedit-state-map (kbd "C-n") 'evil-multiedit-next)
(general-define-key :keymap 'evil-multiedit-state-map (kbd "C-p") 'evil-multiedit-prev)
(general-define-key :keymap 'evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
(general-define-key :keymap 'evil-multiedit-insert-state-map (kbd "C-p") 'evil-multiedit-prev)
(evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
)
;; TODO need to add evil-mc to play hand in hand with evil-multiedit

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
:config 
(add-to-list 'nerd-icons-extension-icon-alist `("bsv"   nerd-icons-fontello "nf-bluespec"    :face nerd-icons-bluespec-blue))
(add-to-list 'nerd-icons-mode-icon-alist `(bsv-mode   nerd-icons-fontello "nf-bluespec"    :face nerd-icons-bluespec-blue))
(nerd-icons-define-icon fontello nerd-icons/fontello-alist fontello-font-family "Fontello")
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

(setq org-agenda-files 
   '("/home/rohit/org/Tasks.org")
)

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)"))
)

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

(use-package org-tree-slide
:config
(setq org-image-acutal-width nil))

(defun rgrs/toggle-emphasis-markers ()
"Toggle emphasis marker in Org-Mode"
(interactive)
(if (eq org-hide-emphasis-markers nil)
    (setq org-hide-emphasis-markers t)
    (setq org-hide-emphasis-markers nil))
)

(defun rgrs/org-mode-empahsis-toggle ()
(interactive)
(add-hook `org-mode-hook 'rgrs/toggle-emphasis-markers)
(revert-buffer-quick))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/RoamNotes")
  :config
  (org-roam-setup))

  ;; :bind (("C-c n l" . org-roam-buffer-toggle)
  ;;        ("C-c n f" . org-roam-node-find)
  ;;        ("C-c n g" . org-roam-graph)
  ;;        ("C-c n i" . org-roam-node-insert)
  ;;        ("C-c n c" . org-roam-capture)
  ;;        ;; Dailies
  ;;        ("C-c n j" . org-roam-dailies-capture-today))

(use-package pdf-tools
  :config
  (pdf-tools-install))

(use-package tablist)

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

(use-package solaire-mode
:config
(solaire-global-mode +1)
)

(use-package sudo-edit
:config
(rgrs/leader-keys
  "f u" '(sudo-edit-find-file :wk "Sudo find file")
  "f U" '(sudo-edit :wk "Sudo edit file")))

(defun move-to-tab-1 ()
  (interactive)
  (tab-bar-select-tab 1))

(defun move-to-tab-2 ()
  (interactive)
  (tab-bar-select-tab 2))

(defun move-to-tab-3 ()
  (interactive)
  (tab-bar-select-tab 3))

(defun move-to-tab-4 ()
  (interactive)
  (tab-bar-select-tab 4))

(defun move-to-tab-5 ()
  (interactive)
  (tab-bar-select-tab 5))

(defun move-to-tab-6 ()
  (interactive)
  (tab-bar-select-tab 6))

(defun move-to-tab-7 ()
  (interactive)
  (tab-bar-select-tab 7))

(defun move-to-tab-8 ()
  (interactive)
  (tab-bar-select-tab 8))

(defun move-to-tab-9 ()
  (interactive)
  (tab-bar-select-tab 9))

(use-package tabspaces
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup. 
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo t)
  (tabspaces-todo-file-name "project-todo.org")
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t))

(defun my--tabspace-setup ()
 "Set up tabspace at startup."
 ;; Add *Messages* and *splash* to Tab \`Home\'
 (tabspaces-mode 1)
 (progn
   (tab-bar-rename-tab "Home")
   (when (get-buffer "*Messages*")
     (set-frame-parameter nil
                          'buffer-list
                          (cons (get-buffer "*Messages*")
                                (frame-parameter nil 'buffer-list))))
   (when (get-buffer "*splash*")
     (set-frame-parameter nil
                          'buffer-list
                          (cons (get-buffer "*splash*")
                                (frame-parameter nil 'buffer-list))))))

(add-hook 'elpaca-after-init-hook #'my--tabspace-setup)
;;Consult integration
(with-eval-after-load 'consult
;; hide full buffer list (still available with "b" prefix)
(consult-customize consult--source-buffer :hidden t :default nil)
;; set consult-workspace buffer list
(defvar consult--source-workspace
  (list :name     "Workspace Buffers"
        :narrow   ?w
        :history  'buffer-name-history
        :category 'buffer
        :state    #'consult--buffer-state
        :default  t
        :items    (lambda () (consult--buffer-query
                         :predicate #'tabspaces--local-buffer-p
                         :sort 'visibility
                         :as #'buffer-name)))

  "Set workspace buffer list for consult-buffer.")
(add-to-list 'consult-buffer-sources 'consult--source-workspace))

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")

(use-package doom-themes
:ensure t
:config
(setq doom-themes-enabled-bold t
      doom-themes-enable-italic t))

(setq custom-safe-themes t)
(add-hook 'elpaca-after-init-hook (lambda() (load-theme 'doom-one)))
;; (load-theme 'doom-nord)

(defun rgrs/toggle-line-numbering ()
  "Toggle line numbering between absolute and relative."
  (interactive)
  (if (eq display-line-numbers 'relative)
      (setq display-line-numbers t)
    (setq display-line-numbers 'relative)))

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

(global-set-key (kbd "C-0") 'winum-select-window-0)
(global-set-key (kbd "C-1") 'winum-select-window-1)
(global-set-key (kbd "C-2") 'winum-select-window-2)
(global-set-key (kbd "C-3") 'winum-select-window-3)
(global-set-key (kbd "C-4") 'winum-select-window-4)
(global-set-key (kbd "C-5") 'winum-select-window-5)
(global-set-key (kbd "C-6") 'winum-select-window-6)
(global-set-key (kbd "C-7") 'winum-select-window-7)
(global-set-key (kbd "C-8") 'winum-select-window-8)

(winner-mode 1)

(use-package windresize)

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

(use-package  yasnippet)
