(add-to-list 'load-path "~/.config/emacs/languages/bsv/")
(add-to-list 'load-path "~/.config/emacs/languages/bsv/emacs20-extras.el")
(add-to-list 'load-path "~/.config/emacs/languages/bsv/mark.el")

(autoload 'bsv-mode "bsv-mode" "BSV mode" t )
(setq auto-mode-alist (cons  '("\\.bsv\\'" . bsv-mode) auto-mode-alist))
(setq auto-mode-alist (cons  '("\\.defines\\'" . bsv-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.defs\\'" . bsv-mode) auto-mode-alist))
(setq bsv-indent-level-behavioral 2)
(setq bsv-indent-level-declaration 2)
(setq bsv-indent-level 2)

;;remainder theree is a bsv-indent-alist that defines the the indent variable to be used for each type of block

(setq auto-mode-alist (cons '("\\.sdc\\'" . tcl-mode) auto-mode-alist))

(use-package anaconda-mode
:config
(add-hook 'python-mode-hook 'anaconda-mode))

(use-package yaml-mode
:config
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
    '(lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))
