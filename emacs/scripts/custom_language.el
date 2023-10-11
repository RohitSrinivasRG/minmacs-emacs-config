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

(setq auto-mode-alist (cons '("\\.sdc\\'" . tcl-mode) auto-mode-alist))
