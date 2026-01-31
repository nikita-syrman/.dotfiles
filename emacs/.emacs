;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Package Management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; UI Settings
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)

;; Line numbers (relative with absolute on current line)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Whitespace visualization (spaces as dots)
(setq whitespace-style '(face spaces space-mark))
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])))  ; 32 = space, 183 = middle dot
(global-whitespace-mode 1)
(set-face-attribute 'whitespace-space nil
                    :background nil
                    :foreground "gray30")

;;; Editor Settings
;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq-default tab-always-indent t)

;;; Navigation
(ido-mode 1)
(ido-everywhere 1)

;; Smex (M-x enhancement)
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;; Custom Modes
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'fastc-mode)
(require 'fasm-mode)

;;; Compilation
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;;; Custom (auto-generated)
(custom-set-variables
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("09276f492e8e604d9a0821ef82f27ce58b831f90f49f986b4d93a006c12dbcdb"
     "e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7"
     default))
 '(package-selected-packages '(claude-code gruber-darker-theme nasm-mode naysayer-theme smex))
 '(tool-bar-mode nil))
(custom-set-faces
 '(default ((t (:family "CaskaydiaMono Nerd Font" :foundry "SAJA" :slant normal :weight regular :height 113 :width normal)))))

;;; init.el ends here
