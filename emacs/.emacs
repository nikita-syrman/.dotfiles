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
(setq-default line-spacing 0)

;; Line numbers (relative with absolute on current line)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Scale line numbers with text-scale (C-x C-+/-)
(defun my/scale-line-numbers ()
  (let ((h (if (bound-and-true-p text-scale-mode)
               (expt text-scale-mode-step text-scale-mode-amount)
             1.0)))
    (dolist (face '(line-number line-number-current-line))
      (face-remap-set-base face :height h))))
(add-hook 'text-scale-mode-hook #'my/scale-line-numbers)

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

;;; Custom Modes & Theme
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/")
(require 'fastc-mode)
(require 'fasm-mode)

;;; Header/Source Switch (F12)
(defun casey-find-corresponding-file ()
  "Find the file that corresponds to this one."
  (interactive)
  (let ((corresponding nil)
        (basename (file-name-sans-extension buffer-file-name)))
    (cond ((string-match "\\.c$" buffer-file-name)
           (setq corresponding (concat basename ".h")))
          ((string-match "\\.cpp$" buffer-file-name)
           (setq corresponding (concat basename ".h")))
          ((string-match "\\.h$" buffer-file-name)
           (if (file-exists-p (concat basename ".c"))
               (setq corresponding (concat basename ".c"))
             (setq corresponding (concat basename ".cpp")))))
    (if corresponding
        (find-file corresponding)
      (error "No corresponding file found"))))

(defun casey-find-corresponding-file-other-window ()
  "Find the file that corresponds to this one in another window."
  (interactive)
  (let ((corresponding nil)
        (basename (file-name-sans-extension buffer-file-name)))
    (cond ((string-match "\\.c$" buffer-file-name)
           (setq corresponding (concat basename ".h")))
          ((string-match "\\.cpp$" buffer-file-name)
           (setq corresponding (concat basename ".h")))
          ((string-match "\\.h$" buffer-file-name)
           (if (file-exists-p (concat basename ".c"))
               (setq corresponding (concat basename ".c"))
             (setq corresponding (concat basename ".cpp")))))
    (if corresponding
        (find-file-other-window corresponding)
      (error "No corresponding file found"))))

(global-set-key (kbd "<f12>") 'casey-find-corresponding-file)
(global-set-key (kbd "M-<f12>") 'casey-find-corresponding-file-other-window)

;;; TODO/NOTE Highlighting
(defface todo-face
  '((t (:foreground "red" :weight bold)))
  "Face for TODO keywords.")

(defface note-face
  '((t (:foreground "green" :weight bold)))
  "Face for NOTE keywords.")

(defun add-todo-note-keywords ()
  "Add TODO and NOTE keyword highlighting."
  (font-lock-add-keywords nil
    '(("\\<\\(TODO\\)\\>" 1 'todo-face prepend)
      ("\\<\\(NOTE\\)\\>" 1 'note-face prepend))))

(add-hook 'prog-mode-hook 'add-todo-note-keywords)

;;; Compilation
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;;; Custom (auto-generated)
(custom-set-variables
 '(custom-enabled-themes '(casey))
 '(custom-safe-themes
   '("09276f492e8e604d9a0821ef82f27ce58b831f90f49f986b4d93a006c12dbcdb"
     "e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7"
     default))
 '(package-selected-packages '(claude-code gruber-darker-theme nasm-mode naysayer-theme smex))
 '(tool-bar-mode nil))
(custom-set-faces
 '(default ((t (:family "CaskaydiaMono Nerd Font" :foundry "SAJA" :slant normal :weight regular :height 113 :width normal)))))

;;; init.el ends here
