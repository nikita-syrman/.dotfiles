;;; casey-theme.el --- Casey Muratori's Handmade Hero color theme -*- lexical-binding: t; -*-

;; Author: Casey Muratori (adapted from handmadehero/.emacs)
;; URL: https://github.com/ecxr/handmadehero/blob/master/misc/.emacs

;;; Commentary:
;; Dark theme based on Casey Muratori's Emacs configuration from Handmade Hero.

;;; Code:

(deftheme casey "Casey Muratori's Handmade Hero theme")

(custom-theme-set-faces
 'casey

 ;; Basic faces
 '(default ((t (:background "#161616" :foreground "burlywood3"))))
 '(cursor ((t (:background "#40FF40"))))
 '(region ((t (:background "medium blue"))))
 '(hl-line ((t (:background "midnight blue"))))
 '(fringe ((t (:background "#161616"))))
 '(vertical-border ((t (:foreground "#161616"))))

 ;; Font lock faces
 '(font-lock-builtin-face ((t (:foreground "burlywood3"))))
 '(font-lock-comment-face ((t (:foreground "gray50"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "gray50"))))
 '(font-lock-constant-face ((t (:foreground "olive drab"))))
 '(font-lock-doc-face ((t (:foreground "gray50"))))
 '(font-lock-function-name-face ((t (:foreground "burlywood3"))))
 '(font-lock-keyword-face ((t (:foreground "DarkGoldenrod3"))))
 '(font-lock-preprocessor-face ((t (:foreground "olive drab"))))
 '(font-lock-string-face ((t (:foreground "olive drab"))))
 '(font-lock-type-face ((t (:foreground "burlywood3"))))
 '(font-lock-variable-name-face ((t (:foreground "burlywood3"))))

 ;; Mode line
 '(mode-line ((t (:background "#161616" :foreground "burlywood3"))))
 '(mode-line-inactive ((t (:background "#161616" :foreground "gray50"))))

 ;; Line numbers (inherit from default so they scale with zoom)
 '(line-number ((t (:inherit default :foreground "gray50"))))
 '(line-number-current-line ((t (:inherit default :foreground "burlywood3"))))

 ;; Minibuffer
 '(minibuffer-prompt ((t (:foreground "burlywood3")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'casey)

;;; casey-theme.el ends here
