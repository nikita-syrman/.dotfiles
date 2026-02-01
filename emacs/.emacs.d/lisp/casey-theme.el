;;; casey-theme.el --- Casey Muratori's Handmade Hero color theme -*- lexical-binding: t; -*-

;; Author: Casey Muratori (adapted from handmadehero/.emacs)
;; URL: https://github.com/ecxr/handmadehero/blob/master/misc/.emacs

;;; Commentary:
;; Dark theme based on Casey Muratori's Emacs configuration from Handmade Hero.

;;; Code:

(deftheme casey "Casey Muratori's Handmade Hero theme")

(custom-theme-set-faces
 'casey

 ;; Basic faces (from 4coder theme)
 '(default ((t (:background "#161616" :foreground "#A08563"))))
 '(cursor ((t (:background "#40FF40"))))
 '(region ((t (:background "#703419"))))
 '(hl-line ((t (:background "#121E12"))))
 '(fringe ((t (:background "#161616"))))
 '(vertical-border ((t (:foreground "#262626"))))

 ;; Font lock faces (syntax highlighting)
 '(font-lock-builtin-face ((t (:foreground "#A08563"))))
 '(font-lock-comment-face ((t (:foreground "#7D7D7D"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#7D7D7D"))))
 '(font-lock-constant-face ((t (:foreground "#6B8E23"))))
 '(font-lock-doc-face ((t (:foreground "#7D7D7D"))))
 '(font-lock-function-name-face ((t (:foreground "#CC7755"))))
 '(font-lock-keyword-face ((t (:foreground "#CD950C"))))
 '(font-lock-preprocessor-face ((t (:foreground "#DAB98F"))))
 '(font-lock-string-face ((t (:foreground "#6B8E23"))))
 '(font-lock-type-face ((t (:foreground "#D4A86A"))))
 '(font-lock-variable-name-face ((t (:foreground "#A08563"))))

 ;; Special characters
 '(font-lock-warning-face ((t (:foreground "#FF0000"))))
 '(escape-glyph ((t (:foreground "#FF0000"))))

 ;; Macros / function calls
 '(font-lock-function-call-face ((t (:foreground "#CC7755"))))

 ;; Mode line
 '(mode-line ((t (:background "#262329" :foreground "#A0823B"))))
 '(mode-line-inactive ((t (:background "#262329" :foreground "#7D7D7D"))))

 ;; Line numbers (inherit from default so they scale with zoom)
 '(line-number ((t (:inherit default :foreground "#484848"))))
 '(line-number-current-line ((t (:inherit default :foreground "#A08563"))))

 ;; Margins and borders
 '(header-line ((t (:background "#262626" :foreground "#A08563"))))

 ;; Highlighting
 '(highlight ((t (:background "#703419"))))
 '(lazy-highlight ((t (:background "#703419"))))
 '(isearch ((t (:background "#FFBB00" :foreground "#161616"))))
 '(isearch-fail ((t (:background "#FF0000" :foreground "#161616"))))

 ;; Matching parens
 '(show-paren-match ((t (:background "#404040" :foreground "#40FF40"))))
 '(show-paren-mismatch ((t (:background "#FF0000" :foreground "#161616"))))

 ;; Secondary selection (for paste preview, etc.)
 '(secondary-selection ((t (:background "#80005D"))))

 ;; Minibuffer
 '(minibuffer-prompt ((t (:foreground "#A0823B"))))

 ;; Completions
 '(completions-common-part ((t (:foreground "#CD950C"))))
 '(completions-first-difference ((t (:foreground "#DAB98F"))))

 ;; Links
 '(link ((t (:foreground "#456E67" :underline t))))
 '(link-visited ((t (:foreground "#7D7D7D" :underline t))))

 ;; Errors and warnings
 '(error ((t (:foreground "#FF4040"))))
 '(warning ((t (:foreground "#FFBB00"))))
 '(success ((t (:foreground "#40FF40")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'casey)

;;; casey-theme.el ends here
