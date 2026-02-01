;;; naysayer-theme.el --- Jon Blow's color theme -*- lexical-binding: t; -*-

;; Author: Based on Jon Blow's Emacs configuration
;; URL: https://vegard.wiki/w/Jon_Blow_emacs_colorscheme

;;; Commentary:
;; Dark teal theme based on Jon Blow's Emacs configuration from his streams.

;;; Code:

(deftheme naysayer "Jon Blow's Naysayer theme")

(let ((bg         "#062329")
      (bg-hl      "#0b3335")
      (bg-region  "#0000ff")
      (fg         "#d1b897")
      (fg-dim     "#126367")
      (comment    "#44b340")
      (string     "#2ec09c")
      (constant   "#7ad0c6")
      (keyword    "#ffffff")
      (type       "#8cde94")
      (func       "#ffffff")
      (variable   "#c1d1e3")
      (preproc    "#b0ffb0")
      (warning    "#ffaa00")
      (error      "#ff0000")
      (cursor     "#90c090")
      (match      "#ffbb00"))

  (custom-theme-set-faces
   'naysayer

   ;; Basic faces
   `(default ((t (:background ,bg :foreground ,fg))))
   `(cursor ((t (:background ,cursor))))
   `(region ((t (:background ,bg-region))))
   `(hl-line ((t (:background ,bg-hl))))
   `(fringe ((t (:background ,bg))))
   `(vertical-border ((t (:foreground ,fg-dim))))

   ;; Font lock faces (syntax highlighting)
   `(font-lock-builtin-face ((t (:foreground ,type))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
   `(font-lock-constant-face ((t (:foreground ,constant))))
   `(font-lock-doc-face ((t (:foreground ,comment))))
   `(font-lock-function-name-face ((t (:foreground ,func))))
   `(font-lock-keyword-face ((t (:foreground ,keyword))))
   `(font-lock-preprocessor-face ((t (:foreground ,preproc))))
   `(font-lock-string-face ((t (:foreground ,string))))
   `(font-lock-type-face ((t (:foreground ,type))))
   `(font-lock-variable-name-face ((t (:foreground ,variable))))

   ;; Special characters
   `(font-lock-warning-face ((t (:foreground ,error))))
   `(escape-glyph ((t (:foreground ,warning))))

   ;; Function calls
   `(font-lock-function-call-face ((t (:foreground ,func))))

   ;; Mode line
   `(mode-line ((t (:background "#0a3030" :foreground ,fg))))
   `(mode-line-inactive ((t (:background "#0a3030" :foreground ,fg-dim))))

   ;; Line numbers
   `(line-number ((t (:inherit default :foreground ,fg-dim))))
   `(line-number-current-line ((t (:inherit default :foreground ,fg))))

   ;; Margins and borders
   `(header-line ((t (:background ,bg-hl :foreground ,fg))))

   ;; Highlighting
   `(highlight ((t (:background ,bg-region))))
   `(lazy-highlight ((t (:background "#184d68"))))
   `(isearch ((t (:background ,match :foreground ,bg))))
   `(isearch-fail ((t (:background ,error :foreground ,bg))))

   ;; Matching parens
   `(show-paren-match ((t (:background ,bg-hl :foreground ,cursor))))
   `(show-paren-mismatch ((t (:background ,error :foreground ,bg))))

   ;; Secondary selection
   `(secondary-selection ((t (:background "#184d68"))))

   ;; Minibuffer
   `(minibuffer-prompt ((t (:foreground ,constant))))

   ;; Completions
   `(completions-common-part ((t (:foreground ,keyword))))
   `(completions-first-difference ((t (:foreground ,type))))

   ;; Links
   `(link ((t (:foreground ,string :underline t))))
   `(link-visited ((t (:foreground ,fg-dim :underline t))))

   ;; Errors and warnings
   `(error ((t (:foreground ,error))))
   `(warning ((t (:foreground ,warning))))
   `(success ((t (:foreground ,comment))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'naysayer)

;;; naysayer-theme.el ends here
