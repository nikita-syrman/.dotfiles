;;; fastc-mode.el --- Fast C/C++ editing mode -*- lexical-binding: t; -*-

;; Author: fogs
;; Based on: simpc-mode by Alexey Kutepov <reximkut@gmail.com>
;;           https://github.com/rexim/simpc-mode
;;
;; Original simpc-mode copyright:
;; Copyright 2020 Alexey Kutepov <reximkut@gmail.com>
;;
;; SPDX-License-Identifier: MIT
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;
;; Extensions by fogs:
;; - Enhanced indentation for braceless control statements (if/for/while/else/do)
;; - Multi-line macro support with proper backslash handling
;; - Auto-alignment of continuation backslashes
;; - Proper indentation inside macro blocks (do { } while patterns)

;;; Code:

(require 'subr-x)

;;; Customization

(defgroup fastc nil
  "Fast C/C++ mode for quick editing."
  :group 'languages)

(defcustom fastc-indent-level 4
  "Number of spaces for each indentation level."
  :type 'integer
  :group 'fastc)

(defcustom fastc-backslash-padding 1
  "Minimum spaces before backslash in multi-line macros."
  :type 'integer
  :group 'fastc)

;;; Syntax Table

(defvar fastc-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C/C++ style comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; Preprocessor
    (modify-syntax-entry ?# "." table)
    ;; Chars are the same as strings
    (modify-syntax-entry ?' "\"" table)
    ;; Treat <> as punctuation (for templates)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?% "." table)
    table)
  "Syntax table for `fastc-mode'.")

;;; Font Lock (Syntax Highlighting)

(defun fastc--types ()
  "Return list of C/C++ type keywords."
  '("char" "int" "long" "short" "void" "bool" "float" "double" "signed" "unsigned"
    "char16_t" "char32_t" "char8_t"
    "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t" "int64_t" "uint64_t"
    "uintptr_t" "size_t" "ptrdiff_t" "va_list"))

(defun fastc--keywords ()
  "Return list of C/C++ keywords."
  '("auto" "break" "case" "const" "continue" "default" "do"
    "else" "enum" "extern" "for" "goto" "if" "register"
    "return" "sizeof" "static" "struct" "switch" "typedef"
    "union" "volatile" "while" "alignas" "alignof" "and"
    "and_eq" "asm" "atomic_cancel" "atomic_commit" "atomic_noexcept" "bitand"
    "bitor" "catch" "class" "co_await"
    "co_return" "co_yield" "compl" "concept" "const_cast" "consteval" "constexpr"
    "constinit" "decltype" "delete" "dynamic_cast" "explicit" "export" "false"
    "friend" "inline" "mutable" "namespace" "new" "noexcept" "not" "not_eq"
    "nullptr" "operator" "or" "or_eq" "private" "protected" "public" "reflexpr"
    "reinterpret_cast" "requires" "static_assert" "static_cast" "synchronized"
    "template" "this" "thread_local" "throw" "true" "try" "typeid" "typename"
    "using" "virtual" "wchar_t" "xor" "xor_eq"))

(defun fastc--font-lock-keywords ()
  "Return font-lock keywords for `fastc-mode'."
  (list
   `("# *\\(warn\\|error\\)" . font-lock-warning-face)
   `("# *[#a-zA-Z0-9_]+" . font-lock-preprocessor-face)
   `("# *include\\(?:_next\\)?\\s-+\\(\\(<\\|\"\\).*\\(>\\|\"\\)\\)" . (1 font-lock-string-face))
   `("\\(?:enum\\|struct\\)\\s-+\\([a-zA-Z0-9_]+\\)" . (1 font-lock-type-face))
   `(,(regexp-opt (fastc--keywords) 'symbols) . font-lock-keyword-face)
   `(,(regexp-opt (fastc--types) 'symbols) . font-lock-type-face)))

;;; Indentation Helpers

(defun fastc--previous-non-empty-line ()
  "Return (line-contents . indentation) of previous non-empty line, or nil."
  (save-excursion
    (move-beginning-of-line nil)
    (if (bobp)
        nil
      (forward-line -1)
      (while (and (not (bobp))
                  (string-empty-p (string-trim-right (thing-at-point 'line t))))
        (forward-line -1))
      (if (string-empty-p (string-trim-right (thing-at-point 'line t)))
          nil
        (cons (thing-at-point 'line t)
              (current-indentation))))))

(defun fastc--nth-prev-non-empty-line (n)
  "Return the Nth previous non-empty line as (line . indentation) or nil."
  (save-excursion
    (move-beginning-of-line nil)
    (let ((count 0))
      (while (and (< count n) (not (bobp)))
        (forward-line -1)
        (unless (string-empty-p (string-trim-right (thing-at-point 'line t)))
          (setq count (1+ count))))
      (if (= count n)
          (cons (thing-at-point 'line t) (current-indentation))
        nil))))

(defun fastc--line-ends-with-backslash-p (line)
  "Check if LINE ends with a backslash (macro continuation)."
  (and line (string-suffix-p "\\" (string-trim-right line))))

(defun fastc--strip-trailing-backslash (line)
  "Remove trailing \\ and whitespace before it from LINE for analysis."
  (let ((trimmed (string-trim-right line)))
    (if (string-suffix-p "\\" trimmed)
        (string-trim-right (substring trimmed 0 -1))
      trimmed)))

(defun fastc--is-braceless-control-p (line)
  "Check if LINE is a braceless control statement (if/for/while/else/do).
The line must end with ) to be a complete condition."
  (when line
    (let ((trimmed (string-trim (fastc--strip-trailing-backslash line))))
      (and (string-match-p "^\\(if\\|for\\|while\\|else\\s+if\\|else\\|do\\)\\b" trimmed)
           (string-suffix-p ")" trimmed)
           (not (string-suffix-p "{" trimmed))
           (not (string-suffix-p ";" trimmed))))))

(defun fastc--goto-prev-non-empty-line ()
  "Move to previous non-empty line. Return t if found, nil otherwise."
  (forward-line -1)
  (while (and (not (bobp))
              (string-blank-p (thing-at-point 'line t)))
    (forward-line -1))
  (not (string-blank-p (thing-at-point 'line t))))

(defun fastc--ends-braceless-control-p ()
  "Check if current line ends a braceless control condition.
Returns the indentation of the control statement, or nil.
Works for both single-line and multi-line conditions."
  (save-excursion
    (end-of-line)
    (skip-chars-backward " \t\\\\")
    (when (eq (char-before) ?\))
      (ignore-errors
        (backward-list)
        (let ((opener-line (string-trim (fastc--strip-trailing-backslash
                                         (thing-at-point 'line t)))))
          (when (string-match-p "^\\(if\\|for\\|while\\|else\\s+if\\|else\\|do\\)\\b" opener-line)
            (current-indentation)))))))

(defun fastc--find-braceless-base-indent ()
  "Find the base indent after exiting nested braceless control bodies.
Goes back through prev-prev, and if that was a braceless control
that was itself a body of another braceless control, keeps going
until finding the outermost control's indent."
  (save-excursion
    (let ((result-indent nil))
      ;; Go to prev-prev (the braceless control whose body just ended)
      (fastc--goto-prev-non-empty-line)
      (fastc--goto-prev-non-empty-line)
      (let* ((line (string-trim (fastc--strip-trailing-backslash
                                 (thing-at-point 'line t))))
             (ends-control (fastc--ends-braceless-control-p)))
        ;; Get the indent - use ends-control value for multi-line conditions
        (when (or (fastc--is-braceless-control-p line) ends-control)
          (setq result-indent (or ends-control (current-indentation)))
          ;; Check if this control was itself a body of outer control
          (while (and (fastc--goto-prev-non-empty-line)
                      (let* ((prev-line (string-trim (fastc--strip-trailing-backslash
                                                      (thing-at-point 'line t))))
                             (prev-ends (fastc--ends-braceless-control-p))
                             (prev-ctrl-indent (or prev-ends
                                                   (and (fastc--is-braceless-control-p prev-line)
                                                        (current-indentation)))))
                        (when (and prev-ctrl-indent
                                   (< prev-ctrl-indent result-indent))
                          (setq result-indent prev-ctrl-indent)
                          t)))  ;; continue loop
            )))
      result-indent)))

(defun fastc--unclosed-paren-column ()
  "Return column to align to if inside unclosed parentheses, or nil.
Uses `syntax-ppss` to find unclosed parens, ignoring those in strings/comments.
Returns column after the innermost unclosed `(`, or paren-col + indent-level
if nothing follows the paren on that line.
Only aligns when previous line ends with continuation chars (comma, paren, &&, ||)."
  (save-excursion
    (beginning-of-line)
    (let* ((ppss (syntax-ppss))
           (paren-depth (nth 0 ppss))
           (innermost-paren-pos (nth 1 ppss)))
      (when (and (> paren-depth 0)
                 innermost-paren-pos
                 (eq (char-after innermost-paren-pos) ?\())
        ;; Check that previous line ends with continuation char
        ;; Include ; for for-loop parts (safe because we already checked paren depth)
        (let ((prev-continues (save-excursion
                                (when (fastc--goto-prev-non-empty-line)
                                  (end-of-line)
                                  (skip-chars-backward " \t\\\\")
                                  (or (memq (char-before) '(?, ?\( ?\;))
                                      (and (>= (- (point) 2) (point-min))
                                           (string-match-p "\\(&&\\|||\\)$"
                                                           (buffer-substring (- (point) 2) (point)))))))))
          (when prev-continues
            (save-excursion
              (goto-char innermost-paren-pos)
              (let ((paren-col (current-column)))
                ;; Move past the opening paren
                (forward-char 1)
                ;; Skip whitespace to find first argument
                (skip-chars-forward " \t")
                (if (or (eolp)
                        (eq (char-after) ?\\))
                    ;; Nothing after paren on this line, use paren + indent
                    (+ paren-col fastc-indent-level)
                  ;; Align to first argument
                  (current-column))))))))))

;;; Indentation Logic

(defun fastc--desired-indentation ()
  "Calculate desired indentation for current line."
  (let ((prev (fastc--previous-non-empty-line)))
    (if (not prev)
        (current-indentation)
      (let* ((indent-len fastc-indent-level)
             (cur-line-raw (string-trim-right (thing-at-point 'line t)))
             (prev-line-raw (string-trim-right (car prev)))
             (cur-line (fastc--strip-trailing-backslash cur-line-raw))
             (prev-line (fastc--strip-trailing-backslash prev-line-raw))
             (prev-indent (cdr prev))
             (prev-prev (fastc--nth-prev-non-empty-line 2)))
        (cond
         ;; Preprocessor directives always at column 0
         ((string-match-p "^\\s-*#" cur-line)
          0)

         ;; Align to unclosed parenthesis
         ((fastc--unclosed-paren-column))

         ;; After line ending with = (string/initializer continuation)
         ((string-match-p "=\\s-*$" prev-line)
          (+ prev-indent fastc-indent-level))

         ;; String literal continuation (prev ends with string, no semicolon)
         ((and (string-match-p "\"\\s-*$" prev-line)
               (not (string-suffix-p ";" prev-line)))
          prev-indent)

         ;; After string block ends, return to base indent
         ((and (string-match-p "^\\s-*\"" prev-line)  ; prev is standalone string
               (string-suffix-p ";" prev-line)        ; ending the statement
               (not (string-match-p "^\\s-*\"" cur-line))) ; current is not string
          (save-excursion
            (fastc--goto-prev-non-empty-line)
            ;; Go back through string lines to find start
            (while (and (string-match-p "^\\s-*\"" (thing-at-point 'line t))
                        (fastc--goto-prev-non-empty-line)))
            ;; Now we're at the line with = or the line before strings
            (current-indentation)))

         ;; switch - don't indent the body directly
         ((string-match-p "^\\s-*switch\\s-*(.+)" prev-line)
          prev-indent)

         ;; { followed by } - same level
         ((and (string-suffix-p "{" prev-line)
               (string-prefix-p "}" (string-trim-left cur-line)))
          prev-indent)

         ;; { increases indent
         ((string-suffix-p "{" prev-line)
          (+ prev-indent indent-len))

         ;; } decreases indent
         ((string-prefix-p "}" (string-trim-left cur-line))
          (let ((base (save-excursion
                        (when (fastc--goto-prev-non-empty-line)
                          (end-of-line)
                          (skip-chars-backward " \t\\\\")
                          (when (eq (char-before) ?\;) (backward-char))
                          (when (eq (char-before) ?\))
                            (ignore-errors
                              (backward-list)
                              (current-indentation)))))))
            (max (- (or base prev-indent) indent-len) 0)))

         ;; { after braceless control - don't indent (brace goes at control level)
         ((and (string-prefix-p "{" (string-trim-left cur-line))
               (or (fastc--is-braceless-control-p prev-line)
                   (save-excursion
                     (fastc--goto-prev-non-empty-line)
                     (fastc--ends-braceless-control-p))))
          (or (save-excursion
                (fastc--goto-prev-non-empty-line)
                (fastc--ends-braceless-control-p))
              prev-indent))

         ;; Braceless control statement - indent next line
         ((fastc--is-braceless-control-p prev-line)
          (+ prev-indent indent-len))

         ;; 'else' after braceless if body - align with the if
         ((and (string-match-p "^\\s-*else\\b" (string-trim-left cur-line))
               prev-prev
               (or (fastc--is-braceless-control-p (car prev-prev))
                   (save-excursion
                     (fastc--goto-prev-non-empty-line)
                     (fastc--goto-prev-non-empty-line)
                     (fastc--ends-braceless-control-p))))
          (or (save-excursion
                (fastc--goto-prev-non-empty-line)
                (fastc--goto-prev-non-empty-line)
                (fastc--ends-braceless-control-p))
              (cdr prev-prev)))

         ;; After body of braceless control - dedent
         ;; Handles nested braceless controls by finding outermost control's indent
         ((and prev-prev
               (or (fastc--is-braceless-control-p (car prev-prev))
                   (save-excursion
                     (fastc--goto-prev-non-empty-line)
                     (fastc--goto-prev-non-empty-line)
                     (fastc--ends-braceless-control-p)))
               (not (fastc--is-braceless-control-p prev-line))
               (not (save-excursion
                      (fastc--goto-prev-non-empty-line)
                      (fastc--ends-braceless-control-p))))
          (fastc--find-braceless-base-indent))

         ;; case/label colon handling
         ((string-suffix-p ":" prev-line)
          (if (string-suffix-p ":" cur-line)
              prev-indent
            (+ prev-indent indent-len)))
         ((string-suffix-p ":" cur-line)
          (max (- prev-indent indent-len) 0))

         ;; First line after #define - indent
         ((and (fastc--line-ends-with-backslash-p prev-line-raw)
               (string-match-p "^\\s-*#" prev-line))
          indent-len)

         ;; Just finished multi-line macro - back to column 0
         ((and prev-prev
               (fastc--line-ends-with-backslash-p (car prev-prev))
               (not (fastc--line-ends-with-backslash-p prev-line-raw))
               (not (string-match-p "^\\s-*#" prev-line)))
          0)

         ;; After closing a multi-line paren, return to opener's indentation
         ;; (or opener's indent + indent-len if it's a braceless control)
         ((let ((control-indent (save-excursion
                                  (when (fastc--goto-prev-non-empty-line)
                                    (fastc--ends-braceless-control-p)))))
            (when control-indent
              (+ control-indent indent-len))))

         ;; After closing a regular multi-line paren (not control), return to base
         ((save-excursion
            (when (fastc--goto-prev-non-empty-line)
              (end-of-line)
              (skip-chars-backward " \t\\\\")
              (when (eq (char-before) ?\;) (backward-char))
              (when (eq (char-before) ?\))
                (ignore-errors
                  (backward-list)
                  (current-indentation))))))

         ;; Default - keep previous indentation
         (t prev-indent))))))

;;; Backslash Alignment

(defun fastc--get-content-end-column ()
  "Get column where actual content ends (before trailing whitespace and \\)."
  (save-excursion
    (end-of-line)
    (skip-chars-backward " \t\\\\")
    (current-column)))

(defun fastc--scan-macro-for-max-column ()
  "Scan current macro block and return max content end column."
  (save-excursion
    (beginning-of-line)
    (let ((max-col 0))
      ;; Go up to find start of macro
      (while (and (not (bobp))
                  (let ((prev-line (save-excursion
                                     (forward-line -1)
                                     (thing-at-point 'line t))))
                    (and prev-line
                         (or (string-suffix-p "\\" (string-trim-right prev-line))
                             (string-match-p "^\\s-*#define" prev-line)))))
        (forward-line -1))
      ;; Scan forward through the macro
      (while (let ((line (thing-at-point 'line t)))
               (and line
                    (or (string-suffix-p "\\" (string-trim-right line))
                        (string-match-p "^\\s-*#define" line))))
        (setq max-col (max max-col (fastc--get-content-end-column)))
        (forward-line 1))
      max-col)))

(defun fastc--align-backslash ()
  "If current line ends with \\, align based on macro block's max width."
  (save-excursion
    (end-of-line)
    (when (and (> (point) (line-beginning-position))
               (eq (char-before) ?\\))
      (backward-char 1)
      (delete-horizontal-space)
      (let* ((max-col (fastc--scan-macro-for-max-column))
             (current-col (current-column))
             (target-col (+ max-col fastc-backslash-padding))
             (spaces-needed (max 1 (- target-col current-col))))
        (insert (make-string spaces-needed ?\s))))))

;;; Indent Function

(defun fastc-indent-line ()
  "Indent current line according to fastc-mode rules."
  (interactive)
  (when (not (bobp))
    (let* ((desired-indentation (fastc--desired-indentation))
           (n (max (- (current-column) (current-indentation)) 0)))
      (indent-line-to desired-indentation)
      (forward-char n)
      (fastc--align-backslash))))

;;; Mode Definition

;;;###autoload
(define-derived-mode fastc-mode prog-mode "FastC"
  "Fast major mode for editing C/C++ files.
Optimized for speed with lightweight indentation."
  :syntax-table fastc-mode-syntax-table
  (setq-local font-lock-defaults '(fastc--font-lock-keywords))
  (setq-local indent-line-function 'fastc-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local comment-start "// ")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(c\\|h\\|cc\\|hh\\|cpp\\|hpp\\|cxx\\|hxx\\)\\'" . fastc-mode))

(provide 'fastc-mode)

;;; fastc-mode.el ends here
