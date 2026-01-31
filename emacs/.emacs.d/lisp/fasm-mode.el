;;; fasm-mode.el --- FASM major mode -*- lexical-binding: t; -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/fasm-mode
;; Version: 0.1.11

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2013-2014, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(defvar fasm-mode-syntax-table
  (let ((syntaxtable (make-syntax-table)))
    (modify-syntax-entry ?_ "_" syntaxtable)
    (modify-syntax-entry ?. "_" syntaxtable)
    (modify-syntax-entry ?$ "_" syntaxtable)
    (modify-syntax-entry ?@ "_" syntaxtable)
    (modify-syntax-entry ?~ "_" syntaxtable)
    (modify-syntax-entry ?? "_" syntaxtable)
    (modify-syntax-entry ?! "_" syntaxtable)
    (modify-syntax-entry ?= "." syntaxtable)
    (modify-syntax-entry ?+ "." syntaxtable)
    (modify-syntax-entry ?- "." syntaxtable)
    (modify-syntax-entry ?* "." syntaxtable)
    (modify-syntax-entry ?/ "." syntaxtable)
    (modify-syntax-entry ?\\ "." syntaxtable)
    (modify-syntax-entry ?\; "<" syntaxtable)
    (modify-syntax-entry ?\n ">" syntaxtable)
    (modify-syntax-entry ?\" "\"" syntaxtable)
    (modify-syntax-entry ?\' "\"" syntaxtable)
    syntaxtable)
  "Syntax table for FASM mode.")

(defvar fasm-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Local keymap for FASM mode.")

(defvar fasm-basic-offset 2
  "Offset for FASM mode indentation.")

(defmacro fasm--regexp-from-keywords (&rest keywords)
  (rx-to-string `(and symbol-start (or ,@keywords) symbol-end)))

(defconst fasm-font-lock-keywords
  `(;; Numbers
    (,(rx (and symbol-start
               (or (and (+ (any "0" "1"))
                        "b")
                   (and (any "0-9")
                        (* (any "0-9" "a-f" "A-F"))
                        "h")
                   (and (or "0x" "$")
                        (+ (any "0-9" "a-f" "A-F")))
                   (and (+ (any "0-9"))
                        (? (and "."
                                (* (any "0-9"))))
                        (? (and (any "e" "E")
                                (? (any "+" "-"))
                                (+ (any "0-9"))))))
               symbol-end))
     . font-lock-constant-face)
    ;; Types
    (,(fasm--regexp-from-keywords
       "byte" "word" "dword" "fword" "pword" "qword" "tbyte" "tword" "dqword"
       "xword" "qqword" "yword" "db" "rb" "dw" "du" "rw" "dd" "rd" "df" "dp"
       "rf" "rp" "dq" "rq" "dt" "rt")
     . font-lock-type-face)
    ;; Directives and operators
    (,(fasm--regexp-from-keywords
       "mod" "rva" "plt" "align" "as" "at" "defined" "dup" "eq" "eqtype" "from"
       "ptr" "relativeto" "used" "binary" "export" "fixups" "import" "native"
       "static" "console" "dynamic" "efiboot" "linkinfo" "readable" "resource"
       "writable" "shareable" "writeable" "efiruntime" "executable" "linkremove"
       "discardable" "interpreter" "notpageable" "if" "end" "err" "org" "data"
       "else" "heap" "load" "align" "break" "entry" "extrn" "label" "stack"
       "store" "times" "while" "assert" "format" "public" "repeat" "display"
       "section" "segment" "virtual" "file")
     . font-lock-keyword-face)
    ;; Preprocessor directives
    (,(fasm--regexp-from-keywords
       "define" "include" "irp" "irps" "macro" "match" "purge" "rept" "restore"
       "restruc" "struc" "common" "forward" "local" "reverse" "equ" "fix")
     . font-lock-preprocessor-face)
    ;; Registers
    (,(fasm--regexp-from-keywords
       "al" "bl" "cl" "dl" "spl" "bpl" "sil" "dil" "r8b" "r9b" "r10b" "r11b"
       "r12b" "r13b" "r14b" "r15b" "ah" "bh" "ch" "dh" "ax" "bx" "cx" "dx" "sp"
       "bp" "si" "di" "r8w" "r9w" "r10w" "r11w" "r12w" "r13w" "r14w" "r15w"
       "eax" "ebx" "ecx" "edx" "esp" "ebp" "esi" "edi" "r8d" "r9d" "r10d" "r11d"
       "r12d" "r13d" "r14d" "r15d" "rax" "rbx" "rcx" "rdx" "rsp" "rbp" "rsi"
       "rdi" "r8" "r9" "r10" "r11" "r12" "r13" "r14" "r15" "rip" "es" "cs" "ss"
       "ds" "fs" "gs" "cr0" "cr2" "cr3" "cr4" "dr0" "dr1" "dr2" "dr3" "st0"
       "st1" "st2" "st3" "st4" "st5" "st6" "st7" "mm0" "mm1" "mm2" "mm3" "mm4"
       "mm5" "mm6" "mm7" "xmm0" "xmm1" "xmm2" "xmm3" "xmm4" "xmm5" "xmm6" "xmm7"
       "xmm8" "xmm9" "xmm10" "xmm11" "xmm12" "xmm13" "xmm14" "xmm15" "ymm0"
       "ymm1" "ymm2" "ymm3" "ymm4" "ymm5" "ymm6" "ymm7" "ymm8" "ymm9" "ymm10"
       "ymm11" "ymm12" "ymm13" "ymm14" "ymm15")
     . font-lock-variable-name-face)
    ;; Instructions (abbreviated for readability - full list in original)
    (,(fasm--regexp-from-keywords
       "bt" "in" "ja" "jb" "jc" "je" "jg" "jl" "jo" "jp" "js" "jz" "or" "aaa"
       "aad" "aam" "aas" "adc" "add" "and" "bsf" "bsr" "btc" "btr" "bts" "cbw"
       "cdq" "clc" "cld" "cli" "cmc" "cmp" "cqo" "cwd" "daa" "das" "dec" "div"
       "fld" "fst" "hlt" "inc" "ins" "int" "jae" "jbe" "jge" "jle" "jmp" "jna"
       "jnb" "jnc" "jne" "jng" "jnl" "jno" "jnp" "jns" "jnz" "jpe" "jpo" "lar"
       "lds" "lea" "les" "lfs" "lgs" "lsl" "lss" "ltr" "mov" "mul" "neg" "nop"
       "not" "out" "pop" "por" "rcl" "rcr" "rep" "ret" "rol" "ror" "rsm" "sal"
       "sar" "sbb" "shl" "shr" "stc" "std" "sti" "str" "sub" "ud2" "xor"
       "call" "push" "test" "leave" "enter" "cpuid" "rdtsc" "syscall" "sysenter"
       "movzx" "movsx" "movsxd" "cmova" "cmovb" "cmovc" "cmove" "cmovg" "cmovl"
       "cmovo" "cmovp" "cmovs" "cmovz" "cmovae" "cmovbe" "cmovge" "cmovle"
       "cmovna" "cmovnb" "cmovnc" "cmovne" "cmovng" "cmovnl" "cmovno" "cmovnp"
       "cmovns" "cmovnz" "seta" "setb" "setc" "sete" "setg" "setl" "seto"
       "setp" "sets" "setz" "setae" "setbe" "setge" "setle" "setna" "setnb"
       "setnc" "setne" "setng" "setnl" "setno" "setnp" "setns" "setnz"
       "imul" "idiv" "cmpxchg" "xchg" "xadd" "lock" "pause"
       ;; SSE/AVX
       "movaps" "movups" "movapd" "movupd" "movdqa" "movdqu"
       "addps" "addpd" "addss" "addsd" "subps" "subpd" "subss" "subsd"
       "mulps" "mulpd" "mulss" "mulsd" "divps" "divpd" "divss" "divsd"
       "sqrtps" "sqrtpd" "sqrtss" "sqrtsd"
       "pxor" "por" "pand" "pandn" "pcmpeqb" "pcmpeqw" "pcmpeqd"
       "vaddps" "vaddpd" "vsubps" "vsubpd" "vmulps" "vmulpd" "vdivps" "vdivpd")
     . font-lock-builtin-face)
    ;; Labels
    (,(rx (and line-start
               (* (any " " "\t"))
               (group (and (any "a-z" "A-Z" "0-9" "." "?" "!" "@")
                           (* (or (syntax word)
                                  (syntax symbol)))))
               ":"))
     1 font-lock-function-name-face)
    ;; Macro names
    (,(rx (and (or "macro" "struct")
               (+ (any " " "\t"))
               (group (and (any "a-z" "A-Z" "0-9" "." "?" "!" "@")
                           (* (or (syntax word)
                                  (syntax symbol)))))))
     1 font-lock-function-name-face))
  "Syntax highlighting for FASM mode.")

(defun fasm--get-indent-level (lineoffset)
  (save-excursion
    (forward-line (1- lineoffset))
    (back-to-indentation)
    (current-column)))

(defun fasm-indent-line ()
  "Indent according to FASM major mode."
  (interactive)
  (let ((previndent (fasm--get-indent-level 0))
        (currindent (fasm--get-indent-level 1)))
    (if (or (> previndent currindent)
            (memq this-command '(newline-and-indent evil-ret-and-indent)))
        (indent-to previndent)
      (indent-to (* fasm-basic-offset (1+ (/ currindent fasm-basic-offset)))))))

;; Emacs < 24 did not have prog-mode
(defalias 'fasm-parent-mode
  (if (fboundp 'prog-mode) #'prog-mode #'fundamental-mode))

(defmacro fasm--set-local (variable value)
  `(set (make-local-variable ',variable) ,value))

;;;###autoload
(define-derived-mode fasm-mode fasm-parent-mode "Fasm"
  "Major mode for editing assembly in FASM format."
  (fasm--set-local font-lock-defaults '(fasm-font-lock-keywords nil t))
  (fasm--set-local indent-line-function #'fasm-indent-line)
  (fasm--set-local comment-use-syntax t)
  (fasm--set-local comment-start ";")
  (fasm--set-local comment-end "")
  (fasm--set-local comment-start-skip ";+[ \t]*")
  (fasm--set-local comment-column 0))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(fasm\\|inc\\)\\'" . fasm-mode))

(provide 'fasm-mode)

;;; fasm-mode.el ends here
