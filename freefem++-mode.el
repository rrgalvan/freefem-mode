;;; freefem++-mode.el --- Major mode for the FreeFem++ language

;; Copyright © 2014-2017  Mark Oteiza <mvoteiza@udel.edu>
;; Copyright © 2008-2011  J. Rafael Rodríguez Galván

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;;         J. Rafael Rodríguez Galván
;; Created: 25 Jan 2014
;; Version: 0.3
;; Keywords: languages

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Inspired by `freefem++-mode.el' by J. Rafael Rodríguez Galván.
;; <https://raw.githubusercontent.com/cucharro/emacs/master/freefem++-mode.el>

;; FreeFem++ <http://www.freefem.org/ff++/> is a partial differential
;; equation solver. It has its own language. FreeFem++ scripts can
;; solve multiphysics nonlinear systems in 2D and 3D.

;;; Code:

(defgroup freefem++ nil
  "Support for the FreeFem++ language."
  :group 'languages
  :link '(url-link "http://www.freefem.org/ff++/")
  :prefix "freefem++-")

(require 'cc-mode)
(require 'compile)

(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  (c-add-language 'freefem++-mode 'c++-mode))


;;; Lexer-level syntax (identifiers, tokens etc).

(c-lang-defconst c-symbol-chars
  ;; No underscore or other symbols permitted in identifiers
  freefem++ c-alnum)

(c-lang-defconst c-cpp-include-directives
  freefem++ '("include" "load"))

(c-lang-defconst c-opt-cpp-macro-define
  freefem++ "macro")


;; Additional constants for parser-level constructs.

(c-lang-defconst c-type-decl-prefix-key
  ;; No pointers, casting, or &= operator, only references
  freefem++ (concat "\\(&\\)\\([^=]\\|$\\)"))

(c-lang-defconst c-opt-type-suffix-key
  ;; Arrays
  freefem++ "\\(\\[[^]]*\\]\\|\\[\<^\>]*\\]\\)")


;; Keyword lists.

(c-lang-defconst c-primitive-type-kwds
  freefem++ '("bool" "border" "Cmatrix" "complex" "func" "ifstream"
              "int" "macro" "matrix" "mesh" "mesh3" "ofstream"
              "problem" "real" "R3" "solve" "string" "varf"))

(c-lang-defconst c-primitive-type-prefix-kwds
  ;; Both here and in `c-primitive-type-kwds'
  freefem++ '("func"))

(c-lang-defconst c-paren-nontype-kwds
  freefem++ '("int1d" "int2d" "int3d" "intalledges" "on" "jump" "mean"
              "dx" "dxx" "dxy" "dy" "dyx" "dyy" "dz"
              "interpolate" "set" "plot"))

(c-lang-defconst c-constant-kwds
  freefem++ '("true" "false" "pi"
              ;; Reserved
              "P" "N"
              ;; Finite elements
              "P0" "P1" "P2" "P3" "P4"
              "P0VF" "P1nc" "P1dc" "P2dc"
              "P1b" "P2b" "P2h"
              "RT0" "RT1" "BDM1" "RT0Ortho" "RT1Ortho" "BDM1Ortho" "RTmodif"
              "P0edge" "P1edge" "P2edge" "P3edge" "P4edge" "P5edge"
              "P03d" "P13d" "P1b3d" "P23d" "RT03d" "Edge03d"
              ;; Solvers
              "LU" "Cholesky" "Crout" "CG" "GMRES" "UMFPACK" "sparsesolver"
              ;; Quadrature finite elements
              "qf1pE" "qf2pE" "qf3pE" "qf4pE" "qf5pE" "qf1pElump"))


;; Font Lock linking

(defconst freefem++-font-lock-keywords-1 (c-lang-const c-matchers-1 freefem++)
  "Minimal highlighting for FreeFem++ mode.")

(defconst freefem++-font-lock-keywords-2 (c-lang-const c-matchers-2 freefem++)
  "Fast normal highlighting for FreeFem++ mode.")

(defconst freefem++-font-lock-keywords-3 (c-lang-const c-matchers-3 freefem++)
  "Accurate normal highlighting for FreeFem++ mode.")

(defvar freefem++-font-lock-keywords freefem++-font-lock-keywords-3
  "Default expressions to highlight in FreeFem++ mode.")


;; Program invocation

(defcustom freefem++-program "FreeFem++"
  "Command used to execute the FreeFem++ compiler.
See also `freefem++-program-options'."
  :type 'string
  :group 'freefem++)

(defcustom freefem++-program-options (list "-ne")
  "Options applied to `freefem++-program'.

Options:
 -v <level>,      level of freefem output (0--1000000)
 -fglut <path>,   the file name of save all plots (replot with ffglut command)
 -glut <command>, change <command> compatible with ffglut
 -gff <command>,  change <command> compatible with ffglut (with space quoting)
 -nowait,         nowait at the end on window
 -wait,           wait at the end on window
 -nw,             no ffglut, ffmedit  (=> no graphics windows)
 -ne,             no edp script output
 -cd,             Change dir to script dir"
  :type '(repeat string)
  :group 'freefem++)

(defcustom freefem++-run-error-regexp-alist
  '(("^ Error line number \\([0-9]+\\), in file \\([[:alpha:]][-[:alnum:].]+\\)," 2 1))
  "Alist that specifies how to match errors in FreeFem++ output."
  :type 'alist
  :link '(variable-link compilation-error-regexp-alist)
  :group 'freefem++)

(defvar-local freefem++-process nil
  "Process currently executing `freefem++-program'")

(defun freefem++-kill-process ()
  "Send kill signal to FreeFem++ process."
  (interactive)
  (interrupt-process freefem++-process))

(define-compilation-mode freefem++-run-mode "Compilation"
  "Major mode for FreeFem++ compilation log buffers.")

(defun freefem++-run-buffer ()
  "Send current buffer to FreeFem++."
  (interactive)
  (save-some-buffers)
  (let ((file (file-name-nondirectory buffer-file-name))
        (command (mapconcat #'identity
                            (cons freefem++-program
                                  freefem++-program-options)
                            " ")))
    (setq freefem++-process (compilation-start (concat command " " file)
                                               'freefem++-run-mode))))


;; Easy menu

(c-lang-defconst c-mode-menu
  ;; The definition for the mode menu. The menu title is prepended to
  ;; this before it's fed to `easy-menu-define'.
  t `(["Process this buffer" freefem++-run-buffer t]
      ["Kill FreeFem++ process" freefem++-kill-process t]
      "---"
      ["Comment Out Region" comment-dwim
       (c-fn-region-is-active-p)]
      ["Uncomment Region" comment-dwim
       (c-fn-region-is-active-p)]
      ["Indent Expression" c-indent-exp
       (memq (char-after) '(?\( ?\[ ?\{))]
      ["Indent Line or Region" c-indent-line-or-region t]
      ["Fill Comment Paragraph" c-fill-paragraph t]
      "----"
      ["Backward Statement" c-beginning-of-statement t]
      ["Forward Statement" c-end-of-statement t]
      "----"
      ("Toggle..."
       ["Syntactic indentation" c-toggle-syntactic-indentation
        :style toggle :selected c-syntactic-indentation]
       ["Electric mode" c-toggle-electric-state
        :style toggle :selected c-electric-flag]
       ["Auto newline" c-toggle-auto-newline
        :style toggle :selected c-auto-newline]
       ["Hungry delete" c-toggle-hungry-state
        :style toggle :selected c-hungry-delete-key]
       ["Subword mode" subword-mode
        :style toggle :selected (and (boundp 'subword-mode)
                                     subword-mode)])))


;; Support for FreeFem++

(defcustom freefem++-code-style '("bsd" (c-basic-offset . 4))
  "Specification of the \"freefem++\" style."
  :type '(cons string (alist :key-type symbol :value-type sexp))
  :link '(info-link "(ccmode) Style Variables")
  :group 'freefem++)

(defvar freefem++-mode-syntax-table
  (let ((table (make-syntax-table)))
    (set-char-table-parent table c++-mode-syntax-table)
    (modify-syntax-entry ?' "_" table)
    table)
  "Syntax table used in `freefem++-mode' buffers.")

(defvar freefem++-mode-abbrev-table nil
  "Abbreviation table used in `freefem++-mode' buffers.")
(c-define-abbrev-table 'freefem++-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)))

(defvar freefem++-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map c-mode-base-map)
    (define-key map (kbd "C-c C-c") 'freefem++-run-buffer)
    (define-key map (kbd "C-c C-k") 'freefem++-kill-process)
    map)
  "Keymap used in `freefem++-mode' buffers.")

(easy-menu-define freefem++-menu freefem++-mode-map "FreeFem++ Mode Commands"
  (cons "FreeFem++" (c-lang-const c-mode-menu freefem++)))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.[ei]dp\\'" . freefem++-mode))

;;;###autoload
(define-derived-mode freefem++-mode c++-mode "FreeFem++"
  "Major mode for editing code written in the FreeFem++ programming language.
See http://www.freefem.org/ff++/ for more information about the
FreeFem++ language.

In addition to any hooks its parent mode might have run, this
mode runs the hook `freefem++-mode-hook' as the final step during
initialization.

Key bindings:
\\{freefem++-mode-map}"
  :group 'freefem++
  (c-initialize-cc-mode t)
  (c-init-language-vars freefem++-mode)
  (c-common-init 'freefem++-mode)
  (easy-menu-add freefem++-menu)
  (c-add-style "freefem++" freefem++-code-style)
  (unless (or c-file-style
              (stringp c-default-style)
              (assq 'freefem++-mode c-default-style))
    (c-set-style "freefem++" t))
  (cc-imenu-init cc-imenu-c++-generic-expression)
  (c-update-modeline))

(provide 'freefem++-mode)

;;; freefem++-mode.el ends here
