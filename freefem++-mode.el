;;; freefem++-mode.el --- FreeFem++ Programming Language mode for (X)Emacs
;;;               Requires a cc-mode of version 5.30 or greater

;; Author:     2008-2011 J. Rafael Rodríguez Galván
;; Maintainer: J. Rafael Rodríguez Galván
;; Created:    September 2008
;; Version:    0.3
;; Based on:   D programming language mode for (X)Emacs, by William Baxter
;; Keywords:   FreeFem++ finite element programming language emacs cc-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Usage:
;; 1. Download this file to a directory located in emacs site-lisp
;; 2. Put these lines in your .emacs startup file:
;;  (autoload 'freefem++-mode "freefem++-mode" "Major mode for editing FreeFem++ code." t)
;;  (add-to-list 'auto-mode-alist '("\\.edp$" . freefem++-mode))
;;  (add-to-list 'auto-mode-alist '("\\.idp$" . freefem++-mode))
;;
;; cc-mode version 5.30 or greater is required.
;; You can check your cc-mode with the command M-x c-version.
;; You can get the latest version of cc-mode at http://cc-mode.sourceforge.net
;;
;;
;; TODO:
;;   * Assure all FreeFem++ keywords are highlighted.
;;   * ¿Use client/server scheme for communicating emacs/freefem++?
;;   * Build a help on freefem++-mode
;;
;; History:
;;   * 2011 June 29 - Released version 0.3
;;      Changelog:
;;         - Mode is renamed from ff++-mode to freefem++-mode
;;         - Uploaded to github (https://github.com/cucharro/emacs/blob/master/freefem++-mode.el)
;;   * 2011 February 10 - Released version 0.29
;;      Changelog:
;;         - FreeFem++ is executed as an asynchronous process
;;   * 2008 September 23 - Released version 0.2
;;      Changelog:
;;        - Set up of comment/uncomment region menu entries.
;;        - Added syntax highligting for more keywords.
;;        - Run buffer function was improved: 
;;            + After running, window is split into two frames and FreeFem++ 
;;              output is displayed in the bottom one.
;;            + If FreeFem++ returned with an error: the error line is 
;;              highlighted in output buffer; in code buffer, cursor goes to 
;;              error line and error keyword is highlighted
;;   * 2008 September 9 - Released version 0.1
;;      Changelog:
;;        - First version: Syntax higlighting of several keywords, 
;;          FreeFem++ menú, possibility of running FreFem++ code buffer
;;----------------------------------------------------------------------------
;; Code:

(require 'cc-mode)
(require 'hi-lock)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
;; Coment out 'when-compile part for debugging
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts)
  )


;; ;; Declare freefem++-mode as a new language derived from c++
(c-add-language 'freefem++-mode 'c++-mode)
(eval-and-compile
  ;; Make our mode known to the language constant system.  Use C++
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'freefem++-mode 'c++-mode))

;;----------------------------------------------------------------------------

;; Built-in basic types
(c-lang-defconst c-primitive-type-kwds
  freefem++ '("bool" "border" "Cmatrix" "complex" "element" "fespace"
	 "func" "ifstream" "int" "macro" "matrix" "mesh" "ofstream" "problem"
	 "real" "R3" "solve" "string" "varf" "vertex" ))

;; Keywords that can prefix normal declarations of identifiers
(c-lang-defconst c-modifier-kwds 
  freefem++  nil)

(c-lang-defconst c-class-decl-kwds
  ;; Keywords introducing declarations where the following block (if any)
  ;; contains another declaration level that should be considered a class.
  freefem++  nil)

(c-lang-defconst c-brace-list-decl-kwds
  freefem++  nil)

(c-lang-defconst c-type-modifier-kwds
  freefem++  nil)

(c-lang-defconst c-type-prefix-kwds
  ;; Keywords where the following name - if any - is a type name, and
  ;; where the keyword together with the symbol works as a type in
  ;; declarations.  In this case, like "mixin foo!(x) bar;"
  freefem++  nil)

(c-lang-defconst c-typedef-decl-kwds
  freefem++  nil)

(c-lang-defconst c-decl-hangon-kwds
  freefem++  nil)

(c-lang-defconst c-protection-kwds
  ;; Access protection label keywords in classes.
  freefem++  nil)

(c-lang-defconst c-type-list-kwds
  freefem++  nil)

(c-lang-defconst c-ref-list-kwds
  freefem++  nil)

(c-lang-defconst c-colon-type-list-kwds
  ;; Keywords that may be followed (not necessarily directly) by a colon
  ;; and then a comma separated list of type identifiers.
  freefem++  nil)

(c-lang-defconst c-paren-nontype-kwds
  ;;Keywords that may be followed by a parenthesis expression that doesn't
  ;; contain type identifiers.
  freefem++  nil)

(c-lang-defconst c-paren-type-kwds
  ;; Keywords that may be followed by a parenthesis expression containing
  ;; type identifiers separated by arbitrary tokens.
  freefem++  nil)

(c-lang-defconst c-block-stmt-1-kwds
  ;; Statement keywords followed directly by a substatement.
  ;; 'static' is there for the "else static if (...) {}" usage.
  freefem++ '("else" "try"))

(c-lang-defconst c-block-stmt-2-kwds
  ;; Statement keywords followed by a paren sexp and then by a substatement.
  freefem++ '("catch" "for" "if" "while"))

(c-lang-defconst c-simple-stmt-kwds
  ;; Statement keywords followed by an expression or nothing.
  freefem++ '("break" "continue" "return" "throw"))

(c-lang-defconst c-paren-stmt-kwds
  ;; Statement keywords followed by a parenthesis expression that
  ;; nevertheless contains a list separated with ';' and not ','."
  freefem++ '("adaptmesh" "assert" "buildmesh"  "clock" "convect" 
         "dx" "dy" "dz" "intalledges" "int1d" "int2d" "int3d" 
	 "jump" "mean" "movemesh" "on" "plot" "savemesh" "square"
         "sin" "cos" "tan" "atan" "asin" "acos"
         "cotan" "sinh" "cosh" "tanh""cotanh"
         "exp" "log" "log10" "sqrt" "abs" "max" "min"
         "load" "include"))

(c-lang-defconst c-asm-stmt-kwds
  ;; Statement keywords followed by an assembler expression.
  freefem++ nil)

(c-lang-defconst c-label-kwds
  ;; Keywords introducing colon terminated labels in blocks.
  freefem++ nil)

(c-lang-defconst c-before-label-kwds
  ;; Keywords that might be followed by a label identifier.
  freefem++ nil)

(c-lang-defconst c-constant-kwds
  ;; Keywords for constants.
  freefem++ '("false" "i" "pi" "true"
         "anisomax" "area" "CG" "cin" "cout"  "endl" "eps"
         "hTriangle""init" "label" 
         "lenEdge" "N"  "nTonEdge" "nuTriangle" "nuEdge" 
         "precon""region" "split" "solver" "strategy" 
         "tgv" "tolpivot" "tolpivotsym"
         "x" "y" "z"
					; Finite elements
         "P" "P0" "P1" "P2" "RT0" "P1nc" "P1dc" "P2dc" "P1b"
					; Solvers
         "LU" "Cholesky" "Crout" "CG" "GMRES" "UMFPACK" "sparsesolver"
					; Quadrature keywords
         "qfe" "qforder" "qf1pE" "qf2pE" "qf3pE" "qf4pE" "qf5pE" "qf1pElump"
         "qf1pT" "qf2pT" "qf5pT" "qf1pTlump" "qf2pT4P1" "qf7pT" "qf9pT"
					; Plot options
         "fill" "wait"
					; Adaptmesh
         "hmax" "hmin" "err" "errg" "nbvx" "nbsmooth" "nbjacobi"
         "ratio" "omega" "iso" "abserror" "cutoff" "verbosity" 
         "inquire" "splitpbedge" "maxsubdiv" "rescaling" "keepbackvertices"
         "isMetric" "power" "thetamax" "splitin2" "metric"
         "nomeshgeneration" "periodic"
	 ))

(c-lang-defconst c-primary-expr-kwds
  ;; Keywords besides constants and operators that start primary expressions.
  freefem++ nil)

(c-lang-defconst c-inexpr-class-kwds
  ;; Keywords that can start classes inside expressions.
  freefem++    nil)

(c-lang-defconst c-inexpr-brace-list-kwds
  ;; Keywords that can start brace list blocks inside expressions.
  freefem++    nil)

(c-lang-defconst c-other-decl-kwds
  P0 P1
  freefem++ nil)

(c-lang-defconst c-other-kwds
  ;; Keywords not accounted for by any other `*-kwds' language constant.
  freefem++ nil )

;; (defcustom freefem++-font-lock-extra-types nil
;;   "*List of extra types (aside from the type keywords) to recognize in FreeFem++ mode. 
;;    Each list item should be a regexp matching a single identifier."
;;   )

(defconst freefem++-font-lock-keywords-1 (c-lang-const c-matchers-1 freefem++)
  "Minimal highlighting for FreeFem++ mode.")

(defconst freefem++-font-lock-keywords-2 (c-lang-const c-matchers-2 freefem++)
  "Fast normal highlighting for FreeFem++ mode.")

(defconst freefem++-font-lock-keywords-3 (c-lang-const c-matchers-3 freefem++)
  "Accurate normal highlighting for FreeFem++ mode.")

(defvar freefem++-font-lock-keywords freefem++-font-lock-keywords-3
  "Default expressions to highlight in FreeFem++ mode.")

(defvar freefem++-mode-syntax-table nil
  "Syntax table used in freefem++-mode buffers.")
(or freefem++-mode-syntax-table
    (setq freefem++-mode-syntax-table
	  (let ((table (funcall (c-lang-const c-make-mode-syntax-table freefem++))))
	    ;; Make it recognize D `backquote strings`
	    (modify-syntax-entry ?' "_" table)
	    ;; Make it recognize D's nested /+ +/ comments 
	    ;; You'll definitely need an elisp manual for this:
	    ;; http://www.delorie.com/gnu/docs/elisp-manual-21/
					;(modify-syntax-entry ?+  ". 23n"   table)
	    table)))

(defvar freefem++-mode-abbrev-table nil
  "Abbreviation table used in freefem++-mode buffers.")
(c-define-abbrev-table 'freefem++-mode-abbrev-table
  ;; Use the abbrevs table to trigger indentation actions 
  ;; on keywords that, if they occur first on a line, might alter the
  ;; syntactic context.
  ;; Syntax for abbrevs is:
  ;; ( pattern replacement command initial-count)
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))


;(defvar freefempp-program "xterm -e FreeFem++" 
(defvar freefempp-program "FreeFem++-x11"
      "* Command used to execute FreeFem++. Possible values: FreeFem++, FreeFem++-x11, FreeFem++-mpi, etc")
(defvar freefempp-output-buffer "*FreeFem++ Output*")
(defvar freefempp-process "Process currently executing freefem")
(defvar freefempp-process-code-buffer nil "Buffer freefem output is being displayied in")

(defun show-code-and-output-buffers () 
  (switch-to-buffer freefempp-process-code-buffer)
  (delete-other-windows)
  (switch-to-buffer-other-window freefempp-output-buffer)
  (switch-to-buffer-other-window freefempp-process-code-buffer))

(defun freefempp-process-sentinel (process event)
  (let ((freefempp-return-value (process-exit-status process))
	(freefempp-error-info (list 0 nil)))
    (cond ((/= freefempp-return-value 0)
	   ;; Save information about error (error-line, invalid-token)
	   (setq freefempp-error-info (freefempp-get-error-info))
	   ;; Highlight error line in output buffer
	   (freefempp-output-buffer-show-error freefempp-error-info))
	  (t (freefempp-output-buffer-show-no-error)))
    ;; Switch to code buffer. If error, go to error line and highlight wrong token
    (show-code-and-output-buffers)
    (freefempp-code-buffer-show-error freefempp-error-info)
    ;; Show exit info
    (cond ((string-equal (process-status freefempp-process) "signal")
	   (message (format "%s %s"
			    (process-name freefempp-process) 
			    "interrupted")))
	  (t (message (format "%s %s "
			      (process-name freefempp-process)
			      (process-status freefempp-process)))))))


(defun freefempp-interrupt-process()
  "Send *interrupt* signal to FreeFem++ process"
  (interactive)
  (interrupt-process freefempp-process))

(defun freefempp-kill-process()
  "Send *kill* signal to FreeFem++ process"
  (interactive)
  (kill-process freefempp-process))

(defun freefempp-stop-process()
  "Send *stop* signal to FreeFem++ process. Use freefempp-continue-process to resume execution"
  (interactive)
  (stop-process freefempp-process))

(defun freefempp-continue-process()
  "Send *continue* signal to FreeFem++ process"
  (interactive)
  (continue-process freefempp-process))

(defun freefempp-run-buffer()
  "Send current buffer to FreeFem++."
  (interactive)
  (save-some-buffers)
  (freefempp-remove-hi-lock)
  (let 
      ((freefempp-code-buffer (file-name-nondirectory buffer-file-name))
       (freefempp-error-info (list 0 nil)))
    ;; Clean freefempp-output-buffer, split window and change to it
    (if (get-buffer freefempp-output-buffer)
	(kill-buffer freefempp-output-buffer))
    (switch-to-buffer-other-window freefempp-output-buffer)
    ;; Send code to FreeFem++, saving the process
    (setq freefempp-process 
	  (start-file-process-shell-command "FreeFem++"
					    (get-buffer-create freefempp-output-buffer)
	  				    freefempp-program
	  				    freefempp-code-buffer))
    ;; Attach a sentinel to the running process
    (setq freefempp-process-code-buffer freefempp-code-buffer)
					;(file-name-nondirectory buffer-file-name))
    (set-process-sentinel freefempp-process 'freefempp-process-sentinel)))


(defun freefempp-remove-hi-lock ()
  "Redraw highlighted expressions, using default face"
  (while hi-lock-interactive-patterns
    (hi-lock-unface-buffer (car(car hi-lock-interactive-patterns)))))

(defun freefempp-get-error-info () 
  "Look at FreeFem++ output and return a list (err-line, err-string),
  where err-line is the error line (or 0 if no information about error
  line was detected) and err-string is the information presented by
  FreeFem++ (usually, a wrong token)."
  (let ((line-number "0") 
	(error-regexp "\s*Error line number \\([0-9]*\\),") 
	(wrong-token nil))
    (save-excursion 
       (cond ((search-backward-regexp error-regexp nil t)
              (setq line-number (match-string 1)
                    wrong-token (match-string 2)))))
    (list (string-to-number line-number) wrong-token))
)

(defun freefempp-output-buffer-show-error(freefempp-error-info)
  "If an error was detected, set point in error line in FreeFem++ output"
  (let ((freefempp-error-line (car freefempp-error-info)))
    (cond ((> freefempp-error-line 0)
     (let ((freefempp-error-regexp 
       (concatenate 'string "^[ \t]*" (number-to-string freefempp-error-line) " :")))
     (search-backward-regexp freefempp-error-regexp nil t)
     (hi-lock-line-face-buffer freefempp-error-regexp))))))
   
(defun freefempp-code-buffer-show-error(freefempp-error-info)
  "If an error was detected, set point in error line an highligt the wrong token"
  (let ((freefempp-error-line (car freefempp-error-info))
        (freefempp-invalid-token (car (cdr freefempp-error-info))))
    (cond ((> freefempp-error-line 0)
      (goto-line freefempp-error-line) 
      (highlight-regexp freefempp-invalid-token)))))

(defface freefempp-no-error-face
  '((((type tty) (class mono)) :inverse-video t)
    (t :foreground "white" :background "DarkGreen"))
  "Face for highlighting the FreeFem++ no error message."
  :group 'basic-faces)

(defun freefempp-output-buffer-show-no-error ()
  "To do if no error was detected"
  (let ((freefempp-no-error-regexp "^Bien: On a fini Normalement\\|^Ok: Normal End"))
  (hi-lock-line-face-buffer freefempp-no-error-regexp 'freefempp-no-error-face)))

(defvar ffpp-mode-map () "Keymap used in freefem++-mode buffers.")
(if ffpp-mode-map nil
  (setq ffpp-mode-map (c-make-inherited-keymap))
  ;; Add bindings which are only useful for FreeFem++
  (define-key ffpp-mode-map "\C-c\C-c"  'freefempp-run-buffer)
  (define-key ffpp-mode-map "\C-c\C-i"  'freefempp-interrupt-process)
  (define-key ffpp-mode-map "\C-c\C-k"  'freefempp-kill-process)
  )

(c-lang-defconst c-mode-menu
  ;; The definition for the mode menu.  The menu title is prepended to
  ;; this before it's fed to `easy-menu-define'.
  t `(["Process this buffer"     freefempp-run-buffer t]
      ["Interrupt FreeFem++ process" freefempp-stop-process t]
      ["Kill FreeFem++ process" freefempp-kill-process t]
      "---"
      ["Comment Out Region"     comment-dwim
       (c-fn-region-is-active-p)]
      ["Uncomment Region"       comment-dwim
       (c-fn-region-is-active-p)]
      ["Indent Expression"      c-indent-exp
       (memq (char-after) '(?\( ?\[ ?\{))]
      ["Indent Line or Region"  c-indent-line-or-region t]
      ["Fill Comment Paragraph" c-fill-paragraph t]
      "----"
      ["Backward Statement"     c-beginning-of-statement t]
      ["Forward Statement"      c-end-of-statement t]
      "----"
      ("Toggle..."
       ["Syntactic indentation" c-toggle-syntactic-indentation
	:style toggle :selected c-syntactic-indentation]
       ["Electric mode"         c-toggle-electric-state
	:style toggle :selected c-electric-flag]
       ["Auto newline"          c-toggle-auto-newline
	:style toggle :selected c-auto-newline]
       ["Hungry delete"         c-toggle-hungry-state
	:style toggle :selected c-hungry-delete-key]
       ["Subword mode"          c-subword-mode
	:style toggle :selected (and (boundp 'c-subword-mode)
				     c-subword-mode)])))

(easy-menu-define ffpp-menu ffpp-mode-map "FreeFem++ Mode Commands"
  (cons "FreeFem++" (c-lang-const c-mode-menu freefem++)))
;; To activate the menu in X-Emacs. See http://www.emacswiki.org/emacs/EasyMenu
(easy-menu-add ffpp-menu ffpp-mode-map)

;;----------------------------------------------------------------------------

;;;###autoload
(defun freefem++-mode ()
  "Major mode for editing code written in the FreeFem++ Programming Language.
See http://www.freefem.org/freefem++/ for more information about the FreeFem++ language.
The hook `c-mode-common-hook' is run with no args at mode
initialization, then `freefem++-mode-hook'.

Key bindings:
\\{ffpp-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table freefem++-mode-syntax-table)
  (setq major-mode 'freefem++-mode
	mode-name "FreeFem++"
	local-abbrev-table freefem++-mode-abbrev-table
	abbrev-mode t)

  (make-local-variable 'comment-start)
  (setq comment-end   "")
  (make-local-variable 'comment-end)
  (setq comment-start "// ")
  (use-local-map ffpp-mode-map)
  (c-init-language-vars freefem++-mode)
  (c-common-init 'freefem++-mode)
  (easy-menu-add ffpp-menu)
  (c-run-mode-hooks 'c-mode-common-hook 'freefem++-mode-hook)
  (c-update-modeline))

(provide 'freefem++-mode)

;;; freefem++-mode.el ends here
