# FreeFem++-Mode

An [Emacs](http://www.gnu.org/software/emacs) mode for the
[FreeFem++](http://www.freefem.org/ff++/) language.

- It is based on *cc* mode, from which it inherits its features,
  including syntax highlighting, automatic indentation (*TAB* key) and
  others. Current version (0.3) provides syntax highligting for more
  FreeFem++ reserved words and additional menu entries.
- It defines the "Run buffer" function (key *C-c C-c*) as follows:
  1. Send current buffer (current FreeFem++ program) to the FreeFem++ interpreter.
  2. Then window is split into two frames and FreeFem++ output is displayed in the bottom one.
  3. If FreeFem++ returned with an error:
    - In the output buffer, the error line is highlighted
    - In the code buffer, the cursor goes to the error line and error
       is highlighted

Install:
--------

Copy the file *freefem++-mode.el* into any directory located in your Emacs
*load-path*. Then, add the appropriate lines into your *.emacs*
configuration file, for instance:

```lisp
	(autoload 'freefem++-mode "freefem++-mode"
		"Major mode for editing FreeFem++ code." t)
	(add-to-list 'auto-mode-alist '("\\.edp$" . freefem++-mode))
	(add-to-list 'auto-mode-alist '("\\.idp$" . freefem++-mode))
```

Optionally, you can also specify the desired FreeFem++ interpreter,
for instance

```lisp
	(setq freefempp-program "FreeFem++-nw")
```
or
```lisp
	(setq freefempp-program "ff-mpirun -np 2")
```

<!--
Local Variables: 
mode: Markdown
ispell-local-dictionary: "english"
End:
-->
