;;----------------------------------------------------------------------------
;; PHPTOOLS - Bigloo tools for manipulating PHP source.
;; Copyright (C) 2002 Cronosys, LLC
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA 
;;----------------------------------------------------------------------------

(module php-rg
	(export make-php-rg))

(define php-keywords
  '(("__file__" . kw-__file__)
    ("__line__" . kw-__line__)
    ("and" . kw-and)
    ("array" . kw-array)
    ("as" . kw-as)
    ("break" . kw-break)
    ("case" . kw-case)
    ("cfunction" . kw-function)
    ("class" . kw-class)
    ("const" . kw-const)
    ("continue" . kw-continue)
    ("declare" . kw-declare)
    ("default" . kw-default)
    ("die" . kw-exit)
    ("do" . kw-do)
    ("echo" . kw-echo)
    ("else" . kw-else)
    ("elseif" . kw-elseif)
    ("empty" . kw-empty)
    ("enddeclare" . kw-enddeclare)
    ("endfor" . kw-endfor)
    ("endforeach" . kw-endforeach)
    ("endif" . kw-endif)
    ("endswitch" . kw-endswitch)
    ("endwhile" . kw-endwhile)
    ("eval" . kw-eval)
    ("exit" . kw-exit)
    ("extends" . kw-extends)
    ("for" . kw-for)
    ("foreach" . kw-foreach)
    ("function" . kw-function)
    ("global" . kw-global)
    ("if" . kw-if)
    ("include" . kw-include)
    ("include_once" . kw-include_once)
    ("isset" . kw-isset)
    ("list" . kw-list)
    ("new" . kw-new)
    ("old_function" . kw-old_function)
    ("or" . kw-or)
    ("print" . kw-print)
    ("require" . kw-require)
    ("require_once" . kw-require_once)
    ("return" . kw-return)
    ("static" . kw-static)
    ("switch" . kw-switch)
    ("unset" . kw-unset)
    ("var" . kw-var)
    ("while" . kw-while)
    ("xor" . kw-xor)
    ("::" . op-paamayim-nekudotayim)
    ("=>" . op-=>)
    ("++" . op-++)
    ("--" . op---)
    ("===" . op-===)
    ("!==" . op-!==)
    ("==" . op-==)
    ("!=" . op-!=)
    ("<>" . op-!=)
    ("<=" . op-<=)
    (">=" . op->=)
    ("+=" . op-+=)
    ("-=" . op--=)
    ("*=" . op-*=)
    ("/=" . op-/=)
    (".=" . op-.=)
    ("%=" . op-%=)
    (">>=" . op->>=)
    ("<<=" . op-<<=)
    ("&=" . op-&=)
    ("|=" . op-bit-or=)
    ("^=" . op-^=)
    ("||" . op-boolean-or)
    ("&&" . op-&&)
    ("<<" . op-<<)
    (">>" . op->>)))

(define php-1char-operators
  '((#\[ . op-lsqbrace)
    (#\] . op-rsqbrace)
    (#\{ . op-lcbrace)
    (#\} . op-rcbrace)
    (#\$ . op-$)
    (#\| . op-bit-or)
    (#\^ . op-^)
    (#\& . op-&)
    (#\+ . op-+)
    (#\- . op--)
    (#\/ . op-/)
    (#\* . op-*)
    (#\= . op-=)
    (#\% . op-%)
    (#\! . op-!)
    (#\~ . op-~)
    (#\< . op-<)
    (#\> . op->)
    (#\? . op-?)
    (#\; . op-semicolon)
    (#\: . op-colon)
    (#\, . op-comma)
    (#\. . op-.)
    (#\( . op-lparen)
    (#\) . op-rparen)
    (#\@ . op-suppress)))

;; These macros provide a context stack for a regular-grammar.
(define-macro (sync-context)
  `(if (or (null? context-stack)
	   (eq? (car context-stack) #f))
     (rgc-context)
     (rgc-context (car context-stack))))
(define-macro (push-context c)
  `(begin
     (set! context-stack (cons ,c context-stack))
     (sync-context)))
(define-macro (pop-context)
  `(begin
     (if (not (null? context-stack))
       (set! context-stack (cdr context-stack)))
     (sync-context)))
(define-macro (set-context . args)
  `(begin
     (if (null? context-stack)
       (push-context ,(if (null? args) #f (car args)))
       (set-car! context-stack ,(if (null? args) #f (car args))))
     (sync-context)))
(define-macro (update-line)
  '(set! line (+ line (line-count (the-string)))))

;; Count the number of \n's in a string.
(define (line-count str)
  (let iloop ((i 0)
	      (v 0))
    (cond
      ((>= i (string-length str))
       v)
      ((char=? #\newline (string-ref str i))
       (iloop (+ i 1) (+ v 1)))
      (else
       (iloop (+ i 1) v)))))

;; Function to parse double-quoted, escaped string data:
(define (unescape-double-quotes str)
  (let ((ret '()))
    (string-case str 
      ((: "\\" (in "$\\\""))
	(set! ret (cons (the-substring 1 2) ret))
	(ignore))
      ("\\n"
	(set! ret (cons "\n" ret))
	(ignore))
      ("\\r"
	(set! ret (cons "\r" ret))
	(ignore))
      ("\\t"
	(set! ret (cons "\t" ret))
	(ignore))

      ;; Hex constants:
      ((: (uncase "\\x")
	  (submatch (** 1 2 (in "09afAF"))))
       (let ((s (the-submatch 1)))
	 (let xloop ((i 0)
		     (v 0))
	   (if (>= i (string-length s))
	     (set! ret (cons (make-string 1 (integer->char v)) ret))
	     (xloop (+ i 1)
		    (+ (* v 16)
		       (let ((c (string-ref s i)))
			 (if (char-numeric? c)
			   (- (char->integer c)
			      (char->integer #\0))
			   (+ (- (char->integer (char-downcase c))
				 (char->integer #\a))
			      10)))))))
	 (ignore)))

      ;; Octal constants:
      ((: "\\" (submatch (** 1 3 (in "07"))))
       (let ((s (the-submatch 1)))
	 (let oloop ((i 0)
		     (v 0))
	   (if (>= i (string-length s))
	     (set! ret (cons (make-string 1 (integer->char v)) ret))
	     (oloop (+ i 1)
		    (+ (* v 8)
		       (let ((c (string-ref s i)))
			 (- (char->integer c)
			    (char->integer #\0))))))))
       (ignore))

      ;; Everything else:
      (all
	(set! ret (cons (the-string) ret))
	(ignore))
      (else
	(apply string-append (reverse ret))))))

(define (unescape-single-quote str)
  (let ((ret '()))
    (string-case str
      ((: "\\" (in #\\ #\'))
	(set! ret (cons (the-substring 1 2) ret))
	(ignore))
      ((or all #\Newline)
	(set! ret (cons (the-string) ret))
	(ignore))
      (else
	(apply string-append (reverse ret))))))

(define (make-php-rg . args)
  (let ((asp-tags? #f)
	(short-tags? #t)
	(ignore-whitespace? #t)
	(ignore-comments? #t)
	(context-stack '())
	(value-buffer '())
	(file #f)
	(line 1)
	(heredoc-label #f)
	(token-queue '()))

    ;; Parse arguments.
    (let loop ((args args))
      (if (not (null? args))
	(case (car args)
	  ((asp-tags:)
	   (set! asp-tags? (cadr args))
	   (loop (cddr args)))

	  ((short-tags:)
	   (set! short-tags? (cadr args))
	   (loop (cddr args)))

	  ((file:)
	   (set! file (cadr args)))
	  ((line:)
	   (set! line (cadr args)))

	  ((ignore-whitespace:)
	   (set! ignore-whitespace? (cadr args))
	   (loop (cddr args)))

	  ((ignore-comments:)
	   (set! ignore-comments? (cadr args))
	   (loop (cddr args))))))

	(regular-grammar
	  ;; Bindings
	  ((whitespace
	     (+ (in " \n\r\t")))
	   (label
	     (posix "[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*"))
	   (tokens
	     (in ";:,.[]()|^&+-/*=%!~$<>?@"))
	   (escaped-and-whitespace
	     (+ (in "\n\t\r #'.:;,()|^&+-/*=%!~<>?@")))
	   (encapsed-tokens
	     (in "[]{}$"))
	   (lnum (posix "[0-9]+"))
	   (hnum (posix "0[xX][0-9a-fA-F]+"))
	   (dnum (posix "([0-9]*[\\.][0-9]+)|([0-9]+[\\.][0-9]*)"))
	   (exponent-dnum 
	     (: (or lnum dnum) (in "eE") (? (in "+-")) lnum))
	   (tabs-and-spaces
	     (* (in " \t")))
	   )

	  ((context need-semicolon (or all #\Newline))
	   (pop-context)
	   (rgc-buffer-unget-char input-port (char->integer (the-character)))
	   'op-semicolon)

	  ;; Variables:
	  ((when (memq (the-context)
		       '(in-scripting double-quotes heredoc backquote))
		 (: "$" label))
	   (cons 'v-variable (the-substring 1 (the-length))))

	  ;; *** STRING LITERALS ***
	  ((context in-scripting (: #\"
				    (* (or (out #\$ #\" #\\) (: #\\ all)))
				    #\"))
	   (update-line)
	   (let ((s (the-string)))
	     (cons 'v-constant-encapsed-string
		   (unescape-double-quotes 
		     (substring s 1 (- (string-length s) 1))))))
	  ((context in-scripting #\")
	   (set-context 'double-quotes)
	   'op-q-double)
	  ((context in-scripting (: #\'
				    (* (or (out #\' #\\)
					   (: #\\ (in all #\Newline)))
					   )
				    #\'))
	   (update-line)
	   (let ((s (the-string)))
	     (cons 'v-constant-encapsed-string
		   (unescape-single-quote
		     (substring s 1 (- (string-length s) 1))))))
	  ((context in-scripting #\')
	   (cons 'op-q-single (the-string)))
	  ((context in-scripting #\`)
	   (cons 'op-q-back (the-string)))
	  ((when (memq (the-context) '(double-quotes backquote heredoc))
		 escaped-and-whitespace)
	   (update-line)
	   (cons 'v-encapsed-and-whitespace (the-string)))
	  ((context double-quotes (+ #\`))
	   (cons 'v-encapsed-and-whitespace (the-string)))
	  ((context backqouote (+ #\"))
	   (cons 'v-encapsed-and-whitespace (the-string)))
	  ((when (memq (the-context) '(double-quotes backquote heredoc))
		 encapsed-tokens)
	   (update-line)
	   (cdr (assq (the-character) php-1char-operators)))
	  ((context single-quote (+ (or (out "'\\") (: "\\" (out "'\\")))))
	   (update-line)
	   (cons 'v-encapsed-and-whitespace (the-string)))
	  ((context single-quote "\\'")
	   (cons 'v-character #\'))
	  ((context single-quote "\\\\")
	   (cons 'v-character #\\))
	  ((context double-quotes "\\\"")
	   (cons 'v-character "\""))
	  ((context backquote "\\`")
	   (cons 'v-character #\`))
	  ((when (memq (the-context) '(double-quotes backquote heredoc))
		 (posix "\\[0-7]{1,3}"))
	   (cons 'v-character
		 (let oloop ((i 1)
			     (v 0))
		   (if (>= i (the-length))
		     (integer->char v)
		     (oloop (+ i 1) (+ (* v 8)
				       (- (char->integer
					    (string-ref (the-string) i))
					  (char->integer #\0))))))))

	  ((when (memq (the-context) '(double-quotes backquote heredoc))
		 (posix "\\x[0-9a-fA-F]{1,2}"))
	   (cons 'v-character
		 (let xloop ((i 1)
			     (v 0))
		   (if (>= i (the-length))
		     (list->string (list (integer->char v)))
		     (xloop (+ i 1) (+ (* v 16)
				       (let ((c (string-ref (the-string) i)))
					 (if (char-numeric? c)
					   (- (char->integer c)
					      (char->integer #\0))
					   (+ (- (char->integer
						   (char-downcase c))
						 (char->integer #\a))
					      10)))))))))

	  ((when (memq (the-context) '(double-quotes backquote heredoc))
		 (: "\\" all))
	   (update-line)
	   (let ((c (string-ref (the-string) 1)))
	     (case c
	       ((#\n) (cons 'v-character #\newline))
	       ((#\t) (cons 'v-character (integer->char 9)))
	       ((#\r) (cons 'v-character (integer->char 13)))
	       ((#\\) (cons 'v-character #\\))
	       ((#\$) (cons 'v-character #\$))
	       ((#\{) (cons 'v-character #\{))
	       (else
		 (cons 'v-encapsed-and-whitespace
		       (list->string (list #\\ c)))))))

	  ((context heredoc (+ (in "\"'`")))
	   (cons 'v-encapsed-and-whitespace (the-string)))
	  ((when (memq (the-context) '(double-quotes backquote heredoc)) "{$")
	   (rgc-buffer-unget-char input-port (char->integer #\$))
	   (push-context 'in-scripting)
	   'op-lcbrace-$)
	  ((context double-quotes #\")
	   (set-context 'in-scripting)
	   'op-q-double)
	  ((context single-quote #\')
	   (set-context 'in-scripting)
	   'op-q-single)
	  ((context backquote #\`)
	   (set-context 'in-scripting)
	   'op-q-back)
	  ((when (memq (the-context) '(double-quotes backquote heredoc))
		 (or lnum hnum))
	   (cons 'v-num-string (the-string)))
	  ((when (memq (the-context) '(double-quotes backquote heredoc))
		 (: #\$ (posix "[^a-zA-Z_\x7f-\xff{]")))
	   (rgc-buffer-unget-char input-port 
				  (char->integer (string-ref (the-string) 1)))
	   (cons 'v-character #\$))
	  ;; *** END STRING LITERALS ***

	  ;; Numbers:  In octal, decimal, and hex.
	  ((context in-scripting lnum)
	   (if (char=? (the-character) #\0)
	     (let oloop ((i 1)
			 (v 0))
	       (if (>= i (the-length))
		 (cons 'v-lnumber v)
		 (oloop (+ i 1)
			(+ (* v 8)
			   (- (char->integer (string-ref (the-string) i))
			      (char->integer #\0))))))
	     (cons 'v-lnumber (the-fixnum))))
	  ((context in-scripting hnum)
	   (let ((s (the-string)))
	     (let xloop ((i 2)
			 (v 0))
	       (if (>= i (string-length s))
		 (cons 'v-lnumber v)
		 (xloop (+ i 1)
			(+ (* v 16)
			   (if (char-numeric? (string-ref s i))
			     (- (char->integer (string-ref s i))
				(char->integer #\0))
			     (+ (- (char->integer (char-downcase
						    (string-ref s i)))
				   (char->integer #\a))
				10))))))))
	  ((context in-scripting (or dnum exponent-dnum))
	   (cons 'v-dnumber (the-flonum)))

	  ;; Heredocs:
	  ((context in-scripting (: "<<<"
				    tabs-and-spaces
				    (submatch label)
				    (? "\r")
				    "\n"))
	   (update-line)
	   (push-context 'heredoc)
	   (set! heredoc-label (the-submatch 1))
	   'op-start-heredoc)
	  ((context heredoc (bol (: (submatch label) (? ";") (? "\r") "\n")))
	   (update-line)
	   (if (string=? (the-submatch 1) heredoc-label)
	     (begin
	       (pop-context)
	       (if (pregexp-match ";" (the-string))
	         (push-context 'need-semicolon))
	       'op-end-heredoc)
	     (cons 'v-string (the-string))))

	  ;; Labels:  In scripting context, we check if the label is a keyword.
	  ((context in-scripting label)
	   (let ((lower (string-copy (the-string))))
	     (let lloop ((i 0))
	       (if (not (>= i (the-length)))
		 (begin
		   (string-set! lower i (char-downcase (string-ref lower i)))
		   (lloop (+ i 1)))))
	     (if (assoc lower php-keywords)
	       (let ((sym (cdr (assoc lower php-keywords))))
		 (case sym
		   ((kw-__file__)
		    (cons 'v-string (if file
				    file
				    (input-port-name (the-port)))))
		   ((kw-__line__)
		    (cons 'v-lnumber line))
		   (else
		    sym)))
	       (cons 'v-string (the-string)))))
	  ((when (memq (the-context) '(double-quotes backquote heredoc)) label)
	   (cons 'v-string (the-string)))

	  ;; Comments: C++ and Shell-style
	  ((context in-scripting (: (or "//" "#")
				    (* (or (out "\n\r?") (: "?" (out ">\n\r"))))
				    (? (or "?\n" "?\r\n"))))
	   (update-line)
	   (if ignore-comments?
	     (ignore)
	     (cons 'v-comment (the-string))))

	  ;; Comments: C-style
	  ((context in-scripting "/*")
	   (set! value-buffer (list (the-string)))
	   (push-context 'c-comment)
	   (ignore))
	  ((context c-comment (+ (out #\*)))
	   (set! value-buffer (cons (the-string) value-buffer))
	   (update-line)
	   (ignore))
	  ((context c-comment "*/")
	   (set! value-buffer (cons (the-string) value-buffer))
	   (pop-context)
	   (let ((the-comment (apply string-append (reverse value-buffer))))
	     (set! value-buffer '())
	     (if ignore-comments?
	       (ignore)
	       (cons 'v-comment the-comment))))
	  ((context c-comment #\*)
	   (set! value-buffer (cons (the-string) value-buffer))
	   (ignore))

	  ;; Object Operator:
	  ((when (memq (the-context) 
		       '(in-scripting double-quotes backquote heredoc))
		 "->")
	   (push-context 'looking-for-property)
	   'op-->)
	  ((context looking-for-property label)
	   (pop-context)
	   (cons 'v-string (the-string)))
	  ((context looking-for-property all)
	   (pop-context)
	   (rgc-buffer-unget-char input-port (char->integer (the-character)))
	   (ignore))
	   
	  ;; Typecasts:
	  ((context in-scripting (: "("
				    tabs-and-spaces
				    (or "int" "integer")
				    tabs-and-spaces
				    ")"))
	   (update-line)
	   'op-int-cast)
	  ((context in-scripting (: "("
				    tabs-and-spaces
				    (or "real" "double" "float")
				    tabs-and-spaces
				    ")"))
	   (update-line)
	   'op-double-cast)
	  ((context in-scripting (: "("
				    tabs-and-spaces
				    "string"
				    ")"))
	   (update-line)
	   'op-string-cast)
	  ((context in-scripting (: "("
				    tabs-and-spaces
				    "array"
				    tabs-and-spaces
				    ")"))
	   (update-line)
	   'op-array-cast)
	  ((context in-scripting (: "("
				    tabs-and-spaces
				    "object"
				    tabs-and-spaces
				    ")"))
	   (update-line)
	   'op-object-cast)
	  ((context in-scripting (: "("
				    tabs-and-spaces
				    (or "bool" "boolean")
				    tabs-and-spaces
				    ")"))
	   (update-line)
	   'op-bool-cast)
	  ((context in-scripting (: "("
				    tabs-and-spaces
				    "unset"
				    tabs-and-spaces
				    ")"))
	   (update-line)
	   'op-unset-cast)

	  ;; {}
	  ((context in-scripting #\{)
	   (push-context 'in-scripting)
	   'op-lcbrace)
	  ((when (memq (the-context) '(double-quotes backquote heredoc)) "${")
	   (push-context 'looking-for-varname)
	   'op-$-lcbrace)
	  ((context looking-for-varname label)
	   (pop-context)
	   (push-context 'in-scripting)
	   (cons 'v-string-varname (the-string)))
	  ((context looking-for-varname all)
	   (set-context 'in-scripting)
	   (rgc-buffer-unget-char input-port (char->integer (the-character)))
	   (ignore))
	  ((context in-scripting #\})
	   (pop-context)
	   'op-rcbrace)

	  ;; Operators:
	  ((context in-scripting (or "::" "=>" "++" "--" "===" "<>" "+="
				     "!==" "==" "!=" "<=" ">=" "-=" "*=" "/="
				     ".="
				     "%=" ">>=" "<<=" "&=" "|=" "^=" "||" "&&" 
				     "<<" ">>"))
	   (cdr (assoc (the-string) php-keywords)))
	  ((context in-scripting tokens)
	   (update-line)
	   (cdr (assq (the-character) php-1char-operators)))

	  ((context in-scripting whitespace)
	   (update-line)
	   (if ignore-whitespace?
	     (ignore)
	     (cons 'v-whitespace (the-string))))

	  ;; Closing tags
	  ((context in-scripting
		    (: (or "?>" (uncase (: "</script" whitespace ">")))
		       (? (or "\n" "\r\n"))))
	   (update-line)
	   (set-context)
	   (set! value-buffer '())
	   'op-semicolon)
	  ((when (and (eq? (the-context) 'in-scripting)
		      asp-tags?) "%>")
	   (set-context)
	   (set! value-buffer '())
	   'op-semicolon)

	  ;; INITIAL (HTML) Mode
	  ((when (not (symbol? (the-context)))
	     (: (uncase (or "<?php" 
			    (: "<script"
			       whitespace
			       "language"
			       whitespace
			       "="
			       whitespace 
			       (or "'php'" "\"php\"")
			       whitespace
			       ">")))
		(in " \n\r\t")))
	   (update-line)
	   (set-context 'in-scripting)
	   (if (null? value-buffer)
	     (ignore)
	     (cons 'v-inline-html
		   (apply string-append (reverse value-buffer)))))
	  ((when (and (not (symbol? (the-context)))
		      asp-tags?)
		 "<%")
	   (set-context 'maybe-echo-in-scripting)
	   (if (null? value-buffer)
	     (ignore)
	     (cons 'v-inline-html
		   (apply string-append (reverse value-buffer)))))
	  ((when (and (not (symbol? (the-context)))
		      short-tags?)
		 "<?")
	   (set-context 'maybe-echo-in-scripting)
	   (if (null? value-buffer)
	     (ignore)
	     (cons 'v-inline-html
		   (apply string-append (reverse value-buffer)))))
	  ((when (not (symbol? (the-context))) (or (+ (out "<")) "<"))
	   (update-line)
	   (set! value-buffer (cons (the-string) value-buffer))
	   (ignore))

	  ((context maybe-echo-in-scripting #\=)
	   (set-context 'in-scripting)
	   'kw-echo)
	  ((context maybe-echo-in-scripting (out #\=))
	   (set-context 'in-scripting)
	   ;; Undocumented bigloo feature.
	   (rgc-buffer-unget-char (the-port) (char->integer (the-character)))
	   (ignore))

	  (else
	    (let ((char (the-failure)))
	      (if (eof-object? char)
		(cond
		  ;; If we have been collecting html and we encountered eof, 
		  ;; return the html.
		  ((and (not (symbol? (rgc-context)))
			(not (null? value-buffer)))
		   (let ((ret (cons 'v-inline-html
				    (apply string-append
					   (reverse value-buffer)))))
		     (set! value-buffer '())
		     ret))
		  (else
		    char))
		(error/location
		  "php-grammer"
		  "Illegal character"
		  (the-failure)
		  (input-port-name (the-port))
		  (input-port-position (the-port)))))))))

