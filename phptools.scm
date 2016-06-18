;;----------------------------------------------------------------------------
;; PHPTOOLS - Bigloo tools for manipulating PHP source.
;; Copyright (C) 2002, 2006 Cronosys, LLC
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

(module phptools
  (import (php-rg "php-rg.scm"))
  (import (php-lalr "php-lalr.scm"))
  (import (analyze-globals "analyze-globals.scm"))
  (main main)
  )

(define (parse-proc)
  (let ((rg (make-php-rg)))
    (pp (read/lalrp php-lalrg rg (current-input-port)))))

(define (scan-proc)
  (let ((rg (make-php-rg)))
    (let token-loop ((t (read/rp rg (current-input-port))))
      (if (not (eof-object? t))
	(begin
	  (pp t)
	  (token-loop (read/rp rg (current-input-port))))))))

(define (check-proc)
  (let ((rg (make-php-rg)))
    (read/lalrp php-lalrg rg (current-input-port))))

(define *proc* parse-proc)
(define *processed-file* #f)

(define (help)
  (print
"phptools - Tools for manipulating PHP source.\n"
"Syntax:\n"
"  phptools COMMAND [OPTIONS...]\n"
"\n"
"Commands:\n"
"  analyze-globals [--raw] [FILE...]\n"
"  List global variables and show from where they are assigned and referenced.\n"
"  ``Raw'' mode will write a lisp-parseable S-expression; otherwise, phptools\n"
"  produces an ascii report.\n"
"\n"
"  check [FILE...]\n"
"  Check syntax of files, or stdin if no files are supplied.  Like `parse'\n"
"  except that the parse tree isn't written.\n"
"\n"
"  help\n"
"  Show this usage message.\n"
"\n"
"  parse [FILE...]\n"
"  Output parse tree of files, or stdin if no files are supplied.\n"
"\n"
"  scan [FILE...]\n"
"  Output lexical analysis of files, or stdin if no files are supplied.\n"
))

(define (main argv)
  (cond
    ((null? (cdr argv))
     (help))

    ((string=? (cadr argv) "analyze-globals")
     (let ((files (cddr argv))
	   (analyzer (make-global-analyzer))
	   (raw? #f))

       (if (and (not (null? files)) (string=? (car files) "--raw"))
	 (begin
	   (set! raw? #t)
	   (set! files (cdr files))))

       (if (null? files)
	 (analyzer 'analyze-file! "STDIN" (read/lalrp
					    php-lalrg
					    (make-php-rg)
					    (current-input-port)))
	 (let loop ((files files))
	   (if (not (null? files))
	     (begin
	       (analyzer
	         'analyze-file!
		 (car files)
		 (with-input-from-file (car files)
		   (lambda ()
		     (read/lalrp php-lalrg
				 (make-php-rg)
				 (current-input-port)))))
	       (loop (cdr files))))))
       (if raw?
	 (pp (analyzer 'result))
	 (display (analyzer 'report)))))

    ((string=? (cadr argv) "check")
     (let loop ((args (cddr argv))
		(did-file #f))
       (cond
	 ((null? args)
	  (if (not did-file)
	    (check-proc)))
	 (else
	  (with-input-from-file (car args) check-proc)
	  (loop (cdr args) #t)))))

    ((string=? (cadr argv) "help")
     (help))

    ((string=? (cadr argv) "parse")
     (let loop ((args (cddr argv))
		(did-file #f))
       (cond
	 ((null? args)
	  (if (not did-file)
	    (parse-proc)))
	 (else
	  (with-input-from-file (car args) parse-proc)
	  (loop (cdr args) #t)))))

    ((string=? (cadr argv) "scan")
     (let loop ((args (cddr argv))
		(did-file #f))
       (cond
	 ((null? args)
	  (if (not did-file)
	    (scan-proc)))
	 (else
	  (with-input-from-file (car args) scan-proc)
	  (loop (cdr args) #t)))))
    (else
     (help))))

