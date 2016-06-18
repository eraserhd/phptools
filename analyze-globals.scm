;;----------------------------------------------------------------------------
;; PHPTOOLS - Bigloo tools for manipulating PHP source.
;; Copyright (C) 2006 Cronosys, LLC
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

(module analyze-globals
	(export make-global-analyzer))

(define (make-global-analyzer)
  (define result '())
  (define current-filename "STDIN")
  (define current-function #f)
  (define current-class #f)

  (define (property l keyw)
    (let loop ((iter l))
      (cond
	((null? iter)
	 #f)

	((eq? (car iter) keyw)
	 (cadr iter))

	(else
	 (loop (cdr iter))))))

  (define (set-property! l keyw value)
    (let loop ((iter l))
      (cond
	((null? iter)
	 #f)

	((eq? (car iter) keyw)
	 (set-car! (cdr iter) value))

	(else
	 (loop (cdr iter))))))

  (define (find-variable autoglobal name)
    (let loop ((iter result))
      (cond
	((null? iter)
	 #f)

	((and (string=? (property (car iter) autoglobal:) autoglobal)
	      (string=? (property (car iter) name:) name))
	 (car iter))

	(else
	 (loop (cdr iter))))))

  (define have-variable? find-variable)

  (define (add-variable! autoglobal name)
    (if (not (have-variable? autoglobal name))
      (set! result (cons (list autoglobal: autoglobal
			       name: name
			       referenced-by: '())
			 result))))

  (define (caller)
    (append (list file: current-filename)
	    (if current-class
	      (list class: current-class)
	      '())
	    (if current-function
	      (list function: current-function)
	      '())))

  (define (add-reference! autoglobal name)
    (add-variable! autoglobal name)
    (let ((vardef (find-variable autoglobal name)))
      (set-property! vardef
		     referenced-by:
		     (cons (caller) (property vardef referenced-by:)))))

  (define (autoglobal? agname)
    (or (string=? agname "GLOBALS")
	(string=? agname "_SERVER")
	(string=? agname "_GET")
	(string=? agname "_POST")
	(string=? agname "_COOKIE")
	(string=? agname "_FILES")
	(string=? agname "_ENV")
	(string=? agname "_REQUEST")
	(string=? agname "_SESSION")))

  (define (current-function-name)
    (cond
     ((and current-class current-function)
      (string-append current-class "::" current-function))
     (current-function
      current-function)
     (current-class
      (string-append current-class "::"))
     (else
      "?")))

  (define (analyze! statement)
    (match-case statement
      ((array-lookup (var-lookup (and (? string?)
				      (? autoglobal?)
				      ?autoglobal))
		     ?name)
       (if (string? name)
         (add-reference! autoglobal (string-append "'" name "'"))
	 (add-reference! autoglobal "?")))

      ((class ?class-name ?- ?- ???methods)
       (set! current-class class-name)
       (let loop ((iter methods))
	 (if (not (null? iter))
	   (begin
	     (analyze! (car iter))
	     (loop (cdr iter)))))
       (set! current-class #f))

      ((function ?name ?- ?- ?body-statement)
       (set! current-function name)
       (analyze! body-statement)
       (set! current-function #f))

      ((static-variable-declaration ?name ???-)
       (add-reference! (string-append "static/" (current-function-name)) name))

      ((global ???decls)
       (let loop ((decls decls))
	 (if (not (null? decls))
	   (begin
	     (add-reference! "GLOBALS" (string-append "'" (car decls) "'"))
	     (loop (cdr decls))))))

      (else
       (let loop ((iter statement))
	 (if (not (null? iter))
	   (begin
	     (if (list? (car iter))
	       (analyze! (car iter)))
	     (loop (cdr iter))))))))

  (define (analyze-file! filename parse-tree)
    (set! current-filename filename)
    (set! current-function #f)
    (analyze! parse-tree))

  (define (function-name record)
    (cond
     ((and (property record class:)
	   (property record function:))
      (string-append
	(property record class:)
	"::"
	(property record function:)
	"()"))

     ((property record class:)
      (string-append (property record class:) "::"))

     ((property record function:)
      (string-append (property record function:) "()"))

     (else
      #f)))

  (define (global-name record)
    (let ((agname (property record autoglobal:)))
      (if (and (> (string-length agname) 7)
	       (string=? (substring (property record autoglobal:) 0 7)
		       "static/"))
        (string-append "static $" (property record name:))
        (string-append "$"
		      (property record autoglobal:)
		      "["
		      (property record name:)
		      "]"))))

  (define (reference-name record)
    (string-append
      (if (or (property record class:)
	      (property record function:))
	(string-append (function-name record) " in ")
	"")
      (if (property record file:)
	(property record file:)
	"?")))

  (define (make-report)
    (let* ((strings (list
"\n"
" PHPTOOLS GLOBAL VARIABLE ANALYSIS\n"
" ---------------------------------------------------------------------------\n"
))
	   (s! (lambda (s) (set! strings (append strings (list s))))))
      (let global-loop ((global-iter result))
	(if (not (null? global-iter))
	  (begin
	    (s! " ")
	    (s! (global-name (car global-iter)))
	    (s! " (")
	    (s! (number->string (length (property (car global-iter) referenced-by:))))
	    (s! " references)\n")
	    (let ref-loop ((refs (property (car global-iter) referenced-by:)))
	      (if (not (null? refs))
		(begin
		  (s! "                   ")
		  (s! (reference-name (car refs)))
		  (s! "\n")
		  (ref-loop (cdr refs)))))
	    (s! "\n")
	    (global-loop (cdr global-iter)))))
      (s! " ---------------------------------------------------------------------------\n")
      (s! " ")
      (s! (number->string (length result)))
      (s! " globals analyzed\n")
      (s! "\n")
      (apply string-append strings)))

  (define (dispatch message . args)
    (case message
      ('analyze-file! (apply analyze-file! args))
      ('result result)
      ('report (make-report))))

  dispatch)

