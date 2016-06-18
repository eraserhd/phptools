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

(module testlib
	(export run-tests
		read-test
		meta-prop
		fail
		*test-dir*))
    
(define *test-dir* (string-append (getenv "srcdir") "/tests"))

(define (read-test name)
  (let* ((input-name (string-append *test-dir* "/" name))
	 (input-file (open-input-file input-name))
	 (meta '()))
    (let rloop ((kw (read input-file)))
      (cond
	((eof-object? kw)
	 (error "rgtest" "End of file in test" name))
	((not (keyword? kw))
	 (error "rgtest" "Keyword expected" kw))
	((eq? kw code:)
	 ;; Don't use the reader here, just snarf until end of file.
	 (read-line input-file) ; Eat whitespace after code:
	 (let lloop ((line (read-line input-file))
		     (text '()))
	   (if (not (eof-object? line))
	     (lloop (read-line input-file) (cons line 
						 (if (null? text)
						   '()
						   (cons "\n" text))))
	     (begin
	       (set! meta (cons (cons code:
				      (apply string-append (reverse text)))
			        meta))
	       (close-input-port input-file)
	       meta))))
	(else
	 (set! meta (cons (cons kw (read input-file)) meta))
	 (rloop (read input-file)))))))

(define (meta-prop m name def)
  (if (assq name m)
    (cdr (assq name m))
    def))

(define (fail name t got)
  (print "*** TEST FAILED ***")
  (display "name: ")
  (write name)
  (newline)
  (display "code: ")
  (write (meta-prop t code: ""))
  (newline)
  (print "expected: ")
  (pp (meta-prop t result: '()))
  (newline)
  (print "got: ")
  (pp got)
  (newline)
  #f)

(define (run-tests prefix proc)
  (let test-loop ((tests (directory->list *test-dir*)))
    (cond
      ((null? tests)
       #f)
      ((and (> (string-length (car tests)) 2)
	    (string=? prefix (substring (car tests) 0 (string-length prefix))))
       (let* ((name (car tests))
	      (t (read-test name))
	      (ans (proc t)))
	 (if (not (eq? ans #t))
	   (fail name t ans)))
       (test-loop (cdr tests)))

      (else
       (test-loop (cdr tests))))))
