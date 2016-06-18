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

(module agtest
	(import (php-rg "php-rg.scm")
		(php-lalr "php-lalr.scm")
		(analyze-globals "analyze-globals.scm")
		(testlib "testlib.scm")))

(run-tests "ag"
  (lambda (t)
    (let* ((g (apply make-php-rg (meta-prop t options: '())))
	   (i (open-input-string (meta-prop t code: "")))
	   (expect (meta-prop t result: '()))
	   (analyzer (make-global-analyzer))
	   (got (begin
		  (analyzer 'analyze-file! "STDIN" (read/lalrp php-lalrg g i))
		  (analyzer 'result))))
      (if (equal? expect got)
	#t
	got))))
