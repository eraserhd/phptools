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

(module php-lalr
	(export php-lalrg))

;; This folds consecutive string and character constants in an encaps list.
;; We can return a static string if there are no variables in the encaps list.
(define (make-encaps-list el)
  (define (whatever->string w)
    (cond
      ((string? w)
       w)
      ((char? w)
       (list->string (list w)))))
  (define (whatever? w)
    (or (string? w) (char? w)))

  (let loop ((iter el)
	     (result '()))
    (cond
      ((and (null? iter)
	    (=fx 1 (length result))
	    (string? (car result)))
       (car result))
      ((null? iter)
       (append (list 'encaps-list) (reverse result)))
      ((and (not (null? result))
	    (whatever? (car iter))
	    (whatever? (car result)))
       (set-car! result (string-append (whatever->string (car result))
				       (whatever->string (car iter))))
       (loop (cdr iter) result))
      ((whatever? (car iter))
       (loop (cdr iter) (cons (whatever->string (car iter)) result)))
      (else
       (loop (cdr iter) (cons (car iter) result))))))

;; This calculates a compound statement from two statements.  If the first
;; is a compound statement, the second statement is appended.  Special care
;; is taken to eliminate null statements.
(define (make-compound-statement s::symbol a b)
  (cond
    ((and (equal? a '(null-statement))
	  (equal? b '(null-statement)))
     '(null-statement))
    ((equal? a '(null-statement))
     b)
    ((equal? b '(null-statement))
     a)
    ((and (pair? a)
	  (eq? (car a) s))
     (append a (list b)))
    (else
     `(,s ,a ,b))))

;; Assign consecutive integral indices to array elements which did not have
;; keys specified in the array() statement.
(define (fix-array-indices! a)
  (let loop ((i 0)
	     (iter a))
    (if (null? iter)
      a
      (begin
	(if (eq? (caar iter) 'next-integer)
	  (begin
	    (set-car! (car iter) i)
	    (set! i (+fx i 1))))
	(loop i (cdr iter))))))

;; Consolidate `var' statements into a list of variables and `function'
;; statements into a list of methods.
(define (separate-class-decls l)
  (let loop ((iter l)
	     (vars '())
	     (methods '()))
    (cond
      ((null? iter)
       (cons vars methods))
      ((eq? (caar iter) 'var)
       (loop (cdr iter) (append vars (cdar iter)) methods))
      ((eq? (caar iter) 'function)
       (loop (cdr iter) vars (append methods (list (car iter))))))))

(define (make-if expr statement elseif-list else-single)
  `(if condition: ,expr
     statement: ,statement
     ,@(if (null? elseif-list)
	 '()
	 (list elseif-clauses: elseif-list))
     ,@else-single))

(define (make-static name . args)
  `(static-variable-declaration
     ,name
     ,@(if (null? args)
	 '()
	 (list initial-value: (car args)))))

(define php-lalrg
  (lalr-grammar
    (
     ;; Keywords:
     kw-array kw-as kw-break kw-case kw-class kw-const kw-continue
     kw-declare kw-default kw-do kw-echo kw-empty
     kw-enddeclare kw-endfor kw-endforeach kw-endswitch kw-endwhile
     kw-exit kw-extends kw-for kw-foreach kw-function kw-global kw-if
     kw-isset kw-list kw-old_function
     kw-return kw-static kw-switch kw-unset
     kw-var kw-while

     ;; Operators: Quoting
     op-q-single op-q-double op-q-back

     ;; Operators: Other
     op-rsqbrace op-lcbrace op-rcbrace op-$ op-semicolon op-lparen op-rparen 
     op-paamayim-nekudotayim op-=> op--> op-$-lcbrace op-lcbrace-$ 

     op-start-heredoc op-end-heredoc

     ;; Other Stuff:
     v-character
     v-comment
     v-constant-encapsed-string
     v-dnumber
     v-encapsed-and-whitespace
     v-inline-html
     v-lnumber
     v-num-string
     v-string
     v-string-varname
     v-variable
     v-whitespace

     (left: op-comma)
     (left: kw-or)
     (left: kw-xor)
     (left: kw-and)
     (right: kw-print)
     (left: op-= op-+= op--= op-*= op-/= op-.= op-%= op-&= op-bit-or= op-^=
       op-<<= op->>=)
     (left: op-? op-colon)
     (left: op-boolean-or)
     (left: op-&&)
     (left: op-bit-or)
     (left: op-^)
     (left: op-&)
     (none: op-== op-!= op-=== op-!==)
     (none: op-< op-<= op-> op->=)
     (left: op-<< op->>)
     (left: op-+ op-- op-.)
     (left: op-* op-/ op-%)
     (right: op-! op-~ op-++ op--- op-int-cast op-double-cast op-string-cast
       op-array-cast op-object-cast op-bool-cast op-unset-cast op-suppress)
     (right: op-lsqbrace)
     (none: kw-new)
     (left: kw-elseif)
     (left: kw-else)
     (left: kw-endif)
     (left: kw-include kw-include_once kw-eval kw-require kw-require_once))

    (top-statement-list
      (() `(null-statement))
      ((top-statement-list@a top-statement@b)
       (make-compound-statement 'compound-statement a b))
      )
    (top-statement
      ((statement@a) a)
      ((declaration-statement@a) a))
    (inner-statement-list
      (() `(null-statement))
      ((inner-statement-list@a inner-statement@b)
       (make-compound-statement 'compound-statement a b))
      )
    (inner-statement
      ((statement@a) a)
      ((declaration-statement@a) a)
      )
    (statement
      ((op-lcbrace inner-statement-list@a op-rcbrace) a)
      ((kw-if op-lparen expr op-rparen statement elseif-list else-single)
       (make-if expr statement elseif-list else-single))
      ((kw-if op-lparen expr op-rparen op-colon inner-statement-list
	new-elseif-list new-else-single kw-endif op-semicolon)
       (make-if expr inner-statement-list new-elseif-list new-else-single))
      ((kw-while op-lparen expr op-rparen while-statement)
       `(while ,expr ,while-statement))
      ((kw-do statement kw-while op-lparen expr op-rparen op-semicolon)
       `(do-while ,statement ,expr))
      ((kw-for op-lparen for-expr@a op-semicolon for-expr@b op-semicolon 
        for-expr@c op-rparen for-statement@d)
       `(for ,a ,b ,c ,d))
      ((kw-switch op-lparen expr op-rparen switch-case-list)
       `(switch ,expr ,@switch-case-list))
      ((kw-break op-semicolon)
       `(break 1))
      ((kw-break expr op-semicolon)
       `(break ,expr))
      ((kw-continue op-semicolon) 
       `(continue 1))
      ((kw-continue expr op-semicolon)
       `(continue ,expr))
      ((kw-return op-semicolon)
       `(return 'null))
      ((kw-return expr@a op-semicolon)
       `(return ,a))
      ((kw-global global-var-list op-semicolon)
       `(global ,@global-var-list))
      ((kw-static static-var-list op-semicolon)
       `(static ,@static-var-list))
      ((kw-echo echo-expr-list@a op-semicolon)
       `(echo ,@a))
      ((v-inline-html@a)
       `(echo ,a))
      ((expr@a op-semicolon) 
       a)
      ((kw-unset op-lparen unset-variables op-rparen op-semicolon)
       `(unset ,@unset-variables))
      ((kw-foreach op-lparen w-cvar@a kw-as w-cvar@b foreach-optional-arg@c
	op-rparen foreach-statement)
       `(foreach ,a ,(if c b #f) ,(if c c b) ,foreach-statement))
      ((kw-foreach op-lparen expr-without-variable@a kw-as w-cvar@b
	foreach-optional-arg@c op-rparen foreach-statement)
       `(foreach ,a ,(if c b #f) ,(if c c b) ,foreach-statement))
      ((kw-declare op-lparen declare-list op-rparen declare-statement)
       `(declare ,declare-list ,declare-statement))
      ((op-semicolon) '(null-statement))
      )
    (unset-variables
      ((unset-variable)
       (list unset-variable))
      ((unset-variables op-comma unset-variable)
       (append unset-variables (list unset-variable)))
      )
    (unset-variable
      ((cvar) cvar)
      )
    (declaration-statement
      ((kw-function is-reference v-string op-lparen parameter-list op-rparen
	op-lcbrace inner-statement-list@statement op-rcbrace)
       `(function ,v-string ,is-reference ,parameter-list ,statement))
      ((kw-old_function is-reference v-string parameter-list op-lparen
	inner-statement-list@statement op-rparen op-semicolon)
       `(function ,v-string ,is-reference ,parameter-list ,statement))
      ((kw-class v-string op-lcbrace class-statement-list op-rcbrace)
       `(class ,v-string #f ,@(separate-class-decls class-statement-list)))
      ((kw-class v-string@name kw-extends v-string@ancestor op-lcbrace 
	class-statement-list op-rcbrace)
       `(class ,name ,ancestor ,@(separate-class-decls class-statement-list)))
      )
    (foreach-optional-arg
      (() #f)
      ((op-=> w-cvar) w-cvar)
      )
    (for-statement
      ((statement@a) a)
      ((op-colon inner-statement-list@a kw-endfor op-semicolon) a)
      )
    (foreach-statement
      ((statement) statement)
      ((op-colon inner-statement-list@a kw-endforeach op-semicolon) a)
      )
    (declare-statement
      ((statement)
       statement)
      ((op-colon inner-statement-list kw-enddeclare op-semicolon)
       inner-statement-list)
      )
    (declare-list
      ((v-string op-= static-scalar)
       (list (cons (string->symbol v-string) static-scalar)))
      ((declare-list@l op-comma v-string op-= static-scalar)
       (append l (list (cons (string->symbol v-string) static-scalar))))
      )
    (switch-case-list
      ((op-lcbrace case-list op-rcbrace) case-list)
      ((op-lcbrace op-semicolon case-list op-rcbrace) case-list)
      ((op-colon case-list kw-endswitch op-semicolon) case-list)
      ((op-colon op-semicolon case-list kw-endswitch op-semicolon) case-list)
      )
    (case-list
      (()
       '())
      ((case-list kw-case expr case-separator inner-statement-list)
       (append case-list (list `(case ,expr ,inner-statement-list))))
      ((case-list kw-default case-separator inner-statement-list)
       (append case-list (list `(default ,inner-statement-list))))
      )
    (case-separator
      ((op-colon) #f)
      ((op-semicolon) #f)
      )
    (while-statement
      ((statement) statement)
      ((op-colon inner-statement-list kw-endwhile op-semicolon)
	inner-statement-list)
      )
    (elseif-list
      (()
       '())
      ((elseif-list kw-elseif op-lparen expr op-rparen statement)
       (append elseif-list (list `(elseif
				    condition: ,expr
				    statement: ,statement))))
      )
    (new-elseif-list
      (()
       '())
      ((new-elseif-list kw-elseif op-lparen expr op-rparen op-colon
	inner-statement-list)
       (append new-elseif-list (list `(elseif
					condition: ,expr
					statement: ,inner-statement-list))))
      )
    (else-single
      (()
       '())
      ((kw-else statement)
       `(else-statement: ,statement))
      )
    (new-else-single
      (()
       '())
      ((kw-else op-colon inner-statement-list)
       `(else-statement: ,inner-statement-list))
      )
    (parameter-list
      ((non-empty-parameter-list@a)
       a)
      (()
       '())
      )
    (non-empty-parameter-list
      ((v-variable)
       (list v-variable))
      ((op-& v-variable)
       (list `(by-reference ,v-variable)))
      ((kw-const v-variable)
       (list `(const ,v-variable)))
      ((v-variable op-= static-scalar)
       (list `(default ,v-variable ,static-scalar)))
      ((non-empty-parameter-list@l op-comma v-variable)
       (append l (list v-variable)))
      ((non-empty-parameter-list@l op-comma op-& v-variable)
       (append l (list `(by-reference ,v-variable))))
      ((non-empty-parameter-list@l op-comma kw-const v-variable)
       (append l (list `(const ,v-variable))))
      ((non-empty-parameter-list@l op-comma v-variable op-= static-scalar)
       (append l (list `(default ,v-variable ,static-scalar))))
      )
    (function-call-parameter
      ((expr-without-variable) 
       `(value-parameter ,expr-without-variable))
      ((cvar)
       `(referenceable-parameter ,cvar))
      ((op-& w-cvar)
       `(forced-reference-parameter ,w-cvar))
      )
    (function-call-parameter-list
      ((non-empty-function-call-parameter-list@a) a)
      (() '()))
    (non-empty-function-call-parameter-list
      ((function-call-parameter@a) (list a))
      ((non-empty-function-call-parameter-list@a op-comma
	function-call-parameter@b)
       (append a (list b))))
    (global-var-list
      ((global-var-list op-comma global-var)
       (append global-var-list (list global-var)))
      ((global-var)
       (list global-var))
      )
    (global-var
      ((v-variable) v-variable)
      ((op-$ r-cvar) `(var-value ,r-cvar))
      ((op-$ op-lcbrace expr op-rcbrace) expr)
      )
    (static-var-list
      ((static-var-list op-comma v-variable)
       (append static-var-list (list (make-static v-variable))))
      ((static-var-list op-comma v-variable op-= static-scalar)
       (append static-var-list (list (make-static v-variable static-scalar))))
      ((v-variable)
       (list (make-static v-variable)))
      ((v-variable op-= static-scalar)
       (list (make-static v-variable static-scalar)))
      )
    (class-statement-list
      ((class-statement-list@l class-statement)
       (append l (list class-statement)))
      (()
       '())
      )
    (class-statement
      ((kw-var class-variable-declaration op-semicolon)
       `(var ,@class-variable-declaration))
      ((kw-function is-reference v-string op-lparen parameter-list op-rparen
	op-lcbrace inner-statement-list@statement op-rcbrace)
       `(function ,v-string ,is-reference ,parameter-list ,statement))
      ((kw-old_function is-reference v-string parameter-list op-lparen
	inner-statement-list@statement op-rparen op-semicolon)
       `(function ,v-string ,is-reference ,parameter-list ,statement))
      )
    (is-reference
      (() #f)
      ((op-&) #t)
      )
    (class-variable-declaration
      ((class-variable-declaration@l op-comma v-variable)
       (append l (list (cons v-variable 'null))))
      ((class-variable-declaration@l op-comma v-variable op-= static-scalar)
       (append l (list (cons v-variable static-scalar))))
      ((v-variable) 
       (list (cons v-variable 'null)))
      ((v-variable op-= static-scalar)
       (list (cons v-variable static-scalar)))
      )
    (echo-expr-list
      ((echo-expr-list@a op-comma expr@b) (append a (list b)))
      ((expr@a) (list a))
      )
    (for-expr
      (() #f)
      ((non-empty-for-expr@a) a))
    (non-empty-for-expr
      ((expr) expr)
      ((non-empty-for-expr@a op-comma expr@b)
       (make-compound-statement 'for-expr a b))
      )
    (expr-without-variable
      ((kw-list op-lparen assignment-list op-rparen op-= expr)
       `(list-assign ,assignment-list ,expr))
      ((cvar op-= expr) `(op-= ,cvar ,expr))
      ((cvar op-= op-& w-cvar)
       `(reference-assign ,cvar ,w-cvar))
      ((cvar op-= op-& function-call)
       `(reference-assign ,cvar ,function-call))
      ((cvar op-= op-& kw-new class-name ctor-arguments)
       `(reference-assign ,cvar (new ,class-name ,ctor-arguments)))
      ((kw-new class-name ctor-arguments)
       `(var-value (new ,class-name ,ctor-arguments)))
      ((cvar op-+= expr) `(op-+= ,cvar ,expr))
      ((cvar op--= expr) `(op--= ,cvar ,expr))
      ((cvar op-*= expr) `(op-*= ,cvar ,expr))
      ((cvar op-/= expr) `(op-/= ,cvar ,expr))
      ((cvar op-.= expr) `(op-.= ,cvar ,expr))
      ((cvar op-%= expr) `(op-%= ,cvar ,expr))
      ((cvar op-&= expr) `(op-&= ,cvar ,expr))
      ((cvar op-bit-or= expr) `(op-bit-or= ,cvar ,expr))
      ((cvar op-^= expr) `(op-^= ,cvar ,expr))
      ((cvar op-<<= expr) `(op-<<= ,cvar ,expr))
      ((cvar op->>= expr) `(op->>= ,cvar ,expr))
      ((rw-cvar op-++) `(op-post++ ,rw-cvar))
      ((op-++ rw-cvar) `(op-pre++ ,rw-cvar))
      ((rw-cvar op---) `(op-post-- ,rw-cvar))
      ((op--- rw-cvar) `(op-pre-- ,rw-cvar))
      ((expr@a op-boolean-or expr@b) `(op-boolean-or ,a ,b))
      ((expr@a op-&& expr@b) `(op-&& ,a ,b))
      ((expr@a kw-or expr@b) `(kw-or ,a ,b))
      ((expr@a kw-and expr@b) `(kw-and ,a ,b))
      ((expr@a kw-xor expr@b) `(kw-xor ,a ,b))
      ((expr@a op-bit-or expr@b) `(op-bit-or ,a ,b))
      ((expr@a op-& expr@b) `(op-& ,a ,b))
      ((expr@a op-^ expr@b) `(op-^ ,a ,b))
      ((expr@a op-. expr@b) `(op-. ,a ,b))
      ((expr@a op-+ expr@b) `(op-+ ,a ,b))
      ((expr@a op-- expr@b) `(op-- ,a ,b))
      ((expr@a op-* expr@b) `(op-* ,a ,b))
      ((expr@a op-/ expr@b) `(op-/ ,a ,b))
      ((expr@a op-% expr@b) `(op-% ,a ,b))
      ((expr@a op-<< expr@b) `(op-<< ,a ,b))
      ((expr@a op->> expr@b) `(op->> ,a ,b))
      ((op-+ expr) `(op-+ ,expr))
      ((op-- expr) `(op-- ,expr))
      ((op-! expr) `(op-! ,expr))
      ((op-~ expr) `(op-~ ,expr))
      ((expr@a op-=== expr@b) `(op-=== ,a ,b))
      ((expr@a op-!== expr@b) `(op-!== ,a ,b))
      ((expr@a op-== expr@b) `(op-== ,a ,b))
      ((expr@a op-!= expr@b) `(op-!= ,a ,b))
      ((expr@a op-< expr@b) `(op-< ,a ,b))
      ((expr@a op-<= expr@b) `(op-<= ,a ,b))
      ((expr@a op-> expr@b) `(op-> ,a ,b))
      ((expr@a op->= expr@b) `(op->= ,a ,b))
      ((op-lparen expr@a op-rparen) a)
      ((expr@a op-? expr@b op-colon expr@c) `(op-?-colon ,a ,b ,c))
      ((function-call@a) `(var-value ,a))
      ((internal-functions-in-yacc@a) a)
      ((op-int-cast expr) `(op-int-cast ,expr))
      ((op-double-cast expr) `(op-double-cast ,expr))
      ((op-string-cast expr) `(op-string-cast ,expr))
      ((op-array-cast expr) `(op-array-cast ,expr))
      ((op-object-cast expr) `(op-object-cast ,expr))
      ((op-bool-cast expr) `(op-bool-cast ,expr))
      ((op-unset-cast expr) `(op-unset-cast ,expr))
      ((kw-exit exit-expr) `(exit ,exit-expr))
      ((op-suppress expr) `(op-suppress ,expr))
      ((scalar) scalar)
      ((kw-array op-lparen array-pair-list op-rparen)
       `(array ,@array-pair-list))
      ((op-q-back encaps-list op-q-back)
       `(backquote ,(make-encaps-list encaps-list)))
      ((kw-print expr) `(print ,expr))
      )
    (function-call
      ((v-string op-lparen function-call-parameter-list@params op-rparen)
       `(function-call #f ,v-string ,@params))
      ((cvar op-lparen function-call-parameter-list@params op-rparen)
       `(function-call #f ,cvar ,@params))
      ((v-string@class op-paamayim-nekudotayim v-string@method op-lparen
	function-call-parameter-list@params op-rparen)
       `(function-call ,class ,method ,@params))
      )
    (exit-expr
      (() 0)
      ((op-lparen op-rparen) 0)
      ((op-lparen expr op-rparen) expr))
    (ctor-arguments
      (()
       '())
      ((op-lparen function-call-parameter-list@a op-rparen) a)
      )
    (class-name
      ((v-string) v-string)
      ((r-cvar) r-cvar))
    (common-scalar
      ((v-lnumber) v-lnumber)
      ((v-dnumber) v-dnumber)
      ((v-constant-encapsed-string) v-constant-encapsed-string))
    (static-scalar
      ((common-scalar) common-scalar)
      ((v-string) v-string)
      ((op-+ static-scalar) `(op-+ ,static-scalar))
      ((op-- static-scalar) `(op-- ,static-scalar))
      ((kw-array op-lparen static-array-pair-list op-rparen)
       `(array ,@static-array-pair-list))
      )
    (scalar
      ((v-string) v-string)
      ((v-string-varname) v-string-varname)
      ((common-scalar) common-scalar)
      ((op-q-double encaps-list op-q-double) 
       (make-encaps-list encaps-list))
      ((op-q-single encaps-list op-q-single) 
       (make-encaps-list encaps-list))
      ((op-start-heredoc encaps-list op-end-heredoc)
       (make-encaps-list encaps-list))
      )
    (static-array-pair-list
      (() 
       '())
      ((non-empty-static-array-pair-list@a possible-comma)
       (fix-array-indices! a)))
    (possible-comma
      (() #f)
      ((op-comma) #f)
      )
    (non-empty-static-array-pair-list
      ((non-empty-static-array-pair-list@l op-comma static-scalar@k op-=>
	static-scalar@v)
       (append l (list (cons k `(value ,v)))))
      ((non-empty-static-array-pair-list@l op-comma static-scalar)
       (append l (list (cons 'next-integer `(value ,static-scalar)))))
      ((static-scalar@k op-=> static-scalar@v)
       (list (cons k `(value ,v))))
      ((static-scalar)
       (list (cons 'next-integer `(value ,static-scalar))))
      )
    (expr
      ((r-cvar) `(var-value ,r-cvar))
      ((expr-without-variable) expr-without-variable)
      )
    (r-cvar
      ((cvar) cvar))
    (w-cvar
      ((cvar) cvar))
    (rw-cvar
      ((cvar) cvar))
    (cvar
      ((cvar-without-objects@a) a)
      ((cvar-without-objects op--> ref-list)
       ; Zend's parser is messed up here, and we're trying to mirror it.  So,
       ; in order to get a decent data structure out of it, we have to do
       ; this inversion. `ref-list' is a list of object references (e.g.
       ; a -> b -> c), where each element is a list of array references with
       ; the first element being the name of the property.
       (let fix ((v cvar-without-objects)
		 (rl ref-list))
	 (cond
	   ((null? rl)
	    v)
	   (else
	     (let dim-fix ((v `(op--> (var-value ,v) ,(caar rl)))
			   (dim-iter (cdar rl)))
	       (cond
		 ((null? dim-iter)
		  (fix v (cdr rl)))
		 (else
		  (dim-fix `(array-lookup (var-value ,v) ,(car dim-iter)) 
			   (cdr dim-iter)))))))))
      )
    (cvar-without-objects
      ((reference-variable@a) a)
      ((simple-indirect-reference reference-variable)
       (let loop ((i simple-indirect-reference)
		  (r reference-variable))
	 (if (=fx i 0)
	   r
	   (loop (-fx i 1) `(var-lookup ,r)))))
      )
    (reference-variable
      ((reference-variable op-lsqbrace dim-offset op-rsqbrace)
       `(array-lookup ,reference-variable ,dim-offset))
      ((reference-variable op-lcbrace expr op-rcbrace)
       `(array-lookup ,reference-variable ,expr))
      ((compound-variable) compound-variable)
      )
    (compound-variable
      ((v-variable)
       `(var-lookup ,v-variable))
      ((op-$ op-lcbrace expr op-rcbrace)
       `(var-lookup ,expr))
      )
    (dim-offset
      (() '())
      ((expr) expr))
    (ref-list
      ((object-property@a) (list a))
      ((ref-list@l op--> object-property@a)
       (append l (list a)))
      )
    (object-property
      ((object-dim-list) object-dim-list)
      ((cvar-without-objects) cvar-without-objects)
      )
    (object-dim-list
      ((object-dim-list@l op-lsqbrace dim-offset@a op-rsqbrace)
       (append l (list a)))
      ((object-dim-list@l op-lcbrace expr@a op-rcbrace)
       (append l (list a)))
      ((variable-name) 
       (list variable-name))
      )
    (variable-name
      ((v-string) v-string)
      ((op-lcbrace expr op-rcbrace) expr))
    (simple-indirect-reference
      ((op-$) 1)
      ((simple-indirect-reference op-$) (+fx simple-indirect-reference 1)))
    (assignment-list
      ((assignment-list op-comma assignment-list-element)
       (append assignment-list (list assignment-list-element)))
      ((assignment-list-element)
       (list assignment-list-element)))
    (assignment-list-element
      ((cvar) cvar)
      ((kw-list op-lparen assignment-list op-rparen)
       `(assignment-list ,assignment-list))
      (()
       '())
      )
    (array-pair-list
      (()
       '())
      ((non-empty-array-pair-list possible-comma)
       (fix-array-indices! non-empty-array-pair-list))
      )
    (non-empty-array-pair-list
      ((non-empty-array-pair-list@l op-comma expr@a op-=> expr@b)
       (append l (list (cons a `(value ,b)))))
      ((non-empty-array-pair-list@l op-comma expr@a)
       (append l (list (cons 'next-integer `(value ,a)))))
      ((expr@a op-=> expr@b)
       (list (cons a `(value ,b))))
      ((expr)
       (list (cons 'next-integer `(value ,expr))))
      ((non-empty-array-pair-list@l op-comma expr op-=> op-& w-cvar)
       (append l (list (cons expr `(reference ,w-cvar))))
       )
      ((non-empty-array-pair-list@l op-comma op-& w-cvar)
       (append l (list (cons 'next-integer `(reference ,w-cvar))))
       )
      ((expr op-=> op-& w-cvar)
       (list (cons expr `(reference ,w-cvar)))
       )
      ((op-& w-cvar)
       (list (cons 'next-integer `(reference ,w-cvar)))
       )
      )
    (encaps-list
      ((encaps-list@l encaps-var@v) (append l (list `(var-value ,v))))
      ((encaps-list@l v-string)	(append l (list v-string)))
      ((encaps-list@l v-num-string) (append l (list v-num-string)))
      ((encaps-list@l v-encapsed-and-whitespace@s) (append l (list s)))
      ((encaps-list@l v-character@s) (append l (list s)))
      ((encaps-list@l op-lsqbrace@s) (append l (list s)))
      ((encaps-list@l op-rsqbrace@s) (append l (list s)))
      ((encaps-list@l op-lcbrace@s) (append l (list s)))
      ((encaps-list@l op-rcbrace@s) (append l (list s)))
      ((encaps-list@l op-->@s) (append l (list s)))
      (() '())
      )
    (encaps-var
      ((v-variable)
       `(var-lookup ,v-variable))
      ((v-variable op-lsqbrace encaps-var-offset op-rsqbrace)
       `(array-lookup (var-lookup ,v-variable) ,encaps-var-offset))
      ((v-variable@a op--> v-string@b)
       `(op--> (var-lookup ,a) ,b))
      ((op-$-lcbrace expr@a op-rcbrace)
       `(var-lookup ,a))
      ((op-$-lcbrace v-string-varname op-lsqbrace expr op-rsqbrace op-rcbrace)
       `(array-lookup (var-lookup ,v-string-varname) ,expr))
      ((op-lcbrace-$ cvar op-rcbrace)
       `(var-loookup ,cvar))
      )
    (encaps-var-offset
      ((v-string) v-string)
      ((v-num-string) v-num-string)
      ((v-variable) `(var-lookup ,v-variable))
      )
    (internal-functions-in-yacc
      ((kw-isset op-lparen cvar op-rparen) `(isset ,cvar))
      ((kw-empty op-lparen cvar op-rparen) `(empty ,cvar))
      ((kw-include expr) `(include ,expr))
      ((kw-include_once expr) `(include_once ,expr))
      ((kw-eval op-lparen expr op-rparen) `(eval ,expr))
      ((kw-require expr) `(require ,expr))
      ((kw-require_once expr) `(require_once ,expr))
      )
    ))
