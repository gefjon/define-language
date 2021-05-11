(uiop:define-package #:define-language/define-language
  (:nicknames #:define-language)
  (:use #:cl)
  (:export #:define-language))
(in-package #:define-language/define-language)

(defmacro define-language ((parse-fn define-singleton define-clause)
                           &optional fallthrough-arglist &body fallthrough-transform)
  "Define a language processor function PARSE-FUNCTION, with macros to define its behaviors DEFINE-SINGLETON \
and DEFINE-CLAUSE.

PARSE-FN will be defined as a generic function of one argument.

Methods of one argument can be defined by the DEFINE-SINGLETON macro, whose syntax is
  (DEFINE-SINGLETON (VAR CLOS-SPECIALIZER) &body BODY)
CLOS-SPECIALIZER may be any specializer suitable for `defmethod' other than `cons'

When PARSE-FN is invoked on an object which satisfies CLOS-SPECIALIZER, BODY will be evaluated with VAR bound \
to that object.

Methods for lists can be defined by the DEFINE-CLAUSE macro, whose syntax is
  (DEFINE-CLAUSE HEAD ARGLIST &body BODY)
When PARSE-FN is invoked on a list whose `first' is `eql' to HEAD, BODY will be evaluated with the ARGLIST \
bound to the `rest' of that list as if by `destructuring-bind'.
HEAD should usually be a symbol, but may be any object suitable for a CLOS `eql'-specializer. It will not be \
evaluated.

If a FALLTHROUGH-ARGLIST and a FALLTHROUGH-TRANSFORM are supplied, they will be used to transform lists which \
do not match any specialized clause. FALLTHROUGH-ARGLIST must be of the form (HEAD `&rest' TAIL), where HEAD \
and TAIL are symbols to be bound and `&rest' is the literal symbol `&rest'.

For example, you can define a language `print-expr' with:
  (define-language (print-expr define-expr define-expr-clause))
define its contents with:
  (define-expr (sym symbol)
    (print sym))
  (define-expr-clause foo (&rest syms)
    (format t \"foo~{ ~a~}\" syms))
and then print terms like:
  (print-expr 'foo) ; prints `FOO'
  (print-expr 'bar) ; prints `BAR'
  (print-expr '(foo foo bar) ; prints `foo FOO BAR'
  (print-expr 1) ; error
  (print-expr '(bar)) ; error"
  (let* ((parse-clause (gensym (format nil "~a-CLAUSE-" parse-fn)))
         (head (gensym "HEAD-"))
         (tail (gensym "TAIL-")))
    `(progn
       (defgeneric ,parse-clause (head &rest tail))
       ,@(when (and fallthrough-arglist fallthrough-transform)
           `((defmethod ,parse-clause ,fallthrough-arglist ,@fallthrough-transform)))
       (defgeneric ,parse-fn (term))
       (defmethod ,parse-fn ((term cons))
         (apply #',parse-clause term))
       (defmacro ,define-singleton ((var class) &body body)
         (when (eq class 'cons)
           (error "Attempt to override behavior of ~a which processes ~a"
                  ',define-singleton
                  ',define-clause))
         (list* 'defmethod ',parse-fn (list (list var class))
                body))
       (defmacro ,define-clause (head arglist &body body)
         (list 'defmethod ',parse-clause (list (list ',head (list 'eql (list 'quote head)))
                                               '&rest ',tail)
               (list* 'destructuring-bind arglist ',tail
                      body))))))
