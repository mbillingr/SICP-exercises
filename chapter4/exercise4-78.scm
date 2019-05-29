(import (builtin core)
        (sicp utils))

(include "chapter4-query.scm")
(include "chapter4-amb.scm")

; Implement the query evaluator as a program that runs in the amb evaluator
; Simple queryies work. It is possible to assert facts and rules, which can be
; queried correctly.
; Compound queries do not work. While trying to implement conjoin the program
; started to run in a stack overflow, which manifests by try-next in analyze-amb
; being called reapeatedly with the same proceduces.
; I have not found the problem. It might be a bug in this exercise's
; implementation or a bug in the amb evaluator.

(define query-primitives
  (list (list 'assertions (lambda () THE-ASSERTIONS))
        (list 'rules (lambda () THE-RULES))
        (list 'var? var?)
        (list 'binding-value binding-value)
        (list 'binding-in-frame binding-in-frame)
        (list 'extend extend)
        (list 'rename-variables-in rename-variables-in)
        (list 'conclusion conclusion)
        (list 'rule-body rule-body)
        (list 'depends-on? depends-on?)
        (list 'query-syntax-process query-syntax-process)
        (list 'contract-question-mark contract-question-mark)
        (list 'instantiate instantiate)
        (list 'empty-conjunction? empty-conjunction?)
        (list 'rest-conjuncts rest-conjuncts)
        (list 'first-conjunct first-conjunct)
        (list 'type type)
        (list 'contents contents)))

(define the-global-environment
  (extend-environment (primitive-procedure-names query-primitives)
                      (primitive-procedure-objects query-primitives)
                      the-global-environment))

(eval
  '(begin
    (define (qeval query answer)
      (println 'qeval query answer)
      (let ((qtype (type query)))
        (cond ((eq? qtype 'and) (conjoin (contents query) answer))
              (else (simple-query query answer)))))

    (define (pretty-qeval query)
      (println "===========================================================================")
      (println "|" query)
      (println "+--------------------------------------------------------------------------")
      (let ((q (query-syntax-process query)))
        (let ((result (instantiate q
                                   (qeval q '())
                                   (lambda (v f)
                                     (contract-question-mark v)))))
          (display "| ")
          result)))

    (define (simple-query query-pattern answer)
      (println 'simple-query query-pattern answer)
      (amb (find-assertions query-pattern answer)
           (apply-rules query-pattern answer)))

    (define (conjoin conjuncts answer)
      (qeval (first-conjunct conjuncts) answer))
      ;(if (empty-conjunction? conjuncts)))
          ;answer))

          ;(conjoin (rest-conjuncts conjuncts)
          ;         (qeval (first-conjunct conjuncts)
          ;                answer))

    (define (find-assertions pattern answer)
      (let ((result
              (pattern-match pattern
                             (fetch-assertions pattern answer)
                             answer)))
        result))

    (define (pattern-match pat dat answer)
      (cond ((equal? pat dat) answer)
            ((var? pat) (extend-if-consistent pat dat answer))
            ((and (pair? pat) (pair? dat))
             (pattern-match (cdr pat)
                            (cdr dat)
                            (pattern-match (car pat)
                                           (car dat)
                                           answer)))
            (else (amb))))

    (define (extend-if-consistent var dat frame)
      (let ((binding (binding-in-frame var frame)))
        (if binding
            (pattern-match (binding-value binding) dat frame)
            (extend var dat frame))))

    (define (apply-rules pattern answer)
      (println 'apply-rules)
      (let ((clean-rule (rename-variables-in (fetch-rules pattern answer))))
        (qeval (rule-body clean-rule)
               (unify-match pattern
                            (conclusion clean-rule)
                            answer))))

    (define (unify-match p1 p2 answer)
      (cond ((equal? p1 p2) answer)
            ((var? p1) (extend-if-possible p1 p2 answer))
            ((var? p2) (extend-if-possible p2 p1 answer))
            ((and (pair? p1) (pair? p2))
             (unify-match (cdr p1)
                          (cdr p2)
                          (unify-match (car p1)
                                       (car p2)
                                       answer)))
            (else (amb))))

    (define (extend-if-possible var val answer)
      (let ((binding (binding-in-frame var answer)))
        (cond (binding
                (unify-match
                  (binding-value binding) val answer))
              ((var? val)
               (let ((binding (binding-in-frame val answer)))
                 (if binding
                     (unify-match
                       var (binding-value binding) answer)
                     (extend var val answer))))
              ((depends-on? val var answer)
               (amb))
              (else (extend var val answer)))))

    (define (fetch-assertions pattern frame)
      (list->amb (assertions)))

    (define (list->amb rest)
      (if (null? rest)
          (amb)
          (amb (car rest) (list->amb (cdr rest)))))

    (define (fetch-rules pattern frame)
      (list->amb (rules))))

  the-global-environment)

(define THE-ASSERTIONS '())
(define THE-RULES '())

(define (assert! a)
  (add-rule-or-assertion! (query-syntax-process a)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assert)
  (set! THE-ASSERTIONS (cons assert THE-ASSERTIONS))
  'ok)

(define (add-rule! rule)
  (set! THE-RULES (cons rule THE-RULES))
  'ok)

; ------- test it -------

(assert! '(son Adam Cain))
(assert! '(son Cain Enoch))
(assert! '(father Max Ben))

(assert! '(rule (son ?x ?y) (father ?y ?x)))

(all-solutions '(pretty-qeval '(son ?x ?y))
               the-global-environment)

(include "chapter4-sample-db.scm")

;(all-solutions
;  '(pretty-qeval
;    '(job ?x ?y)
;  the-global-environment)

;(assert! '(rule (big-shot ?person ?division)
;                (and (job ?person (?division . ?job))
;                     (or (and (supervisor ?person ?boss)
;                              (not (job ?boss (?division . ?bjob)))
;                         (not (supervisor ?person ?boss)))  ; people without supervisor are big shots too

;(all-solutions
;  '(pretty-qeval
;    '(big-shot ?who ?division)
;  the-global-environment)

(all-solutions
  '(pretty-qeval
    '(job ?person (computer programmer)))
  the-global-environment)

(all-solutions
  '(pretty-qeval
    '(and (job ?person (computer programmer))
          (address ?person ?where)))
  the-global-environment)

(driver-loop)
