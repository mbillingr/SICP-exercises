(import (builtin core)
        (sicp utils))

(include "chapter4-amb.scm")

(eval
  '(begin
    (define nouns '(noun student professor cat class))
    (define verbs '(verb studies lectures eats sleeps))
    (define articles '(article the a))
    (define prepositions '(prep by to in with for))
    (define adjectives '(adjective nice smart bad red green blue black))

    (define (parse-sentence)
      (list 'sentence
            (parse-noun-phrase)
            (parse-verb-phrase)))

    (define (parse-noun-phrase)
      (define (maybe-extend noun-phrase)
        (ramb noun-phrase
              (maybe-extend (list 'noun-phrase
                                  noun-phrase
                                  (parse-prepositional-phrase)))))
      (maybe-extend (parse-simple-noun-phrase)))

    (define (parse-simple-noun-phrase)
      (list 'simple-noun-phrase
            (parse-word articles)
            (parse-adj-noun)))

    (define (parse-adj-noun)
      (ramb (list 'adj-noun
                  (parse-word nouns))
            (list 'adj-noun
                  (parse-word adjectives)
                  (parse-word nouns))))

    (define (parse-verb-phrase)
      (define (maybe-extend verb-phrase)
        (ramb verb-phrase
              (maybe-extend (list 'verb-phrase
                                  verb-phrase
                                  (parse-prepositional-phrase)))))
      (maybe-extend (parse-word verbs)))

    (define (parse-prepositional-phrase)
      (list 'prep-phrase
            (parse-word prepositions)
            (parse-noun-phrase)))

    (define *output* '())

    (define (parse-word word-list)
      (define (words rest)
        (if (null? rest)
            (amb)
            (ramb (car rest)
                  (words (cdr rest)))))
      (set! *output* (cons (words (cdr word-list))
                           *output*))
      *output*)

    (define (reverse list)
      (define (iter in out)
        (if (null? in)
            out
            (iter (cdr in)
                  (cons (car in) out))))
      (iter list '()))

    (define (generate)
      (set! *output* '())
      (parse-sentence)
      (reverse *output*)))

  the-global-environment)

(n-solutions 3 '(generate) the-global-environment)

(driver-loop)
