(import (builtin core)
        (sicp utils))

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((and (leaf? tree)
              (eq? (symbol-leaf tree) symbol))
         '())
        ((element-of-set? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((element-of-set? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol not in tree: ENCODE SYMBOL" symbol))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)     ; symbol
                               (cadr pair))   ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (successive-merge
        (adjoin-set (make-code-tree (car leaf-set)
                                    (cadr leaf-set))
                    (cddr leaf-set)))))

(define rock-song-alphabet (list (list 'a 2)
                                 (list 'get 2)
                                 (list 'sha 3)
                                 (list 'wah 1)
                                 (list 'boom 1)
                                 (list 'job 2)
                                 (list 'na 16)
                                 (list 'yip 9)))

(define rock-song-tree (generate-huffman-tree rock-song-alphabet))
(println rock-song-tree)

(define rock-song '(get a job
                    sha na na na na na na na na
                    get a job
                    sha na na na na na na na na
                    wah yip yip yip yip yip yip yip yip yip
                    sha boom))
(println rock-song)

(define encoded (encode rock-song rock-song-tree))
(println encoded)
(println "Number of bits:" (length encoded))

(let ((n-bits (/ (log (length rock-song-alphabet))
                 (log 2))))
  (println "Number of bits assuming a fixed-length code:" (* (length rock-song) n-bits)))
