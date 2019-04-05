# This is a Python implementation of SICP Exercise 2.4:
#
#     Here is analternative procedural representation of pairs. For this
#     representation, verify that (car (cons x y)) yields x for any objects
#     x and y.
#
#    (define (cons x y) (lambda (m) (m x y)))
#    (define (car z) (z (lambda (p q) p)))
#
#    What is the corresponding deﬁnitionof cdr?
#
# For verification, see the Scheme (and maybe the Rust) implementation.

def cons(x, y):
    return lambda m: m(x, y)

def car(z):
    return z(lambda p, q: p)

def cdr(z):
    return z(lambda p, q: q)


z = cons(1, 2)
print("car:", car(z))
print("cdr:", cdr(z))

zz = cons(cons(1, 2), cons(3, 4))
print("car-car:", car(car(zz)))
print("cdr-car:", car(cdr(zz)))
