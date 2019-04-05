//! This is a Rust implementation of SICP Exercise 2.4:
//!
//!     Here is analternative procedural representation of pairs. For this
//!     representation, verify that (car (cons x y)) yields x for any objects
//!     x and y.
//!
//!    (define (cons x y) (lambda (m) (m x y)))
//!    (define (car z) (z (lambda (p q) p)))
//!
//!    What is the corresponding deﬁnitionof cdr?
//!
//! The fact that the code compiles should suffice as verification.

use std::rc::Rc;

type Select<T> = Box<dyn Fn(T, T) -> T>;
type Cons<T> = Rc<dyn Fn(Select<T>) -> T>;

fn cons<T>(x: T, y: T) -> Cons<T>
where
    T: 'static + Clone,
{
    Rc::new(move |m| m(x.clone(), y.clone()))
}

fn car<T>(z: Cons<T>) -> T {
    z(Box::new(|p, _| p))
}

fn cdr<T>(z: Cons<T>) -> T {
    z(Box::new(|_, q| q))
}

fn main() {
    let z = cons(1, 2);

    println!("car: {}", car(z.clone()));
    println!("cdr: {}", cdr(z.clone()));

    let zz = cons(cons(1, 2), cons(3, 4));

    println!("car-car: {:?}", car(car(zz.clone())));
    println!("cdr-car: {:?}", car(cdr(zz.clone())));
}
