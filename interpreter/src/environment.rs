use crate::errors::*;
use crate::expression::{Args, Expression, Procedure, Symbol};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Add, Div, Mul, Sub};
use std::rc::Rc;

pub type EnvRef = Rc<RefCell<Environment>>;

pub fn default_env() -> EnvRef {
    use Expression as X;

    let mut map = HashMap::new();

    // simple i/o

    map.insert("display".to_string(), X::Native(native_display));

    // numerical operations

    map.insert(
        "+".to_string(),
        X::Native(|args| native_fold(args, X::zero(), X::add)),
    );
    map.insert(
        "*".to_string(),
        X::Native(|args| native_fold(args, X::one(), X::mul)),
    );
    map.insert(
        "-".to_string(),
        X::Native(|args| native_unifold(args, X::zero(), X::sub)),
    );
    map.insert(
        "/".to_string(),
        X::Native(|args| native_unifold(args, X::one(), X::div)),
    );

    let env = Rc::new(RefCell::new(Environment { map, parent: None }));

    env.borrow_mut().map.insert(
        "newline".to_string(),
        X::Procedure(Procedure {
            name: Some("newline".into()),
            body: Box::new(X::List(vec![
                X::Symbol("display".into()),
                X::String("\n".into()),
            ])),
            params: vec![],
        }),
    );

    env
}

pub struct Environment {
    map: HashMap<Symbol, Expression>,
    parent: Option<EnvRef>,
}

impl Environment {
    pub fn new(parent: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Environment {
            map: Default::default(),
            parent: Some(parent),
        }))
    }

    pub fn lookup(&self, key: &str) -> Option<Expression> {
        self.map
            .get(key)
            .cloned()
            .or_else(|| self.parent.clone().and_then(|p| p.borrow().lookup(key)))
    }

    pub fn insert(&mut self, key: String, expr: Expression) {
        self.map.insert(key, expr);
    }

    pub fn set_vars(&mut self, names: &[Expression], args: Vec<Expression>) -> Result<()> {
        if names.len() < args.len() {
            return Err(ErrorKind::ArgumentError.into());
        }

        for (n, a) in names.iter().zip(args) {
            self.map.insert(n.try_as_symbol()?.clone(), a);
        }

        Ok(())
    }
}

/// apply a bivariate function to all arguments in sequence
fn native_fold<F: Fn(Expression, Expression) -> Result<Expression>>(
    args: Args,
    mut acc: Expression,
    func: F,
) -> Result<Expression> {
    for b in args {
        acc = func(acc, b)?;
    }
    Ok(acc)
}

/// apply a bivariate function to all arguments in sequence, but handle a single argument as
/// special case. For example: (- 5 2) -> 3  but (- 5) -> -5
fn native_unifold<F: Fn(Expression, Expression) -> Result<Expression>>(
    args: Args,
    mut acc: Expression,
    func: F,
) -> Result<Expression> {
    let mut args = args.into_iter();

    let first = args.next().ok_or(ErrorKind::ArgumentError)?;

    match args.next() {
        None => return func(acc, first),
        Some(second) => acc = func(first, second)?,
    }

    for b in args {
        acc = func(acc, b)?;
    }
    Ok(acc)
}

fn native_display(args: Args) -> Result<Expression> {
    print!(
        "{}",
        args.into_iter().next().ok_or(ErrorKind::ArgumentError)?
    );
    Ok(Expression::Undefined)
}
