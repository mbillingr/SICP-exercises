use crate::environment::{EnvRef, EnvWeak, Environment};
use crate::errors::*;
use crate::macros::Macro;
use crate::symbol::{self, Symbol};
use crate::tracer::trace_procedure_call;

use std::hash::{Hash, Hasher};
pub use std::rc::{Rc as Ref, Weak};

pub type List = Expression;
pub type Args = Expression;
pub type NativeFn = fn(Args) -> Result<Expression>;
pub type NativeIntrusiveFn = fn(Args, &EnvRef) -> Result<Expression>;

#[derive(Clone)]
pub enum Expression {
    Undefined,
    Nil,
    Symbol(Symbol),
    String(Ref<String>),
    Char(char),
    Integer(i64),
    Float(f64),
    True,
    False,
    Pair(Ref<(Expression, Expression)>),
    Procedure(Procedure<EnvRef>),
    Macro(Macro),
    Native(NativeFn),
    NativeIntrusive(NativeIntrusiveFn),
    Error(Ref<List>),
}

impl Expression {
    pub fn zero() -> Self {
        Expression::Integer(0)
    }

    pub fn one() -> Self {
        Expression::Integer(1)
    }

    pub fn from_literal<T: AsRef<str> + ToString>(s: T) -> Self {
        match s.as_ref() {
            "#t" => return Expression::True,
            "#f" => return Expression::False,
            _ => {}
        }

        if let Ok(i) = s.as_ref().parse() {
            return Expression::Integer(i);
        }

        if let Ok(f) = s.as_ref().parse() {
            return Expression::Float(f);
        }

        Expression::Symbol(Symbol::new(s))
    }

    pub fn from_vec(l: Vec<Expression>) -> Self {
        let mut list = Expression::Nil;
        for x in l.into_iter().rev() {
            list = Expression::cons(x, list);
        }
        list
    }

    pub fn cons(car: impl Into<Expression>, cdr: impl Into<Expression>) -> Self {
        Expression::Pair(Ref::new((car.into(), cdr.into())))
    }

    pub fn decons(&self) -> Result<(&Expression, &Expression)> {
        match self {
            Expression::Pair(pair) => Ok((&pair.0, &pair.1)),
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
    }

    pub fn car(&self) -> Result<&Expression> {
        match self {
            Expression::Pair(pair) => Ok(&pair.0),
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
    }

    pub fn cdr(&self) -> Result<&Expression> {
        match self {
            Expression::Pair(pair) => Ok(&pair.1),
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
    }

    pub fn car_mut(&mut self) -> Result<&mut Expression> {
        match self {
            Expression::Pair(pair) => Ok(&mut Ref::get_mut(pair)
                .expect("mutable reference must be unique")
                .0),
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
    }

    pub fn cdr_mut(&mut self) -> Result<&mut Expression> {
        match self {
            Expression::Pair(pair) => Ok(&mut Ref::get_mut(pair)
                .expect("mutable reference must be unique")
                .1),
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
    }

    pub fn set_car(&self, x: Expression) -> Result<()> {
        match self {
            Expression::Pair(pair) => {
                // mutating shared data is unsafe in Rust but expected behavior in Scheme.
                unsafe {
                    let car = &pair.0 as *const _ as *mut Expression;
                    *car = x;
                }
            }
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
        Ok(())
    }

    pub fn set_cdr(&self, x: Expression) -> Result<()> {
        match self {
            Expression::Pair(pair) => {
                // mutating shared data is unsafe in Rust but expected behavior in Scheme.
                unsafe {
                    let cdr = &pair.1 as *const _ as *mut Expression;
                    *cdr = x;
                }
            }
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
        Ok(())
    }

    pub fn iter_list(&self) -> ListIterator {
        ListIterator::from_expression(self)
    }

    pub fn len(&self) -> Result<usize> {
        let mut n = 0;
        for x in self.iter_list() {
            x?;
            n += 1;
        }
        Ok(n)
    }

    pub fn append(&self, last: Expression) -> Result<Self> {
        let mut start = Expression::Nil;
        let mut current = &mut start;

        for x in self.iter_list() {
            *current = Expression::cons(x?.clone(), Expression::Nil);
            current = current.cdr_mut().unwrap();
        }

        *current = last;

        Ok(start)
    }

    pub fn map_list<F: FnMut(&Expression) -> Result<Expression>>(
        &self,
        mut func: F,
    ) -> Result<Expression> {
        let mut result = Expression::Nil;
        let mut in_cursor = self;
        let mut out_cursor = &mut result;

        loop {
            match in_cursor {
                Expression::Nil => break,
                Expression::Pair(pair) => {
                    let (car, cdr) = &**pair;
                    let x = func(&car)?;
                    in_cursor = &*cdr;

                    *out_cursor = Expression::cons(x, Expression::Nil);
                    out_cursor = out_cursor.cdr_mut().unwrap();
                }
                _ => return Err(ErrorKind::TypeError("not a list".into()))?,
            }
        }

        Ok(result)
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Expression::Nil => true,
            _ => false,
        }
    }

    pub fn is_true(&self) -> bool {
        match self {
            Expression::False => false,
            _ => true,
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Expression::Integer(_) => true,
            Expression::Float(_) => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Expression::Integer(_) => true,
            Expression::Float(f) if float_eq(*f, f.trunc()) => true,
            _ => false,
        }
    }

    pub fn is_exact(&self) -> bool {
        match self {
            Expression::Integer(_) => true,
            _ => false,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
            Expression::Symbol(_) => true,
            _ => false,
        }
    }

    pub fn is_named_symbol<T: AsRef<str>>(&self, name: T) -> bool {
        match self {
            Expression::Symbol(s) => s.name() == name.as_ref(),
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            Expression::Nil => true,
            Expression::Pair(pair) => pair.1.is_list(),
            _ => false,
        }
    }

    pub fn is_pair(&self) -> bool {
        match self {
            Expression::Pair(_) => true,
            _ => false,
        }
    }

    pub fn try_as_integer(&self) -> Result<i64> {
        match self {
            Expression::Integer(i) => Ok(*i),
            Expression::Float(f) if float_eq(*f, f.trunc()) => Ok(*f as i64),
            _ => Err(ErrorKind::TypeError(format!("{} is not an integer.", self)).into()),
        }
    }

    pub fn try_as_float(&self) -> Result<f64> {
        match self {
            Expression::Integer(i) => Ok(*i as f64),
            Expression::Float(f) => Ok(*f),
            _ => Err(ErrorKind::TypeError(format!("{} is not a number.", self)).into()),
        }
    }

    pub fn try_as_symbol(&self) -> Result<&Symbol> {
        match self {
            Expression::Symbol(s) => Ok(s),
            _ => Err(ErrorKind::TypeError(format!("{} is not a Symbol.", self)).into()),
        }
    }

    pub fn try_as_str(&self) -> Result<&str> {
        match self {
            Expression::String(s) => Ok(s),
            _ => Err(ErrorKind::TypeError(format!("{} is not a String.", self)).into()),
        }
    }

    pub fn try_into_symbol(self) -> Result<Symbol> {
        match self {
            Expression::Symbol(s) => Ok(s),
            _ => Err(ErrorKind::TypeError(format!("{} is not a Symbol.", self)).into()),
        }
    }

    pub fn try_to_vec(&self) -> Result<Vec<Expression>> {
        self.iter_list().map(|r| r.map(Clone::clone)).collect()
    }

    pub fn logical_and(self, other: Self) -> Result<Self> {
        if self.is_true() {
            Ok(other)
        } else {
            Ok(self)
        }
    }

    pub fn logical_or(self, other: Self) -> Result<Self> {
        if self.is_true() {
            Ok(self)
        } else {
            Ok(other)
        }
    }

    pub fn min(self, other: Self) -> Result<Self> {
        Ok(if other < self { other } else { self })
    }

    pub fn max(self, other: Self) -> Result<Self> {
        Ok(if self < other { other } else { self })
    }

    pub fn eqv(&self, rhs: &Self) -> bool {
        use Expression::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => a == b,
            (Float(a), Float(b)) => float_eq(*a, *b),
            (String(a), String(b)) => Ref::ptr_eq(a, b),
            (Symbol(a), Symbol(b)) => a == b,
            (Char(a), Char(b)) => a == b,
            (True, True) => true,
            (False, False) => true,
            (Nil, Nil) => true,
            (Pair(a), Pair(b)) => Ref::ptr_eq(a, b),
            (Procedure(a), Procedure(b)) => a.eqv(b),
            (Native(a), Native(b)) => a == b,
            _ => false,
        }
    }

    pub fn equal(&self, rhs: &Self) -> bool {
        use Expression::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => a == b,
            (Float(a), Float(b)) => float_eq(*a, *b),
            (String(a), String(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (Char(a), Char(b)) => a == b,
            (True, True) => true,
            (False, False) => true,
            (Nil, Nil) => true,
            (Pair(a), Pair(b)) => a.0 == b.0 && a.1 == b.1,
            (Procedure(a), Procedure(b)) => a.equal(b),
            (Native(a), Native(b)) => a == b,
            _ => false,
        }
    }

    pub fn round(&self) -> Result<Self> {
        match self {
            Expression::Integer(i) => Ok(Expression::Integer(*i)),
            Expression::Float(f) => Ok(Expression::Integer(f.round() as i64)),
            _ => Err(ErrorKind::TypeError(format!("not a number: {}", self)).into()),
        }
    }

    pub fn truncate_quotient(&self, other: &Self) -> Result<Self> {
        use Expression::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a / b)),
            (Integer(a), Float(b)) => {
                if other.is_integer() {
                    Ok(Float(*a as f64 / b))
                } else {
                    Err(ErrorKind::TypeError(format!("not an integer: {}", b)).into())
                }
            }
            (Float(a), Integer(b)) => {
                if self.is_integer() {
                    Ok(Float(a / *b as f64))
                } else {
                    Err(ErrorKind::TypeError(format!("not an integer: {}", a)).into())
                }
            }
            (Float(a), Float(b)) if self.is_integer() && other.is_integer() => Ok(Float(a / b)),
            (a, b) => Err(ErrorKind::TypeError(format!("not integers: {}, {}", a, b)).into()),
        }
    }

    pub fn truncate_remainder(&self, other: &Self) -> Result<Self> {
        use Expression::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a % b)),
            (Integer(a), Float(b)) => {
                if other.is_integer() {
                    Ok(Float(*a as f64 % b))
                } else {
                    Err(ErrorKind::TypeError(format!("not an integer: {}", b)).into())
                }
            }
            (Float(a), Integer(b)) => {
                if self.is_integer() {
                    Ok(Float(a % *b as f64))
                } else {
                    Err(ErrorKind::TypeError(format!("not an integer: {}", a)).into())
                }
            }
            (Float(a), Float(b)) if self.is_integer() && other.is_integer() => Ok(Float(a % b)),
            (a, b) => Err(ErrorKind::TypeError(format!("not integers: {}, {}", a, b)).into()),
        }
    }

    pub fn short_repr(&self) -> String {
        match self {
            Expression::Undefined => "#<unspecified>".into(),
            Expression::Nil => "'()".into(),
            Expression::Symbol(s) => format!("{}", s),
            Expression::String(s) => format!("{:?}", s),
            Expression::Integer(i) => format!("{}", i),
            Expression::Float(i) => format!("{}", i),
            Expression::Char(ch) => format!("{:?}", ch),
            Expression::True => "#t".into(),
            Expression::False => "#f".into(),
            Expression::Pair(pair) => {
                let (car, cdr) = &**pair;
                let mut s = format!("({}", car.short_repr());
                let mut cdr = cdr;
                loop {
                    match cdr {
                        Expression::Nil => break,
                        Expression::Pair(ref pair) => {
                            s += &format!(" {}", pair.0.short_repr());
                            cdr = &pair.1;
                        }
                        _ => {
                            s += &format!(" . {}", cdr.short_repr());
                            break;
                        }
                    }
                }
                s.push(')');
                s
            }
            Expression::Procedure(p) => format!("λ {}", p.sig_repr()),
            Expression::Macro(_) => "syntax".into(),
            Expression::Native(_) | Expression::NativeIntrusive(_) => "(primitive)".into(),
            Expression::Error(_) => "<ERROR>".into(),
        }
    }
}

// This function exists to make clippy stop complaining about exact floating point comparison.
// I think it accepts this because of the name...
#[inline(always)]
fn float_eq(a: f64, b: f64) -> bool {
    a == b
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::Undefined => write!(f, "#<unspecified>"),
            Expression::Nil => write!(f, "'()"),
            Expression::Symbol(s) => write!(f, "{:?}", s),
            Expression::String(s) => write!(f, "{:?}", s),
            Expression::Integer(i) => write!(f, "{}", i),
            Expression::Float(i) => write!(f, "{}", i),
            Expression::Char(ch) => write!(f, "{:?}", ch),
            Expression::True => write!(f, "#t"),
            Expression::False => write!(f, "#f"),
            Expression::Pair(pair) => {
                let car = &pair.0;
                let mut cdr = &pair.1;
                write!(f, "({:?}", car)?;
                loop {
                    match cdr {
                        Expression::Nil => break,
                        Expression::Pair(p) => {
                            write!(f, " {:?}", p.0)?;
                            cdr = &p.1;
                        }
                        _ => {
                            write!(f, " . {:?}", cdr)?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Expression::Procedure(p) => write!(f, "#<procedure {:p} {}>", p, p.params_ex()),
            Expression::Macro(m) => write!(f, "#<macro {}>", m.name()),
            Expression::Native(_) | Expression::NativeIntrusive(_) => write!(f, "<native>"),
            Expression::Error(l) => {
                let tmp: Vec<_> = l
                    .iter_list()
                    .map(|item| format!("{:?}", item.unwrap()))
                    .collect();
                write!(f, "ERROR: {}", tmp.join(" "))
            }
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::Undefined => write!(f, "#<unspecified>"),
            Expression::Nil => write!(f, "'()"),
            Expression::Symbol(s) => write!(f, "{}", s.name()),
            Expression::String(s) => write!(f, "{}", s),
            Expression::Integer(i) => write!(f, "{}", i),
            Expression::Float(i) => write!(f, "{}", i),
            Expression::Char(ch) => write!(f, "{}", ch),
            Expression::True => write!(f, "#t"),
            Expression::False => write!(f, "#f"),
            Expression::Pair(pair) => {
                let car = &pair.0;
                let mut cdr = &pair.1;
                write!(f, "({}", car)?;
                loop {
                    match cdr {
                        Expression::Nil => break,
                        Expression::Pair(p) => {
                            write!(f, " {}", p.0)?;
                            cdr = &p.1;
                        }
                        _ => {
                            write!(f, " . {}", cdr)?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Expression::Procedure(p) => write!(f, "#<procedure {:p} {}>", p, p.params_ex()),
            Expression::Macro(m) => write!(f, "#<macro {}>", m.name()),
            Expression::Native(_) | Expression::NativeIntrusive(_) => write!(f, "<native>"),
            Expression::Error(l) => {
                let tmp: Vec<_> = l
                    .iter_list()
                    .map(|item| format!("{}", item.unwrap()))
                    .collect();
                write!(f, "ERROR: {}", tmp.join(" "))
            }
        }
    }
}

impl From<&str> for Expression {
    fn from(s: &str) -> Self {
        Expression::String(Ref::new(s.into()))
    }
}

impl From<Symbol> for Expression {
    fn from(s: Symbol) -> Self {
        Expression::Symbol(s)
    }
}

impl From<String> for Expression {
    fn from(s: String) -> Self {
        Expression::String(Ref::new(s))
    }
}

impl From<i64> for Expression {
    fn from(i: i64) -> Self {
        Expression::Integer(i)
    }
}

impl From<f64> for Expression {
    fn from(f: f64) -> Self {
        Expression::Float(f)
    }
}

impl From<bool> for Expression {
    fn from(b: bool) -> Self {
        if b {
            Expression::True
        } else {
            Expression::False
        }
    }
}

impl std::ops::Add for Expression {
    type Output = Result<Expression>;
    fn add(self, other: Self) -> Self::Output {
        use Expression::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a + b)),
            (Integer(a), Float(b)) => Ok(Float(a as f64 + b)),
            (Float(a), Integer(b)) => Ok(Float(a + b as f64)),
            (Float(a), Float(b)) => Ok(Float(a + b)),
            (a, b) => Err(ErrorKind::TypeError(format!("Cannot add {} + {}", a, b)).into()),
        }
    }
}

impl std::ops::Mul for Expression {
    type Output = Result<Expression>;
    fn mul(self, other: Self) -> Self::Output {
        use Expression::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a * b)),
            (Integer(a), Float(b)) => Ok(Float(a as f64 * b)),
            (Float(a), Integer(b)) => Ok(Float(a * b as f64)),
            (Float(a), Float(b)) => Ok(Float(a * b)),
            (a, b) => Err(ErrorKind::TypeError(format!("Cannot multiply {} * {}", a, b)).into()),
        }
    }
}

impl std::ops::Sub for Expression {
    type Output = Result<Expression>;
    fn sub(self, other: Self) -> Self::Output {
        use Expression::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a - b)),
            (Integer(a), Float(b)) => Ok(Float(a as f64 - b)),
            (Float(a), Integer(b)) => Ok(Float(a - b as f64)),
            (Float(a), Float(b)) => Ok(Float(a - b)),
            (a, b) => Err(ErrorKind::TypeError(format!("Cannot subtract {} - {}", a, b)).into()),
        }
    }
}

impl std::ops::Div for Expression {
    type Output = Result<Expression>;
    fn div(self, other: Self) -> Self::Output {
        use Expression::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Float(a as f64 / b as f64)),
            (Integer(a), Float(b)) => Ok(Float(a as f64 / b)),
            (Float(a), Integer(b)) => Ok(Float(a / b as f64)),
            (Float(a), Float(b)) => Ok(Float(a / b)),
            (a, b) => Err(ErrorKind::TypeError(format!("Cannot divide {} / {}", a, b)).into()),
        }
    }
}

impl std::ops::Rem for Expression {
    type Output = Result<Expression>;
    fn rem(self, other: Self) -> Self::Output {
        use Expression::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a % b)),
            (Integer(a), Float(b)) => Ok(Float(a as f64 % b)),
            (Float(a), Integer(b)) => Ok(Float(a % b as f64)),
            (Float(a), Float(b)) => Ok(Float(a % b)),
            (a, b) => Err(ErrorKind::TypeError(format!("Cannot divide {} / {}", a, b)).into()),
        }
    }
}

impl std::cmp::PartialOrd for Expression {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        use Expression::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => a.partial_cmp(b),
            (Integer(a), Float(b)) => (*a as f64).partial_cmp(b),
            (Float(a), Integer(b)) => a.partial_cmp(&(*b as f64)),
            (Float(a), Float(b)) => a.partial_cmp(b),
            (String(a), String(b)) => a.partial_cmp(b),
            (Symbol(a), Symbol(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl std::cmp::PartialEq for Expression {
    fn eq(&self, rhs: &Self) -> bool {
        use Expression::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => a == b,
            (Integer(a), Float(b)) => (*a as f64) == *b,
            (Float(a), Integer(b)) => *a == (*b as f64),
            (Float(a), Float(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (Char(a), Char(b)) => a == b,
            (True, True) => true,
            (False, False) => true,
            (Nil, Nil) => true,
            (Pair(a), Pair(b)) => a.0 == b.0 && a.1 == b.1,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct Procedure<E> {
    body: Ref<Expression>,
    params: Ref<Expression>,
    env: E,
    name: Symbol,
}

impl<E: Default> Default for Procedure<E> {
    fn default() -> Self {
        Procedure {
            body: Ref::new(Expression::Nil),
            params: Ref::new(Expression::Nil),
            env: Default::default(),
            name: "()".into(),
        }
    }
}

impl<E> Procedure<E> {
    pub fn name(&self) -> Symbol {
        self.name
    }

    pub fn rename(mut self, name: Symbol) -> Self {
        self.name = name;
        self
    }

    pub fn env(&self) -> &E {
        &self.env
    }

    pub fn eqv(&self, other: &Self) -> bool {
        Ref::ptr_eq(&self.body, &other.body)
    }

    pub fn equal(&self, other: &Self) -> bool {
        self.body.equal(&other.body) && self.params.equal(&other.params)
    }

    pub fn sig_repr(&self) -> String {
        self.params.short_repr()
    }
}

impl Procedure<EnvRef> {
    pub fn new(params: Ref<Expression>, body: Ref<Expression>, env: EnvRef) -> Self {
        Procedure {
            body,
            params,
            env,
            name: symbol::GREEK_LAMBDA,
        }
    }

    pub fn build(signature: Expression, body: Expression, env: &EnvRef) -> Result<Self> {
        Ok(Procedure::new(
            Ref::new(signature),
            Ref::new(body),
            env.clone(),
        ))
    }

    pub fn new_local_env(&self, args: Expression) -> Result<EnvRef> {
        let mut env = Environment::new_child(self.env.clone(), self.clone());
        env.set_vars(self.params_ex().clone(), args)?;
        Ok(env.into())
    }

    pub fn body_ex(&self) -> &Expression {
        &self.body
    }

    pub fn params_ex(&self) -> &Expression {
        &self.params
    }

    pub fn notify_call(&self, called_env: &EnvRef, calling_env: &EnvRef) {
        trace_procedure_call(self, called_env, calling_env);
        /*print!("calling {} ", self.name);
        match calling_env {
            Some(pe) => match pe.borrow().current_procedure() {
                Some(proc) => println!("from {}", proc.name),
                None => println!("from the root"),
            }
            None => println!("from an unknown location"),
        }*/
    }
}

impl<T> Hash for Procedure<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let id = Ref::as_ref(&self.body) as *const _ as *const u8;
        id.hash(state);
    }
}

impl<T> Eq for Procedure<T> {}
impl<T> PartialEq for Procedure<T> {
    fn eq(&self, other: &Self) -> bool {
        self.eqv(other)
    }
}

impl From<Procedure<EnvRef>> for Procedure<EnvWeak> {
    fn from(proc: Procedure<EnvRef>) -> Self {
        Procedure {
            body: proc.body,
            params: proc.params,
            env: Ref::downgrade(&proc.env),
            name: proc.name,
        }
    }
}

impl From<Procedure<EnvWeak>> for Procedure<EnvRef> {
    fn from(proc: Procedure<EnvWeak>) -> Self {
        Procedure {
            env: proc
                .env
                .upgrade()
                .unwrap_or_else(|| panic!("procedure's environment has been dropped")),
            body: proc.body,
            params: proc.params,
            name: proc.name,
        }
    }
}

impl From<Procedure<EnvWeak>> for Expression {
    fn from(proc: Procedure<EnvWeak>) -> Self {
        Expression::Procedure(proc.into())
    }
}

impl<T> std::fmt::Debug for Procedure<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", Expression::cons(self.name, (*self.params).clone()))
    }
}

pub struct ListIterator<'a> {
    next_pair: &'a Expression,
}

impl<'a> ListIterator<'a> {
    pub fn from_expression(expr: &'a Expression) -> Self {
        ListIterator { next_pair: expr }
    }

    pub fn next_expr(&mut self) -> Result<Option<&'a Expression>> {
        self.next().transpose()
    }

    pub fn tail(&self) -> Result<&'a Expression> {
        match self.next_pair {
            Expression::Nil => Ok(self.next_pair),
            Expression::Pair(pair) => Ok(&pair.1),
            _ => Err(ErrorKind::TypeError("not a list".into()))?,
        }
    }
}

impl<'a> Iterator for ListIterator<'a> {
    type Item = Result<&'a Expression>;
    fn next(&mut self) -> Option<Self::Item> {
        let (car, cdr) = match self.next_pair {
            Expression::Nil => return None,
            Expression::Pair(pair) => (&pair.0, &pair.1),
            _ => return Some(Err(ErrorKind::TypeError("not a list".into()).into())),
        };

        self.next_pair = cdr;
        Some(Ok(car))
    }
}
