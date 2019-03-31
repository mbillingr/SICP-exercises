use crate::errors::*;
use crate::expression::{Expression, List};
use crate::lexer::Token;

pub struct Parser {
    list_stack: Vec<List>,
}

impl Parser {
    pub fn new() -> Self {
        Parser { list_stack: vec![] }
    }

    pub fn push_token(&mut self, token: Token) -> Result<Option<Expression>> {
        self.parse_expression(token)
            .and_then(|o| o.map(transform).transpose())
    }

    fn parse_expression(&mut self, token: Token) -> Result<Option<Expression>> {
        let expr = match token {
            Token::String(s) => Expression::String(s),
            Token::Symbol(s) => s.into(),
            Token::ListOpen => {
                self.list_stack.push(List::new());
                return Ok(None);
            }
            Token::ListClose => match self.list_stack.pop() {
                Some(list) => Expression::List(list),
                None => return Err(ErrorKind::UnexpectedToken(token.into()))?,
            },
        };

        match self.list_stack.last_mut() {
            Some(list) => {
                list.push(expr);
                Ok(None)
            }
            None => Ok(Some(expr)),
        }
    }
}

// convert some syntactic forms, expand macros(?), check errors, ... (mostly to-do)
fn transform(expr: Expression) -> Result<Expression> {
    use Expression::*;
    match expr {
        List(l) => match l.first() {
            Some(Symbol(s)) if s == "define" => transform_define(l),
            Some(Symbol(s)) if s == "if" => transform_if(l),
            _ => l
                .into_iter()
                .map(transform)
                .collect::<Result<_>>()
                .map(List),
        },
        _ => Ok(expr),
    }
}

fn transform_define(list: List) -> Result<Expression> {
    if list.len() < 3 {
        return Err(ErrorKind::ArgumentError.into());
    }

    let mut list: Vec<_> = list.into_iter().map(transform).collect::<Result<_>>()?;

    if list[1].is_symbol() {
        Ok(Expression::List(list))
    } else if list[1].is_list() {
        let mut new_body = vec![];
        while list.len() > 2 {
            new_body.push(list.pop().unwrap());
        }
        new_body.push(Expression::Symbol("begin".to_string()));
        new_body.reverse();
        list.push(Expression::List(new_body));
        Ok(Expression::List(list))
    } else {
        Err(ErrorKind::TypeError(format!("Cannot use {} as signature.", list[1])).into())
    }
}

fn transform_if(list: List) -> Result<Expression> {
    if list.len() < 3 || list.len() > 4 {
        return Err(ErrorKind::ArgumentError.into());
    }

    let mut list = list
        .into_iter()
        .map(transform)
        .collect::<Result<Vec<_>>>()?;

    if list.len() == 3 {
        list.push(Expression::Undefined);
    }

    Ok(Expression::List(list))
}
