use crate::environment::EnvWeak;
use crate::lexer::{Lexer, Token};
use rustyline::{self, completion::Completer, highlight::Highlighter, hint::Hinter, Helper};

pub struct EnvHelper(EnvWeak);

impl EnvHelper {
    pub fn new(env: EnvWeak) -> Self {
        EnvHelper(env)
    }
}

impl Helper for EnvHelper {}

impl Hinter for EnvHelper {
    fn hint(&self, _line: &str, _pos: usize) -> Option<String> {
        None
    }
}

impl Completer for EnvHelper {
    type Candidate = String;

    fn complete(&self, line: &str, pos: usize) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        if let Some(rc_env) = self.0.upgrade() {
            let mut lexer = Lexer::new();
            let tokens = match lexer.tokenize(line.to_string()) {
                Ok(t) => t,
                Err(_) => return Ok((0, vec![])),
            };

            let token = match tokens
                .iter()
                .find(|&pt| pt.start_idx <= pos && pt.end_idx == pos)
            {
                Some(t) => t,
                None => return Ok((0, vec![])),
            };

            let substr = if let Token::Symbol(s) = &token.token {
                &s[..pos - token.start_idx]
            } else {
                return Ok((0, vec![]));
            };

            let candidates = rc_env
                .borrow()
                .all_keys()
                .filter(|key| key.starts_with(substr))
                .collect();

            Ok((token.start_idx, candidates))
        } else {
            Ok((
                0,
                vec!["bla".to_string(), "foo".to_string(), "bar".to_string()],
            ))
        }
    }
}

impl Highlighter for EnvHelper {}