use crate::ast::*;

pub fn call(fun: Ident, args: Vec<Exp>) -> Exp {
    Exp::Call(fun, args)
}

pub fn if_(test: Exp, conseq: Exp, alt: Exp) -> Exp {
    Exp::If(Box::new(test), Box::new(conseq), Box::new(alt))
}

pub fn lit(v: u64) -> Exp {
    Exp::Lit(v)
}

pub fn macro_call(fun: Ident, args: Vec<Exp>) -> Exp {
    Exp::MacroCall(fun, args)
}

pub fn plus(lhs: Exp, rhs: Exp) -> Exp {
    Exp::Binop(Box::new(lhs), Binop::Add, Box::new(rhs))
}

pub fn lambda(params: Vec<Ident>, body: Exp) -> Exp {
    Exp::Lambda(Lambda {
        params,
        body: Box::new(body),
    })
}

pub fn block(vec: Sequence) -> Exp {
    Exp::Block(vec)
}

pub fn loop_(body: Exp) -> Exp {
    Exp::Loop(Box::new(body))
}

pub fn break_() -> Exp {
    Exp::Break
}

pub fn let_(lhs: Ident, rhs: Exp) -> SequenceItem {
    SequenceItem::Let(lhs, Some(Box::new(rhs)))
}

pub fn let_in(lhs: Ident, rhs: Exp, body: Exp) -> Exp {
    match body {
        Exp::Block(mut s) => {
            let mut new_body = vec![let_(lhs, rhs)];
            new_body.append(&mut s);
            block(new_body)
        }
        body => block(vec![let_(lhs, rhs), seq(body)]),
    }
}

pub fn assign(lhs: Ident, rhs: Exp) -> SequenceItem {
    SequenceItem::Assign(lhs, Box::new(rhs))
}

#[allow(unused)]
pub fn assign_in(lhs: Ident, rhs: Exp, body: Exp) -> Exp {
    match body {
        Exp::Block(mut s) => {
            let mut new_body = vec![assign(lhs, rhs)];
            new_body.append(&mut s);
            block(new_body)
        }
        body => block(vec![assign(lhs, rhs), seq(body)]),
    }
}

pub fn seq(exp: Exp) -> SequenceItem {
    SequenceItem::Exp(Box::new(exp))
}

pub fn ident(name: &str) -> Ident {
    Ident::new(name)
}

pub fn ident_exp(ident: Ident) -> Exp {
    Exp::Ident(ident)
}

pub fn fun(name: Ident, params: Vec<Ident>, body: Exp) -> Fun {
    Fun { name, params, body }
}
