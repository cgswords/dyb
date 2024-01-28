use std::{
    collections::BTreeSet,
    fmt::{Debug, Display},
};

use pretty::{Doc, RcDoc};

// ******************************************* //
// INPUT LANGUAGE                              //
// ******************************************* //

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Var {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Macro {
    pub args: Vec<Ident>,
    pub body: Exp,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub params: Vec<Ident>,
    pub body: Box<Exp>,
}

#[derive(Debug, Clone)]
pub struct Fun {
    pub name: Ident,
    pub params: Vec<Ident>,
    pub body: Exp,
}

#[derive(Debug, Clone)]
pub enum Defn {
    Fn(Fun),
    Macro(Var, Macro),
}

#[derive(Debug, Clone)]
pub enum SequenceItem {
    Let(Ident, Option<Box<Exp>>),
    Assign(Ident, Box<Exp>),
    Exp(Box<Exp>),
}

pub type Sequence = Vec<SequenceItem>;

pub type Mark = u64;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident {
    symbolic_name: Var,
    binding_name: Var,
    marks: BTreeSet<Mark>,
}

#[derive(Debug, Clone)]
pub enum Binop {
    Add,
    Multiple,
    Subtract,
    Divide,
}

#[derive(Debug, Clone)]
pub enum Exp {
    Ident(Ident),
    Lit(u64),
    Call(Ident, Vec<Exp>),
    MacroCall(Ident, Vec<Exp>),
    Block(Sequence),
    Binop(Box<Exp>, Binop, Box<Exp>),
    If(Box<Exp>, Box<Exp>, Box<Exp>),
    Lambda(Lambda),
    Break,
    Loop(Box<Exp>),
}

// ******************************************* //
// OUTPUT LANGUAGE                             //
// ******************************************* //

#[derive(Debug, Clone)]
pub struct OutFun {
    pub name: Var,
    pub params: Vec<Var>,
    pub body: OutExp,
}

#[derive(Debug, Clone)]
pub enum OutSequenceItem {
    Let(Var, Option<Box<OutExp>>),
    Assign(Var, Box<OutExp>),
    Exp(Box<OutExp>),
}

pub type OutSequence = Vec<OutSequenceItem>;

#[derive(Debug, Clone)]
pub enum OutExp {
    Var(Var),
    Lit(u64),
    Call(Var, Vec<OutExp>),
    Block(OutSequence),
    Binop(Box<OutExp>, Binop, Box<OutExp>),
    If(Box<OutExp>, Box<OutExp>, Box<OutExp>),
    Break,
    Loop(Box<OutExp>),
}

// ******************************************* //
// IMPLS                                       //
// ******************************************* //

impl Ident {
    pub fn new(name: &str) -> Ident {
        let name = name.to_string();
        Ident {
            symbolic_name: Var { name: name.clone() },
            binding_name: Var { name },
            marks: BTreeSet::new(),
        }
    }

    pub fn resolve(self) -> Var {
        self.binding_name
    }

    pub fn marks_of(&self) -> BTreeSet<Mark> {
        self.marks.clone()
    }

    pub fn apply_mark(&mut self, mark_id: Mark) {
        if self.marks.contains(&mark_id) {
            self.marks.remove(&mark_id);
        } else {
            self.marks.insert(mark_id);
        }
    }

    pub fn maybe_subst(&mut self, ident: &Ident, var: Var) {
        if self.marks_of() == ident.marks_of() && self.binding_name == ident.binding_name {
            self.binding_name = var;
        }
    }

    pub fn base_name(&self) -> String {
        self.symbolic_name.name.clone()
    }
}

// ******************************************* //
// TRAIT IMPLS                                 //
// ******************************************* //

impl Debug for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<{},{},{:?}>",
            self.symbolic_name.name, self.binding_name.name, self.marks
        )
    }
}

// ******************************************* //
// "PRETTY" PRINDING                           //
// ******************************************* //

pub trait ToDoc {
    fn to_doc(&self) -> RcDoc<()>;
    fn to_doc_string(&self) -> String {
        let mut w = Vec::new();
        self.to_doc().render(80, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl ToDoc for Var {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text(format!("{}", self.name))
    }
}

impl ToDoc for Ident {
    fn to_doc(&self) -> RcDoc<()> {
        let sname = RcDoc::text(format!("{}", self.symbolic_name.name));
        let bname = RcDoc::text(format!("{}", self.binding_name.name));
        let marks = RcDoc::text(format!("{:?}", self.marks));
        RcDoc::text("<")
            .append(RcDoc::intersperse(
                vec![sname, bname, marks],
                RcDoc::text(","),
            ))
            .append(RcDoc::text(">"))
    }
}

impl ToDoc for Macro {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::group(doc_parens(comma_list(&self.args)))
            .append(RcDoc::line())
            .append(block(self.body.to_doc()))
    }
}

impl ToDoc for Fun {
    fn to_doc(&self) -> RcDoc<()> {
        let Fun { name, params, body } = self;
        RcDoc::group(
            RcDoc::text("fun")
                .append(RcDoc::space())
                .append(name.to_doc())
                .append(doc_parens(comma_list(params))),
        )
        .append(block(body.to_doc()))
    }
}

impl ToDoc for Lambda {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::group(doc_bars(comma_list(&self.params)))
            .append(RcDoc::line())
            .append(block(self.body.to_doc()))
    }
}

impl ToDoc for SequenceItem {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            SequenceItem::Let(x, Some(rhs)) => RcDoc::group(
                RcDoc::text("let ")
                    .append(x.to_doc())
                    .append(RcDoc::text(" =")),
            )
            .append(RcDoc::group(RcDoc::line().append(rhs.to_doc())).nest(4)),
            SequenceItem::Let(x, None) => RcDoc::group(RcDoc::text("let ").append(x.to_doc())),
            SequenceItem::Assign(x, rhs) => RcDoc::group(
                RcDoc::text("")
                    .append(x.to_doc())
                    .append(RcDoc::text(" <-")),
            )
            .append(RcDoc::group(RcDoc::line().append(rhs.to_doc())).nest(4)),
            SequenceItem::Exp(e) => e.to_doc(),
        }
    }
}

impl ToDoc for Exp {
    fn to_doc(&self) -> RcDoc<()> {
        let text = RcDoc::text;
        match self {
            Exp::Ident(ident) => ident.to_doc(),
            Exp::Lit(n) => text(n.to_string()),
            Exp::Call(fun, args) => RcDoc::group(
                fun.to_doc()
                    .append(RcDoc::text("("))
                    .append(comma_list(args)),
            )
            .append(")"),
            Exp::Call(fun, args) | Exp::MacroCall(fun, args) => RcDoc::group(
                RcDoc::group(fun.to_doc().append(RcDoc::text("!")))
                    .append(RcDoc::text("("))
                    .append(comma_list(args)),
            )
            .append(")"),
            Exp::Block(entries) => block(semicolon_list(entries)),
            Exp::Binop(lhs, op, rhs) => {
                RcDoc::group(lhs.to_doc().append(op.to_doc()).append(rhs.to_doc()))
            }
            Exp::If(test, conseq, alt) => RcDoc::group(RcDoc::text("if ").append(test.to_doc()))
                .append(RcDoc::space())
                .append(RcDoc::group(
                    block(conseq.to_doc())
                        .append(RcDoc::text(" else "))
                        .append(block(alt.to_doc())),
                )),
            Exp::Lambda(lambda) => lambda.to_doc(),
            Exp::Break => RcDoc::text("break"),
            Exp::Loop(body) => RcDoc::group(RcDoc::text("loop {"))
                .append(RcDoc::line().append(body.to_doc()).nest(4))
                .append(RcDoc::group(RcDoc::line().append(RcDoc::text("}")))),
        }
    }
}

impl ToDoc for Binop {
    fn to_doc(&self) -> RcDoc<()> {
        let op = match self {
            Binop::Add => "+",
            Binop::Multiple => "*",
            Binop::Subtract => "-",
            Binop::Divide => "/",
        };
        RcDoc::text(format!(" {} ", op))
    }
}

impl ToDoc for OutFun {
    fn to_doc(&self) -> RcDoc<()> {
        let OutFun { name, params, body } = self;
        RcDoc::group(
            RcDoc::text("fun")
                .append(RcDoc::space())
                .append(name.to_doc())
                .append(doc_parens(comma_list(params))),
        )
        .append(block(body.to_doc()))
    }
}

impl ToDoc for OutSequenceItem {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            OutSequenceItem::Let(x, Some(rhs)) => RcDoc::group(
                RcDoc::text("let ")
                    .append(x.to_doc())
                    .append(RcDoc::text(" =")),
            )
            .append(RcDoc::group(RcDoc::line().append(rhs.to_doc())).nest(4)),
            OutSequenceItem::Let(x, None) => RcDoc::group(RcDoc::text("let ").append(x.to_doc())),
            OutSequenceItem::Assign(x, rhs) => RcDoc::group(
                RcDoc::text("")
                    .append(x.to_doc())
                    .append(RcDoc::text(" <-")),
            )
            .append(RcDoc::group(RcDoc::line().append(rhs.to_doc())).nest(4)),
            OutSequenceItem::Exp(e) => e.to_doc(),
        }
    }
}

impl ToDoc for OutExp {
    fn to_doc(&self) -> RcDoc<()> {
        let text = RcDoc::text;
        match self {
            OutExp::Var(ident) => ident.to_doc(),
            OutExp::Lit(n) => text(n.to_string()),
            OutExp::Call(fun, args) => RcDoc::group(
                fun.to_doc()
                    .append(RcDoc::text("("))
                    .append(comma_list(args)),
            )
            .append(")"),
            OutExp::Block(entries) => block(semicolon_list(entries)),
            OutExp::Binop(lhs, op, rhs) => {
                RcDoc::group(lhs.to_doc().append(op.to_doc()).append(rhs.to_doc()))
            }
            OutExp::If(test, conseq, alt) => RcDoc::group(RcDoc::text("if ").append(test.to_doc()))
                .append(RcDoc::space())
                .append(RcDoc::group(
                    block(conseq.to_doc())
                        .append(RcDoc::text(" else "))
                        .append(block(alt.to_doc())),
                )),
            OutExp::Break => RcDoc::text("break"),
            OutExp::Loop(body) => RcDoc::group(RcDoc::text("loop {"))
                .append(RcDoc::line().append(body.to_doc()).nest(4))
                .append(RcDoc::group(RcDoc::line().append(RcDoc::text("}")))),
        }
    }
}

fn doc_parens(doc: RcDoc<()>) -> RcDoc<()> {
    RcDoc::group(RcDoc::text("(").append(doc).append(RcDoc::text(")")))
}

fn doc_bars(doc: RcDoc<()>) -> RcDoc<()> {
    RcDoc::group(RcDoc::text("|").append(doc).append(RcDoc::text("|")))
}

fn comma_list<T: ToDoc>(xs: &Vec<T>) -> RcDoc<()> {
    RcDoc::group(RcDoc::intersperse(
        xs.iter().map(|x| x.to_doc()),
        RcDoc::text(","),
    ))
}

fn semicolon_list<T: ToDoc>(xs: &Vec<T>) -> RcDoc<()> {
    RcDoc::group(RcDoc::intersperse(
        xs.iter().map(|x| x.to_doc()),
        RcDoc::text(";").append(RcDoc::line()),
    ))
}

fn block(inner: RcDoc<()>) -> RcDoc<()> {
    RcDoc::text("{").append(RcDoc::group(
        RcDoc::group(RcDoc::line().append(RcDoc::group(inner)))
            .nest(4)
            .append(RcDoc::line())
            .append(RcDoc::text("}")),
    ))
}
