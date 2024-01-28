use std::collections::BTreeMap;

use crate::ast::{
    Exp, Fun, Ident, Lambda, Macro, Mark, OutExp, OutFun, OutSequenceItem, SequenceItem, ToDoc, Var,
};

const PRINT_MACRO_EXPANSIONS: bool = false;
const PRINT_ALL_EXPANSIONS: bool = false;

pub struct Expander {
    defns: BTreeMap<Var, Macro>,
    next_mark: Mark,
    next_id: u64,
}

impl Expander {
    pub fn new(mut defns: BTreeMap<Ident, Macro>) -> Expander {
        let mut expander = Expander {
            defns: BTreeMap::new(),
            next_mark: 0,
            next_id: 0,
        };
        expander.defns = defns.into_iter().map(|(k, v)| (k.resolve(), v)).collect();
        expander
    }

    fn fresh_mark(&mut self) -> Mark {
        self.next_mark = self.next_mark + 1;
        self.next_mark
    }

    fn fresh_var(&mut self, name: String) -> Var {
        self.next_id += 1;
        Var {
            name: format!("{}#{}", name, self.next_id),
        }
    }

    pub fn expand_fun(&mut self, f: Fun) -> OutFun {
        self.next_mark = 0;
        self.next_id = 0;
        let Fun { name, params, body } = f;
        let mut body = body;
        let params = params
            .into_iter()
            .map(|param| {
                let var = self.fresh_var(param.base_name());
                let body_taken = std::mem::replace(&mut body, Exp::Lit(0));
                body = body_taken.subst(&param, &var);
                var
            })
            .collect::<Vec<_>>();
        let body = self.expand(body);
        OutFun {
            name: name.resolve(),
            params,
            body,
        }
    }

    pub fn expand(&mut self, e: Exp) -> OutExp {
        if PRINT_ALL_EXPANSIONS { println!("expanding: {}", e.to_doc_string()); }
        use OutExp as OE;
        let result = match e {
            Exp::Ident(x) => OE::Var(x.resolve()),
            Exp::Lit(l) => OE::Lit(l),
            Exp::Call(fun, args) => OutExp::Call(
                fun.resolve(),
                args.into_iter().map(|arg| self.expand(arg)).collect(),
            ),
            Exp::MacroCall(fun, args) => {
                let macro_body = self.defns.get(&fun.resolve()).unwrap();
                let exp = self.apply_macro(macro_body.clone(), args);
                if PRINT_MACRO_EXPANSIONS { println!("expanded: {}", exp.to_doc_string()); }
                self.expand(exp)
            }
            Exp::Block(entries) => OE::Block(self.sexpand(entries)),
            Exp::Binop(lhs, op, rhs) => OE::Binop(self.bexpand(lhs), op, self.bexpand(rhs)),
            Exp::If(test, conseq, alt) => {
                OE::If(self.bexpand(test), self.bexpand(conseq), self.bexpand(alt))
            }
            Exp::Lambda(lambda) => {
                panic!()
            }
            Exp::Loop(body) => OutExp::Loop(self.bexpand(body)),
            Exp::Break => OutExp::Break,
        };
        result
    }

    fn bexpand(&mut self, be: Box<Exp>) -> Box<OutExp> {
        Box::new(self.expand(*be))
    }

    fn sexpand(&mut self, vs: Vec<SequenceItem>) -> Vec<OutSequenceItem> {
        fn apply_substs(mut s: SequenceItem, substs: &BTreeMap<Ident, Var>) -> SequenceItem {
            for (ident, var) in substs {
                s = s.subst(ident, var);
            }
            s
        }
        let mut substs = BTreeMap::new();
        vs.into_iter()
            .map(|s| {
                let s = apply_substs(s, &substs);
                match s {
                    // This is the key tool for expansion to work: whenever we finding bindings, we
                    // give them a unique binding name and propagate that name. This propagation
                    // respects marks, however, so things with the same name but different marks
                    // won't be renamed.
                    SequenceItem::Let(x, rhs) => {
                        let rhs = rhs.map(|rhs| self.bexpand(rhs));
                        let new_x = self.fresh_var(x.base_name());
                        substs.insert(x, new_x.clone());
                        OutSequenceItem::Let(new_x, rhs)
                    }
                    SequenceItem::Assign(x, rhs) => {
                        OutSequenceItem::Assign(x.resolve(), self.bexpand(rhs))
                    }
                    SequenceItem::Exp(e) => OutSequenceItem::Exp(self.bexpand(e)),
                }
            })
            .collect()
    }

    fn apply_macro(&mut self, macro_defn: Macro, args: Vec<Exp>) -> Exp {
        let mark_id = self.fresh_mark();

        // Do input marking
        let args = mark_all(args, mark_id);

        // Apply transform
        let Macro {
            args: binding_args,
            body,
        } = macro_defn;
        let arg_bindings = binding_args
            .into_iter()
            .zip(args)
            .map(|(x, e)| (x, (e)))
            .collect::<BTreeMap<_, _>>();
        let result = self.apply_transform(body, &arg_bindings);

        // Do output marking
        result.mark(mark_id)
    }

    fn apply_lambda(&mut self, lambda_defn: Lambda, args: Vec<Exp>) -> Exp {
        let mark_id = self.fresh_mark();

        // Do input marking
        let args = mark_all(args, mark_id);

        // Apply transform
        let Lambda { params, body } = lambda_defn;
        let arg_bindings = params
            .into_iter()
            .zip(args)
            .map(|(x, e)| (x, (e)))
            .collect::<BTreeMap<_, _>>();
        let result = self.apply_transform(*body, &arg_bindings);

        // Do output marking
        result.mark(mark_id)
    }

    // Our transformers get to cheat quite a bit: we don't need a "syntax" form! All we need to do
    // is push substitutions inward, and take care to respect `let`.
    fn apply_transform(&mut self, exp: Exp, bindings: &BTreeMap<Ident, Exp>) -> Exp {
        match exp {
            Exp::Ident(i) => {
                if let Some(exp) = bindings.get(&i) {
                    exp.clone()
                } else {
                    Exp::Ident(i)
                }
            }
            l @ Exp::Lit(_) => l,
            // We expand lambdas immediately. This is because, if we didn't, we would need to 
            // add a new expansion form `ApplyLamdda` or similar 
            Exp::Call(i, args) => {
                let args = args
                    .into_iter()
                    .map(|e| self.apply_transform(e, bindings))
                    .collect();
                if let Some(exp) = bindings.get(&i) {
                    match exp {
                        Exp::Lambda(lambda) => {
                            let lambda_exp = self.apply_lambda(lambda.clone(), args);
                            self.apply_transform(lambda_exp, bindings)
                        }
                        _ => panic!("misapplied: expected lambda, got {:#?} for {:#?}", exp, i),
                    }
                } else {
                    Exp::Call(i, args)
                }
            }
            Exp::MacroCall(fun, args) => {
                let args = args
                    .into_iter()
                    .map(|e| self.apply_transform(e, bindings))
                    .collect();
                // We leave the `fun` alone to be handled by expansion
                Exp::MacroCall(fun, args)
            }
            Exp::Block(mut seq) => {
                let mut bindings = bindings.clone();
                for item in seq.iter_mut() {
                    match item {
                        SequenceItem::Let(lhs, exp) => {
                            exp.as_mut().map(|e| {
                                *e = Box::new(self.apply_transform(*e.clone(), &bindings))
                            });
                            bindings.remove(lhs);
                        }
                        SequenceItem::Assign(_, exp) => {
                            *exp = Box::new(self.apply_transform(*exp.clone(), &bindings));
                        }
                        SequenceItem::Exp(e) => {
                            *e = Box::new(self.apply_transform(*e.clone(), &bindings))
                        }
                    }
                }
                Exp::Block(seq)
            }
            Exp::Binop(lhs, op, rhs) => {
                Exp::Binop(self.bat(lhs, bindings), op, self.bat(rhs, bindings))
            }
            Exp::If(test, conseq, alt) => Exp::If(
                self.bat(test, bindings),
                self.bat(conseq, bindings),
                self.bat(alt, bindings),
            ),
            // We don't have to consider arguments because unique naming already happened.
            Exp::Lambda(lambda) => {
                let Lambda { params, body } = lambda;
                let mut bindings = bindings.clone();
                for param in &params {
                    bindings.remove(&param);
                }
                Exp::Lambda(Lambda {
                    params,
                    body: self.bat(body, &bindings),
                })
            }
            Exp::Break => Exp::Break,
            Exp::Loop(body) => Exp::Loop(self.bat(body, bindings)),
        }
    }

    // box_apply_transformer
    fn bat(&mut self, bexp: Box<Exp>, bindings: &BTreeMap<Ident, Exp>) -> Box<Exp> {
        Box::new(self.apply_transform(*bexp, bindings))
    }
}

// ******************************************* //
// SYNTAX TRAIT AND IMPLS                      //
// ******************************************* //

trait Syntax {
    fn mark(self, mark_id: Mark) -> Self;
    fn subst(self, ident: &Ident, var: &Var) -> Self;
}

impl Syntax for Ident {
    fn mark(self, mark_id: Mark) -> Ident {
        let mut result = self;
        result.apply_mark(mark_id);
        result
    }

    fn subst(self, i: &Ident, x: &Var) -> Ident {
        let mut result = self;
        result.maybe_subst(i, x.clone());
        result
    }
}

impl Syntax for SequenceItem {
    fn mark(self, mark_id: Mark) -> SequenceItem {
        use SequenceItem::*;
        match self {
            Let(x, rhs) => Let(x.mark(mark_id), rhs.map(|rhs| rhs.mark(mark_id))),
            Assign(x, rhs) => Assign(x.mark(mark_id), rhs.mark(mark_id)),
            Exp(e) => Exp(e.mark(mark_id)),
        }
    }

    fn subst(self, ident: &Ident, var: &Var) -> SequenceItem {
        use SequenceItem::*;
        match self {
            Let(x, rhs) => Let(x.subst(ident, var), rhs.map(|rhs| rhs.subst(ident, var))),
            Assign(x, rhs) => Assign(x.subst(ident, var), rhs.subst(ident, var)),
            Exp(e) => Exp(e.subst(ident, var)),
        }
    }
}

impl Syntax for Lambda {
    fn mark(self, mark_id: Mark) -> Lambda {
        let Lambda { params, body } = self;
        Lambda {
            params: mark_all(params, mark_id),
            body: body.mark(mark_id),
        }
    }

    fn subst(self, ident: &Ident, var: &Var) -> Lambda {
        let Lambda { params, body } = self;
        Lambda {
            params: subst_all(params, ident, var),
            body: body.subst(ident, var),
        }
    }
}

impl Syntax for Exp {
    fn mark(self, mark_id: Mark) -> Exp {
        match self {
            Exp::Ident(i) => Exp::Ident(i.mark(mark_id)),
            l @ Exp::Lit(_) => l,
            Exp::Call(fun, args) => Exp::Call(fun.mark(mark_id), mark_all(args, mark_id)),
            // NB: we don't have to mark MacroCall because it can't be hidden by imports.
            Exp::MacroCall(fun, args) => Exp::MacroCall(fun.mark(mark_id), mark_all(args, mark_id)),
            Exp::Block(seq) => Exp::Block(mark_all(seq, mark_id)),
            Exp::Binop(lhs, op, rhs) => Exp::Binop(lhs.mark(mark_id), op, rhs.mark(mark_id)),
            Exp::If(test, conseq, alt) => {
                Exp::If(test.mark(mark_id), conseq.mark(mark_id), alt.mark(mark_id))
            }
            Exp::Lambda(lambda) => Exp::Lambda(lambda.mark(mark_id)),
            Exp::Break => Exp::Break,
            Exp::Loop(body) => Exp::Loop(body.mark(mark_id)),
        }
    }

    fn subst(self, ident: &Ident, var: &Var) -> Exp {
        match self {
            Exp::Ident(i) => Exp::Ident(i.subst(ident, var)),
            l @ Exp::Lit(_) => l,
            Exp::Call(fun, args) => Exp::Call(fun.subst(ident, var), subst_all(args, ident, var)),
            Exp::MacroCall(fun, args) => Exp::MacroCall(fun.subst(ident, var), subst_all(args, ident, var)),
            Exp::Block(seq) => Exp::Block(subst_all(seq, ident, var)),
            Exp::Binop(lhs, op, rhs) => {
                Exp::Binop(lhs.subst(ident, var), op, rhs.subst(ident, var))
            }
            Exp::If(test, conseq, alt) => Exp::If(
                test.subst(ident, var),
                conseq.subst(ident, var),
                alt.subst(ident, var),
            ),
            Exp::Lambda(lambda) => Exp::Lambda(lambda.subst(ident, var)),
            Exp::Break => Exp::Break,
            Exp::Loop(body) => Exp::Loop(body.subst(ident, var)),
        }
    }
}

impl Syntax for Box<Exp> {
    fn mark(self, mark_id: Mark) -> Box<Exp> {
        let e = *self;
        Box::new(e.mark(mark_id))
    }

    fn subst(self, ident: &Ident, var: &Var) -> Self {
        let e = *self;
        Box::new(e.subst(ident, var))
    }
}

fn mark_all<T: Syntax>(vec: Vec<T>, mark_id: Mark) -> Vec<T> {
    vec.into_iter().map(|e| e.mark(mark_id)).collect()
}

fn subst_all<T: Syntax>(vec: Vec<T>, ident: &Ident, var: &Var) -> Vec<T> {
    vec.into_iter().map(|e| e.subst(ident, var)).collect()
}
