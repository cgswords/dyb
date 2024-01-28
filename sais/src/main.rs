use std::collections::BTreeMap;

use ast::{Exp, Macro};
use exp_builders::*;
use expand::Expander;

use crate::ast::{Fun, ToDoc};

mod ast;
mod exp_builders;
mod expand;

fn main() {
    let exp_tests = vec![
        test00_exp(),
        test01_exp(),
        test02_exp(),
        test03_exp(),
        test04_exp(),
        test05_exp(),
        test06_exp(),
    ];

    for test in exp_tests {
        run_exp_test(new_expander(), test);
    }

    let fun_tests = vec![test00_fun(), test01_fun()];

    for test in fun_tests {
        run_fun_test(new_expander(), test);
    }
}

// ******************************************* //
// MACRO DEFNS                                 //
// ******************************************* //

fn new_expander() -> Expander {
    let defns = BTreeMap::from([
        (ident("or2"), make_or2()),
        (ident("add3"), make_add_three()),
        (ident("for_each"), make_for_each()),
        (ident("for_each_inner"), make_for_each_inner()),
    ]);
    Expander::new(defns)
}

fn make_or2() -> Macro {
    let e1 = ident("e1");
    let e2 = ident("e2");
    let t1 = ident("t1");

    let t1_exp = ident_exp(t1.clone());
    let test = if_(t1_exp.clone(), t1_exp.clone(), ident_exp(e2.clone()));
    let body = let_in(t1.clone(), ident_exp(e1.clone()), test);
    Macro {
        args: vec![e1, e2],
        body,
    }
}

fn make_add_three() -> Macro {
    let x = ident("x");
    let y = ident("y");
    let z = ident("z");

    let xe = ident_exp(x.clone());
    let ye = ident_exp(y.clone());
    let ze = ident_exp(z.clone());

    let body = let_in(
        x.clone(),
        plus(xe.clone(), ye.clone()),
        let_in(y.clone(), plus(xe.clone(), ze), ye),
    );
    Macro {
        args: vec![x, y, z],
        body,
    }
}

fn make_for_each() -> Macro {
    let vs = ident("vs");
    let bname = ident("body");
    let v = ident("v");

    let vse = ident_exp(vs.clone());
    let ve = ident_exp(v.clone());

    let pop_loop = loop_(if_(
        call(ident("not_empty"), vec![vse.clone()]),
        let_in(
            v.clone(),
            call(ident("vec_pop"), vec![vse.clone()]),
            call(bname.clone(), vec![ve.clone()]),
        ),
        block(vec![]),
    ));

    let body = let_in(vs.clone(), vse, pop_loop);
    Macro {
        args: vec![vs, bname.clone()],
        body,
    }
}

fn make_for_each_inner() -> Macro {
    let vs = ident("vs");
    let bname = ident("body");
    let v = ident("v");

    let vse = ident_exp(vs.clone());
    let ve = ident_exp(v.clone());

    let pop_loop = loop_(if_(
        call(ident("not_empty"), vec![vse.clone()]),
        let_in(
            v.clone(),
            call(ident("vec_pop"), vec![vse.clone()]),
            macro_call(
                ident("for_each"),
                vec![
                    ve.clone(),
                    lambda(vec![v.clone()], call(bname.clone(), vec![ve.clone()])),
                ],
            ),
        ),
        block(vec![]),
    ));

    let body = let_in(vs.clone(), vse, pop_loop);
    Macro {
        args: vec![vs, bname.clone()],
        body,
    }
}

// ******************************************* //
// EXP TESTS                                   //
// ******************************************* //

fn run_exp_test(mut expander: Expander, exp: Exp) {
    println!(
        "-----------------------------------------------------------------------------------------"
    );
    println!("pre_expand:\n{}", exp.to_doc_string());
    println!("--------------------------------");
    let exp_exp = expander.expand(exp);
    println!("post_expand:\n{}", exp_exp.to_doc_string());
}

fn test00_exp() -> Exp {
    let t1 = ident("t1");
    let t1_exp = ident_exp(t1.clone());
    let_in(
        ident("t1"),
        lit(10),
        macro_call(ident("or2"), vec![t1_exp.clone(), t1_exp.clone()]),
    )
}

fn test01_exp() -> Exp {
    let x = ident("x");
    let y = ident("y");
    let z = ident("z");

    let xe = ident_exp(x.clone());
    let ye = ident_exp(y.clone());
    let ze = ident_exp(z.clone());

    let_in(
        x.clone(),
        lit(1),
        let_in(
            y,
            lit(2),
            let_in(z, lit(3), macro_call(ident("add3"), vec![xe, ye, ze])),
        ),
    )
}

fn test02_exp() -> Exp {
    let x = ident("x");
    let y = ident("y");
    let z = ident("z");

    let xe = ident_exp(x.clone());
    let ye = ident_exp(y.clone());
    let ze = ident_exp(z.clone());

    let_in(
        x.clone(),
        lit(1),
        let_in(
            y,
            lit(2),
            let_in(
                z,
                lit(3),
                macro_call(
                    ident("add3"),
                    vec![
                        xe,
                        macro_call(ident("or2"), vec![ye.clone(), ze.clone()]),
                        macro_call(ident("add3"), vec![ye.clone(), ye, ze]),
                    ],
                ),
            ),
        ),
    )
}

fn test03_exp() -> Exp {
    let x = ident("x");

    let xe = ident_exp(x.clone());

    let_in(
        x.clone(),
        lit(1),
        loop_(if_(
            xe.clone(),
            break_(),
            block(vec![assign(
                x.clone(),
                macro_call(ident("add3"), vec![xe.clone(), xe.clone(), xe]),
            )]),
        )),
    )
}

fn test04_exp() -> Exp {
    let x = ident("x");
    let y = ident("y");
    let vs = ident("vs");

    let xe = ident_exp(x.clone());
    let ye = ident_exp(y.clone());
    let vse = ident_exp(vs.clone());

    let add_body = lambda(
        vec![y],
        block(vec![assign(x.clone(), plus(xe.clone(), ye.clone()))]),
    );

    let_in(
        x.clone(),
        lit(0),
        let_in(
            vs.clone(),
            call(ident("make_vec"), vec![lit(1), lit(2)]),
            macro_call(ident("for_each"), vec![vse, add_body]),
        ),
    )
}

fn test05_exp() -> Exp {
    let x = ident("x");
    let y = ident("y");
    let vs = ident("vs");

    let xe = ident_exp(x.clone());
    let ye = ident_exp(y.clone());
    let vse = ident_exp(vs.clone());

    let add_body = lambda(
        vec![y.clone()],
        block(vec![assign(x.clone(), plus(xe.clone(), ye.clone()))]),
    );

    let outer_body = lambda(
        vec![y],
        macro_call(ident("for_each"), vec![ye.clone(), add_body]),
    );

    let_in(
        x.clone(),
        lit(0),
        let_in(
            vs.clone(),
            call(
                ident("make_vec"),
                vec![call(ident("make_vec"), vec![lit(1), lit(2)])],
            ),
            macro_call(ident("for_each"), vec![vse, outer_body]),
        ),
    )
}

fn test06_exp() -> Exp {
    let x = ident("x");
    let y = ident("y");
    let vs = ident("vs");

    let xe = ident_exp(x.clone());
    let ye = ident_exp(y.clone());
    let vse = ident_exp(vs.clone());

    let add_body = lambda(
        vec![y.clone()],
        block(vec![assign(x.clone(), plus(xe.clone(), ye.clone()))]),
    );

    // Note: this one uses `vse` a second time instead of `y`, and should thus
    // refer to that outer variable.
    let outer_body = lambda(
        vec![y],
        macro_call(ident("for_each"), vec![vse.clone(), add_body]),
    );

    let_in(
        x.clone(),
        lit(0),
        let_in(
            vs.clone(),
            call(
                ident("make_vec"),
                vec![call(ident("make_vec"), vec![lit(1), lit(2)])],
            ),
            macro_call(ident("for_each"), vec![vse, outer_body]),
        ),
    )
}

// ******************************************* //
// FUN TESTS                                   //
// ******************************************* //

fn run_fun_test(mut expander: Expander, fun: Fun) {
    println!(
        "-----------------------------------------------------------------------------------------"
    );
    println!("pre_expand:\n{}", fun.to_doc_string());
    println!("--------------------------------");
    let exp_fun = expander.expand_fun(fun);
    println!("post_expand:\n{}", exp_fun.to_doc_string());
}

fn test00_fun() -> Fun {
    let x = ident("x");
    let y = ident("y");
    let vs = ident("vs");

    let xe = ident_exp(x.clone());
    let ye = ident_exp(y.clone());
    let vse = ident_exp(vs.clone());

    let add_body = lambda(
        vec![y.clone()],
        block(vec![assign(x.clone(), plus(xe.clone(), ye.clone()))]),
    );

    let outer_body = lambda(
        vec![y.clone()],
        macro_call(ident("for_each"), vec![ye.clone(), add_body]),
    );

    let body = let_in(
        x.clone(),
        plus(xe.clone(), ye.clone()),
        let_in(
            vs.clone(),
            call(
                ident("make_vec"),
                vec![call(ident("make_vec"), vec![lit(1), lit(2)])],
            ),
            macro_call(ident("for_each"), vec![vse, outer_body]),
        ),
    );
    fun(ident("foo"), vec![x.clone(), y.clone()], body)
}

fn test01_fun() -> Fun {
    let x = ident("x");
    let y = ident("y");
    let vs = ident("vs");

    let xe = ident_exp(x.clone());
    let ye = ident_exp(y.clone());
    let vse = ident_exp(vs.clone());

    let add_body = lambda(
        vec![y.clone()],
        block(vec![assign(x.clone(), plus(xe.clone(), ye.clone()))]),
    );

    let body = let_in(
        x.clone(),
        lit(0),
        let_in(
            vs.clone(),
            call(
                ident("make_vec"),
                vec![call(ident("make_vec"), vec![lit(1), lit(2)])],
            ),
            macro_call(ident("for_each_inner"), vec![vse, add_body]),
        ),
    );
    fun(ident("foo"), vec![x.clone(), y.clone()], body)
}
