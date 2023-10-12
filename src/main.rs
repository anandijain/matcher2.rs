use std::{
    collections::HashMap,
    f32::consts::E,
    fmt,
    ops::{Deref, DerefMut},
};

peg::parser! {
    grammar expr_parser() for str {
        rule whitespace() = [' ' | '\t' | '\n' | '\r']*

        rule symbol() -> Expr
            = s:$(['a'..='z' | 'A'..='Z' | '?' | '$'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_' ]* ) { Expr::Sym(s.into()) }

        rule list() -> Expr
            = "(" l:Expr() ** whitespace() ")" { Expr::List(l) }

        pub rule Expr() -> Expr
            = symbol() / list()
    }
}

fn parse(s: &str) -> Expr {
    expr_parser::Expr(s).unwrap()
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expr {
    Sym(String),
    List(Vec<Expr>),
}
impl Deref for Expr {
    type Target = Vec<Expr>;

    fn deref(&self) -> &Self::Target {
        match self {
            Expr::List(vec) => vec,
            e => panic!("Can only deref Expr::List. ex:{}", e),
        }
    }
}

impl DerefMut for Expr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Expr::List(vec) => vec,
            _ => panic!("Can only deref Expr::List"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Sym(s) => write!(f, "{}", s),
            Expr::List(lst) => {
                let str_list: Vec<String> = lst.iter().map(|x| x.to_string()).collect();
                write!(f, "({})", str_list.join(" "))
            }
        }
    }
}

fn sym(name: &str) -> Expr {
    Expr::Sym(name.to_string())
}

fn list(exprs: Vec<Expr>) -> Expr {
    if exprs.is_empty() {
        panic!("Attempted to create a headless list");
    }
    Expr::List(exprs)
}

fn head(expr: &Expr) -> Expr {
    match expr {
        Expr::Sym(_) => sym("Sym"),
        Expr::List(list) => list[0].clone(),
    }
}

impl Expr {
    /// matches `Length` in WL
    pub fn length(&self) -> usize {
        match self {
            Expr::Sym(_) => 0,
            Expr::List(lst) => lst.len().saturating_sub(1),
        }
    }
    fn as_list(&self) -> Option<&Vec<Expr>> {
        if let Expr::List(lst) = self {
            Some(lst)
        } else {
            None
        }
    }
}

pub fn rebuild_all(expr: Expr, map: &HashMap<Expr, Expr>) -> Expr {
    // First, check if the entire expression exists in the map and replace it if it does
    if let Some(replacement) = map.get(&expr) {
        return replacement.clone();
    }

    // If the expression is not in the map, proceed with the recursion
    match expr {
        Expr::Sym(_) => expr,
        Expr::List(list) => {
            // Recursively rebuild all sub-expressions in the list
            let new_list: Vec<Expr> = list.into_iter().map(|e| rebuild_all(e, map)).collect();
            Expr::List(new_list)
        }
    }
}

fn splice_sequences(expr: Expr) -> Expr {
    match expr {
        Expr::Sym(_) => expr,
        Expr::List(mut list) => {
            let mut i = 0;
            while i < list.len() {
                list[i] = splice_sequences(list[i].clone());
                i += 1;
            }

            let mut new_list = Vec::new();
            let mut i = 0;
            while i < list.len() {
                let item = list[i].clone();
                if let Expr::List(ref sublist) = item {
                    if let Some(Expr::Sym(head)) = sublist.first() {
                        if head == "sequence" {
                            new_list.extend_from_slice(&sublist[1..]);
                            i += 1;
                            continue;
                        }
                    }
                }
                new_list.push(item);
                i += 1;
            }
            Expr::List(new_list)
        }
    }
}

/// we are assuming all patterns are named for the time being
/// and without heads so x_ and x__ not x_h
/// i think we need to change the signature to (bool, HashMap<Expr, Expr>)
/// where we return the new rules that were added to the map in case they need to be reverted up above
///
pub fn get_match(ex: &Expr, pat: &Expr, mut map: &mut HashMap<Expr, Expr>) -> bool {
    println!("get_match: {} | {} with map {:?}", ex, pat, map);
    match ex {
        Expr::Sym(e) => match pat {
            Expr::Sym(p) => return e == p,
            Expr::List(ps) => {
                let p_len = ps.len();
                let p_head = &ps[0];
                match p_head {
                    // (mq f (pattern x (blank)))
                    Expr::Sym(p) => {
                        if p == "pattern" {
                            let p_name = &ps[1];
                            // let p_type = &ps[2][0];

                            if let Some(from_map) = map.get(pat) {
                                return from_map == ex;
                            } else {
                                map.insert(pat.clone(), ex.clone());
                                return true;
                            }
                        } else {
                            // this would be like the case (mq f (f))
                            return false;
                        }
                    }

                    // (mq f ((pattern x (blank))))
                    Expr::List(phs) => {
                        // the important Sym List arm
                        // example case (mq f (f))
                        // if head(p_head) == sym("pattern") {
                        //     // example case (mq (f) ((pattern x (blank))))

                        //     // these are the generic "headless" pattern assertions
                        //     assert!(phs.len() == 3);
                        //     assert!(phs[2].length() == 0);
                        //     assert!(matches!(phs[1], Expr::Sym(_)));
                        //     assert!(matches!(phs[2], Expr::List(_)));

                        //     let p_name = &ps[1];

                        //     if let Some(from_map) = map.get(p_head) {
                        //         return from_map == ex;
                        //     } else {
                        //         map.insert(p_head.clone(), ex.clone());
                        //         return true;
                        //     }

                        // }
                        // todo!()
                        return false;
                    }
                }
            }
        },
        Expr::List(es) => {
            match pat {
                Expr::Sym(_) => return false,
                Expr::List(ps) => {
                    let p_head = &ps[0];
                    let p_rest = &ps[1..];
                    let e_rest = &es[1..];
                    match p_head {
                        // This is the (mq (f) (f)) (true)
                        Expr::Sym(_) => {
                            // this seems like the case where we are matching the whole list to a pattern
                            if p_head == &sym("pattern") {
                                // (mq (f a b) (pattern x (blank))) (true x-> (f a b))
                                // any pattern object that im planning to support should pass here
                                if let Some(from_map) = map.get(pat) {
                                    return from_map == ex;
                                } else {
                                    map.insert(pat.clone(), ex.clone());
                                    // println!("here i am ");

                                    return true;
                                }
                            } else {
                                // if !get_match(&head(ex), &head(pat), map) {
                                //     return false;
                                // }
                                if es[0] != *p_head {
                                    return false;
                                }
                            }
                        }
                        // This is the (mq () (())) (true)
                        Expr::List(head_ps) => {
                            // so the general strat from my notes is first we need to handle matching the heads
                            // because we are in the case where pat is List and the head of pat is list
                            // so that means that we need to make sure that the p_head can match no more than the head of the ex
                            // only then can we start to figure out how long the blank_sequences need to be

                            // the reason why i think i might need to conditionally accept the changes to the bindings map
                            // is that if we are in recursion and we return false, then i think we need to undo this up above somehow
                            // maybe this is only an issue when we are doing the blank_sequence stuff
                            // because there you actually do need to revert a bad guess
                            // or at least overwrite it

                            // pat is ((pattern x (blank))) and ex is (f a b)
                            // let mut tmp_map = map.clone();

                            if !get_match(&head(ex), &head(pat), &mut map) {
                                return false;
                            }
                            // else {
                            //     map = &mut tmp_map;
                            // }
                        }
                    }

                    // we've already made sure the head matches
                    // so ie we had (mq (f a b), (f (pattern x (blank_sequence))))
                    // at this point we've already checked that f<->f
                    // i = 0, p_i = (pattern x (blank_sequence))
                    for (i, p_i) in p_rest.iter().enumerate() {
                        let e_i = &e_rest[i];
                        if head(p_i) == sym("pattern") {
                            let p_type = &p_i.as_list().unwrap()[2].as_list().unwrap()[0];

                            if p_type == &sym("blank_sequence") {
                                // is i..e_rest.len() correct?
                                //
                                // j represents the number of exprs in e_rest that the blank_sequence matches
                                // blank_sequences matches a minimum of 1
                                for j in 1..e_rest.len() {
                                    // let e_j = &e_rest[j];
                                    // todo!
                                    // i think this means the sequence should be i..i+j
                                    let mut seq = parse("(sequence)");
                                    for k in i..=i + j {
                                        seq.push(e_rest[k].clone());
                                    }
                                    println!("seq: {}", seq);
                                    // requires lookahead to see if we need to splice a sequence in
                                    // i think that means it needs to be done at each lookup
                                    // into map.

                                    if let Some(from_map) = map.get(p_i) {
                                    } else {
                                        map.insert(p_i.clone(), seq.clone());
                                        // println!("here i am ");
                                    } // rebuild all uses the map to rebuild the pattern up to equality with no matching
                                    let mut rebuild_pat = rebuild_all(pat.clone(), map);
                                    rebuild_pat = splice_sequences(rebuild_pat);
                                    println!("rebuild_pat: {}", rebuild_pat);

                                    let m = get_match(ex, &rebuild_pat, map);
                                    if !m {
                                        map.remove(p_i);
                                    } else {
                                        return true;
                                    }
                                }
                            } else if p_type == &sym("blank") {
                                //     if let Some(from_map) = map.get(p_i) {
                                //     } else {
                                //         map.insert(p_i.clone(), e_i.clone());
                                //         // println!("here i am ");
                                //     } // rebuild all uses the map to rebuild the pattern up to equality with no matching
                                //     let rebuild_pat = rebuild_all(pat.clone(), map);

                                //     // in this case we're look at a non List<>Pat case meaning
                                //     // this blank is going to (possibly) match a single expr in the arguments to `ex``
                                // ex
                                let m = get_match(e_i, p_i, map);
                                println!("{}: {} | {} with map {:?}", i, e_i, p_i, map);
                                if !m {
                                    map.remove(p_i);
                                    return false;
                                }
                            }
                            // end pattern case
                        } else {
                            // we cant just take [i] becuase we need to know
                            // actually i think we can take [i] because we will have recursed in the build_sequence and rebuilt the
                            // ex
                            let m = get_match(e_i, p_i, map);
                            println!("{}: {} | {} with map {:?}", i, e_i, p_i, map);
                            if !m {
                                map.remove(p_i);
                                return false;
                            }
                        }
                    }
                    // return true;
                    let mut new_ex = rebuild_all(pat.clone(), map);
                    new_ex = splice_sequences(new_ex);

                    if new_ex == *ex {
                        return true;
                    } else {
                        return false;
                    }
                    // if ps[0]
                }
            }
        }
    }
}

fn main() {
    let expr = Expr::List(vec![
        Expr::Sym("f".to_string()),
        Expr::List(vec![
            Expr::Sym("g".to_string()),
            Expr::List(vec![
                Expr::Sym("sequence".to_string()),
                Expr::Sym("x".to_string()),
                Expr::Sym("y".to_string()),
            ]),
            Expr::Sym("z".to_string()),
        ]),
        Expr::List(vec![
            Expr::Sym("sequence".to_string()),
            Expr::Sym("a".to_string()),
            Expr::Sym("b".to_string()),
        ]),
        Expr::Sym("c".to_string()),
    ]);

    let new_expr = splice_sequences(expr);
    println!("{:?}", new_expr);

    let expr = Expr::List(vec![
        Expr::Sym("f".to_string()),
        Expr::List(vec![
            Expr::Sym("g".to_string()),
            Expr::List(vec![
                Expr::Sym("sequence".to_string()),
                Expr::Sym("x".to_string()),
                Expr::List(vec![
                    Expr::Sym("sequence".to_string()),
                    Expr::Sym("y1".to_string()),
                    Expr::Sym("y2".to_string()),
                ]),
                Expr::Sym("z".to_string()),
            ]),
        ]),
        Expr::Sym("h".to_string()),
    ]);

    let new_expr = splice_sequences(expr);
    println!("{:?}", new_expr);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_match() {
        let test_cases = vec![
            // ("f", "f", true),
            // ("f", "g", false),
            // ("f", "(pattern x (blank))", true),
            // ("(f)", "(f)", true),
            // ("(f)", "(pattern x (blank))", true),
            // ("(f)", "(g)", false),
            // ("((((((f))))))", "(pattern x (blank))", true),
            // ("(f a b)", "(pattern x (blank))", true),
            // ("(f a)", "(f (pattern x (blank)))", true),
            // ("(f a)", "(f (pattern x (blank)))", true),
            ("(f a b c)", "(f (pattern x (blank_sequence)))", true),
            ("(f a b (f a b))", "(f (pattern x (blank_sequence)) (f (pattern x (blank_sequence))))", true),
            ("(f a b c a b)", "(f (pattern x (blank_sequence)) (pattern y (blank)) (pattern x (blank_sequence)))", true),
            // ("(f a)", "(f (pattern x (blank)))", true),

            // ("f", "(blank)", true),
            // ("f", "(blank Sym)", true),
            // ("(f)", "(blank)", true),
            // ("(f)", "(f)", true),
            // ("(f)", "(blank f)", true),
            // ("(f a)", "(blank)", true),
            // ("(f a)", "((blank) (blank))", true),
            // ("(f a b)", "((blank) (blank))", false),
            // ("f", "(blank_sequence)", true),
            // ("(f a)", "(blank_sequence)", true),
            // ("(f a)", "(f (blank_sequence))", true),
            // ("(f a b c)", "(f (blank_sequence))", true),
        ];

        for (i, (ex, pat, expected)) in test_cases.iter().enumerate() {
            let mut map = HashMap::new();
            let m = get_match(&parse(ex), &parse(pat), &mut map);
            println!("{}: gives {} on {} | {} with map {:?}", i, m, ex, pat, map);
            assert_eq!(
                m, *expected,
                "Failed on example: {}: {} | {} with map {:?}",
                i, ex, pat, map
            );
        }
    }
}
