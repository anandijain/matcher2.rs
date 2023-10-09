use std::fmt;

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
}

/// for now we assume patterns aren't named, and only blank exists
fn is_match(ex: &Expr, pat: &Expr) -> bool {
    match (ex, pat) {
        (_, Expr::Sym(_)) => return ex == pat,
        (_, Expr::List(ps)) => {
            let p_head = head(pat);
            if p_head == sym("blank") {
                if ps.len() == 2 {
                    if ps[1] == head(ex) {
                        return true;
                    } else {
                        return false;
                    }
                }
                return true;
            }
            // this handles the ex sym case becase otherwise we should have gone to the pat sym arm above
            if ex.length() != pat.length() {
                return false;
            }
            match ex {
                Expr::List(es) => {
                    for (e, p) in es.iter().zip(ps.iter()) {
                        if !is_match(e, p) {
                            return false;
                        }
                    }
                    return true;
                }
                _ => unreachable!(),
            }
        }
    }
}

fn main() {
    // let tups = vec![
    //     ("f", "(blank)", true),
    //     ("f", "(blank Sym)", true),
    //     ("(f)", "(blank)", true),
    //     ("(f)", "(blank f)", true),
    //     ("(f a)", "(blank)", true),
    //     ("(f a)", "((blank) (blank))", true),
    //     ("(f a b)", "((blank) (blank))", false),
    // ];
    // for (ex, pat) in tups {
    //     println!("{} | {}", ex, pat);
    //     println!("{}", is_match(&parse(ex), &parse(pat)));
    // }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_match() {
        let test_cases = vec![
            ("f", "(blank)", true),
            ("f", "(blank Sym)", true),
            ("(f)", "(blank)", true),
            ("(f)", "(blank f)", true),
            ("(f a)", "(blank)", true),
            ("(f a)", "((blank) (blank))", true),
            ("(f a b)", "((blank) (blank))", false),
        ];

        for (ex, pat, expected) in test_cases {
            assert_eq!(is_match(&parse(ex), &parse(pat)), expected, "Failed on example: {} | {}", ex, pat);
        }
    }
}
