use enumo::*;
use recipe_utils::run_workload;
use ruler::*;

egg::define_language! {
pub enum Math {
    "+" = Add([Id; 2]),
    "-" = Sub([Id; 2]),
    "*" = Mul([Id; 2]),
    "/" = Div([Id; 2]),
    Num(i32),
    Var(egg::Symbol),
}
}

impl SynthLanguage for Math {
    type Constant = i32;

    fn eval<'a, F>(&'a self, cvec_len: usize, mut get_cvec: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Math::Add([x, y]) => map!(get_cvec, x, y => x.checked_add(*y)),
            Math::Sub([x, y]) => map!(get_cvec, x, y => x.checked_sub(*y)),
            Math::Mul([x, y]) => map!(get_cvec, x, y => x.checked_mul(*y)),
            Math::Div([x, y]) => map!(get_cvec, x, y => x.checked_div(*y)),
            Math::Num(n) => vec![Some(*n); cvec_len],
            Math::Var(_) => vec![],
        }
    }

    fn mk_interval<'a, F>(&'a self, mut get_interval: F) -> Interval<Self::Constant>
    where
        F: FnMut(&'a Id) -> &'a Interval<Self::Constant>,
    {
        let mut get_const = |x: &'a Id| {
            let ival = get_interval(x);
            if ival.low == ival.high {
                ival.low.clone()
            } else {
                None
            }
        };
        match self {
            Math::Var(_) => None,
            Math::Num(n) => Some(n.clone()),
            Math::Add([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => x.checked_add(y),
                _ => None,
            },
            Math::Sub([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => x.checked_sub(y),
                _ => None,
            },
            Math::Mul([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => x.checked_mul(y),
                _ => None,
            },
            Math::Div([x, y]) => match (get_const(x), get_const(y)) {
                (Some(x), Some(y)) => x.checked_div(y),
                _ => None,
            },
        }
        .map(|c| Interval::new(Some(c.clone()), Some(c)))
        .unwrap_or_default()
    }

    fn initialize_vars(egraph: &mut EGraph<Self, SynthAnalysis>, vars: &[String]) {
        let consts: Vec<Option<i32>> = vec![
            Some(0),
            Some(1),
            Some(2),
            Some(3),
            Some(4),
            Some(5),
            Some(6),
            Some(7),
            Some(8),
            Some(9),
            Some(10),
        ];
        let cvecs = self_product(&consts, vars.len());

        egraph.analysis.cvec_len = cvecs[0].len();

        for (i, v) in vars.iter().enumerate() {
            let id = egraph.add(Math::Var(Symbol::from(v.clone())));
            let cvec = cvecs[i].clone();
            egraph[id].data.cvec = cvec;
        }
    }

    fn mk_var(sym: egg::Symbol) -> Self {
        Math::Var(sym)
    }

    fn to_var(&self) -> Option<Symbol> {
        match self {
            Math::Var(sym) => Some(*sym),
            _ => None,
        }
    }

    fn validate(_lhs: &egg::Pattern<Self>, _rhs: &egg::Pattern<Self>) -> ValidationResult {
        // just trust me.
        ValidationResult::Valid
    }

    fn mk_constant(c: Self::Constant, _egraph: &mut EGraph<Self, SynthAnalysis>) -> Self {
        Math::Num(c)
    }

    fn is_constant(&self) -> bool {
        matches!(self, Math::Num(_))
    }
}

#[test]
fn run_conditional_math_test() {
    let mut ruleset: Ruleset<Math> = Ruleset::default();

    let terms = Workload::new([
        "(+ expr expr)",
        "(- expr expr)",
        "(* expr expr)",
        "(/ expr expr)",
    ])
    .plug("expr", &Workload::new(["a", "b", "0", "1"]));

    let new_rules = run_workload(
        terms,
        ruleset.clone(),
        Limits::synthesis(),
        Limits::minimize(),
        false,
    );

    println!("new rules:");
    for rule in new_rules.iter() {
        println!("{:?} => {:?}", rule.lhs.pretty(80), rule.rhs.pretty(80));
    }
}
