#![feature(once_cell)]
#![feature(box_patterns)]

use std::lazy::Lazy;
use std::boxed::Box;
use std::rc::Rc;
use std::collections::HashMap;

type Pred = Rc<dyn Fn(u64) -> bool>;
type Goal = Rc<dyn Fn(Pred) -> bool>;
type Opt  = Rc<dyn Fn(Goal) -> Pred>;

fn lazy_opt<F: Fn(Goal) -> Pred + Copy + 'static>(f : F) -> Opt {
    Rc::new(move |q| {
        let xx = Lazy::new(move || f(q));
        Rc::new(move |x| xx(x))
    })
}

fn cc(q: Goal, xx: Opt, f: Rc<dyn Fn(Pred) -> Pred>) -> Pred {
    f.clone()(xx(Rc::new (move |x| q(f(x)))))
}

fn merge(n: u64, x: Pred, y: Pred) -> Pred {
    Rc::new(move |u| { if u < n { x(u) } else { y(u) } })
}

fn join(n: u64, xx: Opt, yy: Opt, q: Goal) -> Pred {
    cc(q.clone(), xx,
       Rc::new(move |x| {
           cc(q.clone(), yy.clone(),
              Rc::new(move |y| merge(n, x.clone(), y)))
       }))
}

fn leaf(q: Goal) -> Pred {
    let ll = Lazy::new(move || q(Rc::new(|_| true)));
    Rc::new(move |_| *ll)
}

fn range(m: u64, c: u64, q: Goal) -> Pred {
    if c == 1 {
        return leaf(q)
    }

    let b = c / 2;
    join(m+b,
         lazy_opt(move |q| range(m, b, q)),
         lazy_opt(move |q| range(m+b, b, q)),
         q)
}

fn after(b: u64, q: Goal) -> Pred {
    // b is always one less than a power of 2.
    join(2 * b + 1,
         lazy_opt(move |q| range(b, b+1, q)),
         lazy_opt(move |q| after(2 * b + 1, q)),
         q)
}

fn limit(f: &dyn Fn(u64) -> bool) -> u64 {
    let mut m = 0;
    let mut n = 1;
    while f(n) {
        m = n;
        n = 2 * m + 1;
    }
    while n - m > 1 {
        let p = (m + n) / 2;
        if f(p) { m = p } else { n = p }
    }
    m
}

#[derive(Clone, PartialEq, Eq)]
enum Raw { TT, FF, CC(u64, Box<Raw>, Box<Raw>) }
use Raw::*;

fn raw(p: Goal) -> Raw {
    let arbitrary = Rc::new(|n| n & 1 != 0);
    let p_arbitrary = p(arbitrary.clone());
    let q = p.clone();
    let different = after(0, Rc::new(move |f| q(f) != p_arbitrary));
    if p(different.clone()) == p_arbitrary {
        return if p_arbitrary { TT } else { FF }
    }
    let pivot = limit(&|n| p(merge(n, arbitrary.clone(), different.clone())) != p_arbitrary);
    let q = p.clone();
    let rawt = raw(Rc::new(move |f| q(Rc::new(move |x| x == pivot || f(x)))));
    let rawf = raw(Rc::new(move |f| p(Rc::new(move |x| x != pivot && f(x)))));
    CC(pivot, Box::new(rawt), Box::new(rawf))
}

// Just print it.
fn cook(t: Raw) {
    match t {
        TT => print!("T"),
        FF => print!("F"),
        CC(n, box tt, box ff) => match (tt, ff) {
            (TT, FF) => print!("{}", n),
            (FF, TT) => print!("!{}", n),
            (TT, ff) => {
                print!("{}| ", n);
                cook(ff);
            }
            (FF, ff) => {
                print!("!{}& ", n);
                cook(ff);
            }
            (tt, TT) => {
                print!("!{}| ", n);
                cook(tt);
            }
            (tt, FF) => {
                print!("{}& ", n);
                cook(tt);
            }
            (tt, ff) => {
                print!("IF {} (", n);
                cook(tt);
                print!(",");
                cook(ff);
                print!(")");
            }
        }
    }
}

const GOLDEN: f64 = 0.61803398875;

fn weights(w: f64, acc: &mut HashMap<u64, f64>, r: &Raw) {
    match r {
        CC(n, tt, ff) => {
            *acc.entry(*n).or_insert(0.0) += w;
            let w = w * GOLDEN;
            weights(w, acc, &*tt);
            weights(w, acc, &*ff);
        }
        _ => { }
    }
}

fn split(p: u64, v: bool, r: &Raw) -> Raw {
    match r {
        CC(n, tt, ff) => {
            if *n == p {
                return if v { *tt.clone() } else { *ff.clone() };
            };
            let tt = split(p, v, &*tt);
            let ff = split(p, v, &*ff);
            if tt == ff { tt } else { CC(*n, Box::new(tt), Box::new(ff)) }
        }
        _ => r.clone()
    }
}

fn optimize(r: Raw) -> Raw {
    let mut w = HashMap::<u64, f64>::new();
    weights(1.0, &mut w, &r);
    if w.len() <= 1 { return r }
    let (p, _) = w.drain().max_by(
        |(_,u), (_,v)| u.partial_cmp(v).unwrap()).unwrap();
    let stt = optimize(split(p, true, &r));
    let sff = optimize(split(p, false, &r));
    if stt == sff {
        stt
    }
    else {
        CC(p, Box::new(stt), Box::new(sff))
    }
}

fn ff(p : Pred) -> bool {
    let mut n = 0;
    if p(111111111111111) { n += 1 }
    if p(222222222222222) { n += 2 }
    if p(333333333333333) { n += 4 }
    if p(444444444444444) { n += 8 }
    return p(n) != p(n+1)
}

fn main() {
    let r = raw(Rc::new(ff));
    cook(optimize(optimize(r)));
    println!();
}
