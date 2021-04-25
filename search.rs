#![feature(once_cell)]
#![feature(box_patterns)]

use std::lazy::Lazy;
use std::boxed::Box;
use std::rc::Rc;
use std::collections::HashMap;

type Pred = Rc<dyn Fn(u64) -> bool>;
type Goal = Rc<dyn Fn(Pred) -> bool>;
type Opt  = Rc<dyn Fn(Goal) -> Pred>;

fn merge(n: u64, x: Pred, y: Pred) -> Pred {
    Rc::new(move |u| { if u < n { x(u) } else { y(u) } })
}

fn konst(b : bool) -> Pred {
    Rc::new(move |_| b)
}

fn search_y_for_fxy(f: u64, x: Pred, yy: Opt, q: Goal) -> Pred {
    let s = Lazy::new(
        move ||
            yy(Rc::new(move |y: Pred| q(merge(f.clone(), x.clone(), y)))));
    Rc::new(move |u| s(u))
}

fn search_x_for_fxy(f: u64, xx: Opt, yy: Opt, q: Goal) -> Pred {
    let s = Lazy::new(
        move ||
            xx(Rc::new(
                move |x|
                q(merge(f,
                        x.clone(),
                        search_y_for_fxy(f, x, yy.clone(), q.clone()))))));
    Rc::new(move |u| s(u))
}

fn lift(f: u64, xx: Opt, yy: Opt, q: Goal) -> Pred {
    let x = search_x_for_fxy(f, xx, yy.clone(), q.clone());
    let y = search_y_for_fxy(f, x.clone(), yy, q);
    merge(f, x, y)
}

fn range(m: u64, p: u64, q: Goal) -> Pred {
    if m + 1 == p {
        return konst(q(konst(true)))
    }
    let n = (m + p) / 2;
    lift(n,
         Rc::new(move |q| range(m, n, q)),
         Rc::new(move |q| range(n, p, q)),
         q)
}

fn after(m: u64, q: Goal) -> Pred {
    let n = 2 * m + 1;
    lift(n,
         Rc::new(move |q| range(m, n, q)),
         Rc::new(move |q| after(n, q)),
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
fn cook(t: &Raw) {
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

fn martin(p : Pred) -> bool {
    let mut n = 0;
    if p(111111111111111) { n += 1 }
    if p(222222222222222) { n += 2 }
    if p(333333333333333) { n += 4 }
    if p(444444444444444) { n += 8 }
    p(n) != p(n+1)
}

fn main() {
    let r = raw(Rc::new(martin));
    cook(&optimize(optimize(r)));
    println!();
}
