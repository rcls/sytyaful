#![feature(once_cell)]
#![feature(box_patterns)]
#![feature(trait_alias)]

use std::lazy::Lazy;
use std::boxed::Box;
use std::rc::Rc;
use std::collections::HashMap;

// This could just be a function trait, except that we want to impl. it
// ourselves.
trait PredT {
    fn at(&self, x: u64) -> bool;
}

type Pred = Rc<dyn PredT>;
type Goal = Rc<dyn Fn(Pred) -> bool>;
trait Opt = Fn(Goal) -> Pred + Copy + 'static;

fn rc<T>(x: T) -> Rc<T> { Rc::new(x) }

impl PredT for Pred {
    fn at(&self, x: u64) -> bool { (**self).at(x) }
}
impl<T: Fn(u64) -> bool> PredT for T {
    fn at(&self, x: u64) -> bool { self(x) }
}
impl<F: FnOnce() -> Pred> PredT for Lazy<Pred, F> {
    fn at(&self, x: u64) -> bool { Lazy::force(self).at(x) }
}

fn merge(n: u64, x: impl PredT + 'static, y: impl PredT + 'static) -> Pred {
    rc(move |u| if u < n { x.at(u) } else { y.at(u) })
}

fn konst(b : bool) -> Pred { rc(move |_| b) }

fn search_y_for_fxy(f: u64, x: Pred, yy: impl Opt, q: Goal) -> impl PredT {
    Lazy::new(
        move || yy(rc(move |y| q(merge(f.clone(), x.clone(), y)))))
}

fn search_x_for_fxy(f: u64, xx: impl Opt, yy: impl Opt, q: Goal) -> Pred {
    rc(Lazy::new(move || xx(rc(
        move |x|
        q(merge(f, x.clone(),
                search_y_for_fxy(f, x.clone(), yy, q.clone())))))))
}

fn lift(f: u64, xx: impl Opt, yy: impl Opt, q: Goal) -> Pred {
    let x = search_x_for_fxy(f, xx, yy, q.clone());
    let y = search_y_for_fxy(f, x.clone(), yy, q);
    merge(f, x, y)
}

fn range(m: u64, p: u64, q: Goal) -> Pred {
    if m + 1 == p {
        return konst(q(konst(true)))
    }
    let n = (m + p) / 2;
    lift(n, move |q| range(m, n, q), move |q| range(n, p, q), q)
}

fn after(m: u64, q: Goal) -> Pred {
    let n = 2 * m + 1;
    lift(n, move |q| range(m, n, q), move |q| after(n, q), q)
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
enum Raw { T, F, C(u64, Box<Raw>, Box<Raw>) }
use Raw::*;

fn raw(p: Goal) -> Raw {
    let arbitrary: Pred = rc(|n| n & 1 != 0);
    let p_arbitrary = p(arbitrary.clone());
    let q = p.clone();
    let different = after(0, rc(move |f| q(f) != p_arbitrary));
    if p(different.clone()) == p_arbitrary {
        return if p_arbitrary { T } else { F }
    }
    let pivot = limit(
        &|n| p(merge(n, arbitrary.clone(), different.clone())) != p_arbitrary);
    let q = p.clone();
    let rawt = raw(rc(move |f| q(rc(move |x| x == pivot || f.at(x)))));
    let rawf = raw(rc(move |f| p(rc(move |x| x != pivot && f.at(x)))));
    C(pivot, Box::new(rawt), Box::new(rawf))
}

// Just print it.
fn cook(t: &Raw) {
    match t {
        T => print!("T"),
        F => print!("F"),
        C(n, box x, box y) => match (x, y) {
            (T, F) => print!( "{}", n),
            (F, T) => print!("!{}", n),
            (T, y) => { print!( "{}| ", n); cook(y) }
            (F, y) => { print!("!{}& ", n); cook(y) }
            (x, T) => { print!("!{}| ", n); cook(x) }
            (x, F) => { print!( "{}& ", n); cook(x) }
            (x, y) => {
                print!("IF {} (", n);
                cook(x);
                print!(",");
                cook(y);
                print!(")");
            }
        }
    }
}

const GOLDEN: f64 = 0.61803398875;

fn weights(w: f64, acc: &mut HashMap<u64, f64>, r: &Raw) {
    match r {
        C(n, x, y) => {
            *acc.entry(*n).or_insert(0.0) += w;
            let w = w * GOLDEN;
            weights(w, acc, &*x);
            weights(w, acc, &*y);
        }
        _ => { }
    }
}

fn cond(n: u64, x: Raw, y: Raw) -> Raw {
    if x == y {
        return x
    }
    C(n, Box::new(x), Box::new(y))
}

fn split(p: u64, v: bool, r: &Raw) -> Raw {
    match r {
        C(n, x, y) => {
            if *n == p {
                return if v { *x.clone() } else { *y.clone() };
            };
            cond(*n, split(p, v, &*x), split(p, v, &*y))
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
    cond(p, optimize(split(p, true, &r)), optimize(split(p, false, &r)))
}

fn martin(p : Pred) -> bool {
    let narrow = |x: u64, y: u64| if p.at(x * 111111111111111) { y } else { 0 };
    let n = narrow(1, 1) + narrow(2, 2) + narrow(3, 4) + narrow(4, 8);
    p.at(n) != p.at(n+1)
}

fn main() {
    cook(&optimize(optimize(raw(rc(martin)))));
    println!();
}
