// Usage:
// Get node.js and npm from somewhere, then run:
//   npm install deep-equal
//   time node --stack-size=400000000 search.js

const equal = require('deep-equal')

function lazyf(susp) {
    let value;
    return function(u) {
        if (value === undefined)
            value = susp()
        return value(u)
    }
}

function merge(n, x, y) {
    return u => u < n ? x(u) : y(u)
}

function lift(f, xx, yy, q) {
    function yx(x) {
        return lazyf(() => yy(y => q(merge(f, x, y)))) }
    let x = lazyf(() => xx(x => q(merge(f, x, yx(x)))))
    let y = lazyf(() => yy(y => q(merge(f, x, y))))
    return merge(f, x, y)
}

function range(m, p, q) {
    if (m + 1 >= p) {
        let qt = q(() => true)
        return () => qt
    }
    // The difference between m & p is always a power of two, so n is an
    // integer.
    let n = (m + p) / 2
    return lift(n, qq => range(m, n, qq), qq => range(n, p, qq), q)
}

function after(m, q) {
    let n = 2 * m + 1
    return lift(n, qq => range(m, n, qq), qq => after(n, qq), q)
}

function limit(f) {
    let m = 0
    let n = 1
    while (f(n)) {
        m = n
        n = 2 * m + 1
    }
    while (n - m > 1) {
        // Again, we use power-of-two differences, so this is an integer.
        p = (m + n) / 2
        if (f(p))
            m = p
        else
            n = p
    }
    return m
}

function arbitrary(n) { return (n & 1) != 0 }

function raw(p) {
    let p_arbitrary = p(arbitrary);
    let different = after(0, f => p(f) != p_arbitrary);
    if (p(different) == p_arbitrary)
        return p_arbitrary
    let pivot = limit(n => p(merge(n, arbitrary, different)) != p_arbitrary)
    let rawT = raw(f => p(x => x == pivot || f(x)))
    let rawF = raw(f => p(x => x != pivot && f(x)))
    return [pivot, rawT, rawF]
}

function print(s) {
    process.stdout.write(s)
}

function cook(t) {
    if (t === true)
        return print('T')
    if (t === false)
        return print('F')
    let [pp, x, y] = t
    if (typeof(x) == 'object' && typeof(y) == 'object') {
        print('IF ' + pp + ' (')
        cook(x)
        print(',')
        cook(y)
        print(')')
        return
    }
    if (typeof(y) == 'object') {
        // x is boolean.
        if (x)
            print(pp + '| ')
        else
            print('!' + pp + '& ')
        return cook(y)
    }
    if (typeof(x) == 'object') {
        // y is boolean.
        if (y)
            print('!' + pp + '| ')
        else
            print(pp + '& ')
        return cook(x)
    }
    // Both boolean...
    if (x) {
        if (y)
            print('IF ' + pp + ' T T')
        else
            print(pp.toString())
    }
    else {
        if (y)
            print('!' + pp)
        else
            print('IF ' + pp + ' F F')
    }
}

const GOLDEN = 0.61803398875

function weights(w, acc, r) {
    if (typeof(r) != 'object')
        return
    let [n, x, y] = r
    if (n in acc)
        acc[n] += w
    else
        acc[n] = w
    w *= GOLDEN
    weights(w, acc, x)
    weights(w, acc, y)
}


function cond(n, x, y) {
    if (equal(x, y))
        return x
    return [n, x, y]
}


function split(p, v, r) {
    if (typeof(r) != 'object')
        return r
    let [n, x, y] = r
    if (n == p)
        return v ? x : y
    return cond(n, split(p, v, x), split(p, v, y))
}


function optimize(r) {
    if (typeof(r) != 'object')
        return r

    let acc = {}
    weights(1.0, acc, r)
    let p
    let weight = 0
    for (let [n,w] of Object.entries(acc))
        if (w > weight) {
            p = n
            weight = w
        }
    return cond(p, optimize(split(p, true, r)), optimize(split(p, false, r)))
}

function martin(p) {
    function narrow(x,y) { return p(x * 111111111111111) ? y : 0 }
    let n = narrow(1, 1) + narrow(2, 2) + narrow(3, 4) + narrow(4, 8)
    return p(n) != p(n+1)
}

cook(optimize(optimize(raw(martin))))
print('\n')
