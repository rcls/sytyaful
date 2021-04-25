#!/usr/bin/python3

import resource
import sys
resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))
sys.setrecursionlimit(1000000)

def Const(x):
    return lambda y, x=x: x

def Merge(n,x,y):
    return lambda u, n=n, x=x, y=y: x(u) if u < n else y(u)

class SearchYforFXY:
    __slots__ = 'f', 'x', 'yy', 'q', 'y'
    def __init__(self, f, x, yy, q):
        self.f  = f
        self.x  = x
        self.yy = yy
        self.q  = q
        self.y  = None
    def __call__(self, u):
        if self.y is None:
            self.y = self.yy(
                lambda y, q=self.q, f=self.f, x=self.x: q(
                    Merge(f, x, y)))
        return self.y(u)

class SearchXforFXY:
    __slots__ = 'f', 'xx', 'yy', 'q', 'x'
    def __init__(self, f, xx, yy, q):
        self.f  = f
        self.xx = xx
        self.yy = yy
        self.q  = q
        self.x  = None
    def __call__(self, u):
        if self.x is None:
            self.x = self.xx(
                lambda x, f=self.f, yy=self.yy, q=self.q: q(
                    Merge(f, x, SearchYforFXY(f, x, yy, q))))
        return self.x(u)

def lift(f, xx, yy, q):
    x = SearchXforFXY(f, xx, yy, q)
    y = SearchYforFXY(f, x,  yy, q)
    return Merge(f, x, y)

def between(m, p, q):
    if m + 1 == p:
        return Const(q(Const(True)))
    n = (m + p) // 2
    return lift(n,
                lambda q, m=m, n=n: between(m, n, q),
                lambda q, n=n, p=p: between(n, p, q),
                q)

def after(m, q):
    n = 2 * m + 1
    return lift(n,
                lambda q, m=m, n=n: between(m, n, q),
                lambda q,      n=n: after(   n, q),
                q)

def limit(f):
    m = 0
    n = 1
    while f(n):
        m = n
        n = 2 * m + 1
    while n - m > 1:
        p = (m + n) // 2
        if f(p):
            m = p
        else:
            n = p
    return m

def raw(p):
    def arbitrary(n):
        return (n & 1) != 0
    p_arbitrary = p(arbitrary)
    different = after(0, lambda f, p=p: p(f) != p_arbitrary)
    if p(different) == p_arbitrary:
        return not not p_arbitrary
    def partial(n):
        return p(Merge(n, arbitrary, different)) != p_arbitrary
    pivot = limit(partial)
    rawT = raw(lambda f: p(lambda x: x == pivot or  f(x)))
    rawF = raw(lambda f: p(lambda x: x != pivot and f(x)))
    return pivot, rawT, rawF

def cook(t):
    if type(t) is bool:
        return t
    pp, x, y = t
    if x is True:
        if y is False:
            return f'@{pp}'
        return f'@{pp}|', cook(y)
    if x is False:
        if y is True:
            return f'!{pp}'
        return f'!{pp}&', cook(y)
    if y is True:
        return f'!{pp}|', cook(x)
    if y is False:
        return f'@{pp}&', cook(x)
    return pp, cook(x), cook(y)

golden = 0.61803398875

def weights(w, acc, r):
    if type(r) is bool:
        return
    pp, x, y = r
    acc[pp] = w + acc.get(pp, 0)
    w *= golden
    weights(w, acc, x)
    weights(w, acc, y)

def cond(n, x, y):
    if x == y:
        return x
    else:
        return n, x, y

def split(p, v, r):
    if type(r) is bool:
        return r
    n, x, y = r
    if n == p:
        return x if v else y
    return cond(n, split(p, v, x), split(p, v, y))

def optimize(r):
    if type(r) is bool:
        return r
    n, x, y = r
    if type(x) is bool or type(y) is bool:
        return r
    w = {}
    weights(1, w, r)
    p = max(w.items(), key=lambda x: x[1])[0]
    return cond(p, optimize(split(p, True, r)), optimize(split(p, False, r)))

def narrow(p, x, y):
    if p(111111111111111 * x):
        return y
    else:
        return 0

def martin(p):
    n = narrow(p, 1, 1) + narrow(p, 2, 2) + narrow(p, 3, 4) + narrow(p, 4, 8)
    return p(n) != p(n+1)


if __name__ == "__main__":
    print(cook(optimize(optimize(raw(martin)))))
