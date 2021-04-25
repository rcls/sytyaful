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
    pp, tt, ff = t
    if tt is True:
        if ff is False:
            return f'@{pp}'
        return f'@{pp}|', cook(ff)
    if tt is False:
        if ff is True:
            return f'!{pp}'
        return f'!{pp}&', cook(ff)
    if ff is True:
        return f'!{pp}|', cook(tt)
    if ff is False:
        return f'@{pp}&', cook(tt)
    return pp, cook(tt), cook(ff)

golden = 0.61803398875

def weights(w, acc, r):
    if type(r) is bool:
        return
    pp, tt, ff = r
    acc[pp] = w + acc.get(pp, 0)
    w *= golden
    weights(w, acc, tt)
    weights(w, acc, ff)

def split(p, v, r):
    if type(r) is bool:
        return r
    pp, tt, ff = r
    if pp == p:
        return tt if v else ff
    tt = split(p, v, tt)
    ff = split(p, v, ff)
    if tt == ff:
        return tt
    else:
        return pp, tt, ff

def optimize(r):
    if type(r) is bool:
        return r
    pp, tt, ff = r
    if type(tt) is bool or type(ff) is bool:
        return r
    w = {}
    weights(1, w, r)
    p = max(w.items(), key=lambda x: x[1])[0]
    stt = optimize(split(p, True, r))
    sff = optimize(split(p, False, r))
    if stt == sff:
        return stt
    else:
        return p, stt, sff

def martin(p):
    n = 0
    if p(111111111111111):
        n += 1
    if p(222222222222222):
        n += 2
    if p(333333333333333):
        n += 4
    if p(444444444444444):
        n += 8
    return p(n) != p(n+1)


if __name__ == "__main__":
    r = raw(martin)
    print(cook(optimize(optimize(r))))
