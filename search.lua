#!/usr/bin/lua

function Const(x) return function (y) return x end end

function Merge(n, x, y)
    return function (u) if u < n then return x(u) else return y(u) end end
end

function SearchYforFXY(n, x, yy, q)
    local y
    return function (u)
        if not y then
            y = yy(function (y) return q(Merge(n, x, y)) end)
        end
        return y(u)
    end
end

function SearchXforFXY(n, xx, yy, q)
    local x
    return function (u)
        if not x then
            x = xx(function (x)
                    return q(Merge(n, x, SearchYforFXY(n, x, yy, q))) end)
        end
        return x(u)
    end
end

function lift(n, xx, yy, q)
    local x = SearchXforFXY(n, xx, yy, q)
    local y = SearchYforFXY(n, x,  yy, q)
    return Merge(n, x, y)
end

function between(m, p, q)
    if m + 1 == p then
        return Const(q(Const(true)))
    end
    local n = (m + p) / 2
    return lift(n,
                function (q) return between(m, n, q) end,
                function (q) return between(n, p, q) end,
                q)
end

function after(m, q)
    local n = 2 * m + 1
    return lift(n,
                function (q) return between(m, n, q) end,
                function (q) return after(n, q) end,
                q)
end

function limit(f)
    local m = 0
    local n = 1
    while f(n) do
        m = n
        n = 2 * m + 1
    end
    while n - m > 1 do
        local p = (m + n) / 2
        if f(p) then m = p else n = p end
    end
    return m
end

function arbitrary(n) return (n % 2) ~= 0 end

function raw(p)
    local p_arbitrary = p(arbitrary)
    local different = after(0, function (f) return p(f) ~= p_arbitrary end)
    if p(different) == p_arbitrary then
        return not not p_arbitrary
    end
    local function partial(n)
        return p(Merge(n, arbitrary, different)) ~= p_arbitrary
    end
    local pivot = limit(partial)
    local function tt(f)
        return p(function (x) return x == pivot or  f(x) end)
    end
    local function ff(f)
        return p(function (x) return x ~= pivot and f(x) end)
    end
    return {pivot, raw(tt), raw(ff)}
end

function cook(z)
    if type(z) ~= 'table' then
        return z
    end
    local p, t, f = table.unpack(z)
    if t == true then
        if f == false then
            return "@" .. p
        end
        return "@" .. p .. "| " .. cook(f)
    end
    if t == false then
        if f == true then
            return "!" .. p
        end
        return "!" .. p .. "& " .. cook(f)
    end
    if f == true then
        return "!" .. p .. "| " .. cook(t)
    end
    if f == false then
        return "@" .. p .. "& " .. cook(t)
    end
    return p .. "? " .. cook(t) .. ": " .. cook(f)
end

function weights(w, acc, r)
    if type(r) ~= "table" then
        return
    end
    local p, t, f = table.unpack(r)
    acc[p] = w + (acc[p] or 0)
    w = w * 0.61803398875
    weights(w, acc, t)
    weights(w, acc, f)
end

function identical(a, b)
    if type(a) ~= 'table' or type(b) ~= 'table' then
        return a == b
    end
    local ap, at, af = table.unpack(a)
    local bp, bt, bf = table.unpack(b)
    return ap == bp and identical(at, bt) and identical(af, bf)
end

function cond(n, t, f)
    if identical(t, f) then
        return t
    else
        return {n, t, f}
    end
end

function split(p, v, r)
    if type(r) ~= 'table' then
        return r
    end
    local n, t, f = table.unpack(r)
    if n ~= p then
        return cond(n, split(p, v, t), split(p, v, f))
    elseif v then
        return t
    else
        return f
    end
end

function optimize(r)
    if type(r) ~= 'table' then
        return r
    end
    local n, t, f = table.unpack(r)
    if type(t) ~= 'table' or type(f) ~= 'table' then
        return r
    end
    local wghts = {}
    weights(1, wghts, r)
    local p
    local weight = 0
    for k, w in pairs(wghts) do
        if w > weight then
            p = k
            weight = w
        end
    end
    return cond(p, optimize(split(p, true, r)), optimize(split(p, false, r)))
end

function narrow(p, x, y)
    if p(111111111111111 * x) then
        return y
    else
        return 0
    end
end

function martin(p)
    local n = narrow(p, 1, 1) + narrow(p, 2, 2) + narrow(p, 3, 4) + narrow(p, 4, 8)
    return p(n) ~= p(n+1)
end

print(cook(optimize(optimize(raw(martin)))))
