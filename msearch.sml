
open Int64

(* mlton doesn't appear to have a built-in lazy so provide it.  We are only
   interested in the case where the value is a function type, so specialise for
   that. *)
fun lazy ff = let
    datatype ('a, 'b) laz = suspend of unit -> 'a -> 'b | value of 'a -> 'b
    val it = ref (suspend ff)
in
    fn u => case !it of
                suspend ff => let val f = ff() in it := value f; f u end
              | value f => f u
end

fun merge n x y u = if u < n then x u else y u

fun const x _ = x

fun join f xx yy q = let
    fun yx x = lazy (fn () => yy (fn y => q (merge f x y)))
    val x = lazy (fn () => xx (fn x => q (merge f x (yx x))))
in
    merge f x (yx x)
end

fun range m p q =
    if m+1 = p then const(q(const true))
    else let val n = (m + p) div 2 in join n (range m n) (range n p) q end

fun after m q = let val n = 2 * m + 1 in join n (range m n) (after n) q end

fun limit f = let
    fun betwn m n =
        if m+1 = n then m else
        let val p = (m + n) div 2 in if f p then betwn p n else betwn m p end
    fun after m =
        let val n = 2 * m + 1 in if f n then after n else betwn m n end
in
    after 0
end

datatype Raw = T | F | C of int * Raw * Raw

fun raw p = let
    fun arbitrary n = n mod 2 = 1
    val pArbitrary = p arbitrary
    val different = after 0 (fn f => p f <> pArbitrary)
in
    if p different = pArbitrary then if pArbitrary then T else F
    else let
        val pivot = limit(fn n => p(merge n arbitrary different) <> pArbitrary)
        fun slice b = raw(fn f => p(fn x => if x = pivot then b else f(x)))
    in C(pivot, slice true, slice false)
    end
end

fun add_in k v [] = [(k,v)]
  | add_in k v ((h as (l,w))::t) = if k = l then (l, Real.+(w,v))::t
                                   else h :: add_in k v t

val GOLDEN = 0.61803398875

fun weights w (C(n, x, y)) acc =
    let val ww = Real.*(w, GOLDEN)
    in add_in n w (weights ww x (weights ww y acc)) end
  | weights w x acc = acc

fun cond n x y = if x = y then x else C(n, x, y)

fun split p v (C(n,x,y)) =
    if p = n then if v then x else y
    else cond n (split p v x) (split p v y)
  | split p v x = x

fun heaviest k v [] = k
  | heaviest k v ((kk,vv)::t) =
    if Real.>(v,vv) then heaviest k v t else heaviest kk vv t

fun optimize T = T
  | optimize F = F
  | optimize r = let val n = heaviest 0 0.0 (weights 1.0 r []) in
    cond n (optimize (split n true r)) (optimize (split n false r)) end

fun pint s n t = (print s; print(toString n); print t)

fun cook(C(n, T, F)) = pint "" n ""
  | cook(C(n, F, T)) = pint "!" n ""
  | cook(C(n, T, y)) = (pint ""  n "| "; cook y)
  | cook(C(n, F, y)) = (pint "!" n "& "; cook y)
  | cook(C(n, x, T)) = (pint "!" n "| "; cook x)
  | cook(C(n, x, F)) = (pint ""  n "& "; cook x)
  | cook(C(n, x, y)) = (pint "IF " n " ("; cook x; print ","; cook y; print ")")
  | cook T = print "T"
  | cook F = print "F"

fun martin p = let
    fun narrow x y = if p(111111111111111 * x) then y else 0
    val n = narrow 1 1 + narrow 2 2 + narrow 3 4 + narrow 4 8
in p n <> p(n+1) end

val main = cook(optimize(optimize(raw martin)))
