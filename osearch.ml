
let merge n x y u = if u < n then x u else y u

let force = Lazy.force

let merge1 f x y u = if u < f then force x u else y u
let merge2 f x y u = if u < f then x u else force y u

let lift f xx yy q =
  let yx x = lazy (yy (fun y -> q (merge f x y))) in
  let x = lazy (xx (fun x -> q (merge2 f x (yx x)))) in
  let y = lazy (yy (fun y -> q (merge1 f x y)))
  in fun u -> if u < f then force x u else force y u

let const x _ = x

let rec range m p q =
  if m+1 = p then const(q(const true))
  else let n = (m + p) / 2 in lift n (range m n) (range n p) q

let rec after m q = let n = 2 * m + 1 in lift n (range m n) (after n) q

let limit f =
  let rec betwn m n =
    if m+1 = n then m else
      let p = (m + n) / 2 in if f p then betwn p n else betwn m p in
  let rec after m =
    let n = 2 * m + 1 in if f n then after n else betwn m n
  in after 0

type raw = T | F | C of int * raw * raw

let rec raw p =
  let arbitrary n = n mod 2 = 1 in
  let pArbitrary = p arbitrary in
  let different = after 0 (fun f -> p f <> pArbitrary) in
  if p different = pArbitrary then if pArbitrary then T else F
  else
    let pivot = limit(fun n -> p(merge n arbitrary different) != pArbitrary) in
    let slice b = raw(fun f -> p(fun x -> if x = pivot then b else f(x)))
    in C(pivot, slice true, slice false)

let rec cook = function
    T -> print_char 'T'
  | F -> print_char 'F'
  | C(n, T, F) -> print_int n
  | C(n, F, T) -> Printf.printf "!%i" n
  | C(n, T, y) -> Printf.printf  "%i| " n ; cook y
  | C(n, F, y) -> Printf.printf "!%i& " n ; cook y
  | C(n, x, T) -> Printf.printf "!%i| " n ; cook x
  | C(n, x, F) -> Printf.printf  "%i& " n ; cook x
  | C(n, x, y) ->
     Printf.printf "IF %i (" n;
     cook x;
     print_char ',';
     cook y;
     print_char ')'

let golden = 0.61803398875

module W = Map.Make(Int)

let optadd w = function
    Some v -> Some(w +. v)
  | None -> Some w

let rec weights w = function
    C(n, x, y) ->
     let ww = w *. golden in
     fun m -> W.update n (optadd w) (weights ww x (weights ww y m))
  | _ -> fun m -> m

let cond n x y = if x = y then x else C(n, x, y)

let rec split p v = function
    C(n, x, y) ->
    if n = p then if v then x else y
    else cond n (split p v x) (split p v y)
  | r -> r

let maxval k v (kk, vv) = if v > vv then (k,v) else (kk,vv)

let rec optimize = function
    T -> T
  | F -> F
  | r ->
     let (p, _) = W.fold maxval (weights 1. r W.empty) (0, 0.) in
     cond p (optimize(split p true r)) (optimize(split p false r))

let martin p = let
    narrow x y = if p(111111111111111 * x) then y else 0 in
  let n = narrow 1 1 + narrow 2 2 + narrow 3 4 + narrow 4 8
  in p(n) != p(n+1)
;;

cook(optimize(optimize(raw(martin))))
