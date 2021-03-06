
(* Forcing the use of int here makes about 10% difference to CPU usage.  *)
let merge (n: int) x y u = if u < n then x u else y u

let wrap s u = Lazy.force s u

let cc q xx f = f (wrap (lazy (xx (fun x -> q (f x)))))

let join f xx yy q = cc q xx (fun x -> cc q yy (merge f x))

let const x _ = x

let tree node leaf u =
  let rec range m p u =
    let n = (m + p) / 2
    in if m+1 = p
       then leaf m u
       else node n (range m n) (range n p) u in
  let rec after m u = let n = 2*m+1 in node n (range m n) (after n) u
  in after 0 u

let best = tree join (const (fun q -> const(q(const true))))

let limit f = tree (fun p x y () -> if f p then y() else x()) const ()

type raw = T | F | C of int * raw * raw

let rec raw p =
  let arbitrary n = n mod 2 = 1 in
  let pArbitrary = p arbitrary in
  let different = best (fun f -> p f <> pArbitrary) in
  if p different = pArbitrary then if pArbitrary then T else F
  else
    let pivot = limit(fun n -> p(merge n arbitrary different) != pArbitrary) in
    let slice b = raw(fun f -> p(fun x -> if x = pivot then b else f x))
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

let _ = cook(optimize(optimize(raw(martin))))
