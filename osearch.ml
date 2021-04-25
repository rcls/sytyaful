
let merge n x y u = if u < n then x u else y u

let search_y_for_fxy f x yy q = let
    s = lazy (yy (fun y -> q (merge f x y)))
  in fun u -> Lazy.force s u

let search_x_for_fxy f xx yy q = let
    s = lazy (xx (fun x -> q (merge f x (search_y_for_fxy f x yy q))))
  in fun u -> Lazy.force s u

let lift f xx yy q =
  let x = search_x_for_fxy f xx yy q in
  let y = search_y_for_fxy f x  yy q
  in merge f x y

let const x _ = x

let rec range m p q =
  if m+1 = p then const(q(const true))
  else let n = (m + p) / 2 in lift n (range m n) (range n p) q

let rec after m q = let n = 2 * m + 1 in lift n (range m n) (after n) q

let limit f =
  let rec bet m p =
    if m+1 = p then m else
      let n = (m + p) / 2 in if f n then bet n p else bet m n in
  let rec aft m =
    let n = 2 * m + 1 in if f n then aft n else bet m n
  in aft 0

type raw = TT | FF | CC of int * raw * raw

let rec raw p =
  let arbitrary n = n mod 2 = 1 in
  let pArbitrary = p arbitrary in
  let different = after 0 (fun f -> p f <> pArbitrary) in
  if p different = pArbitrary then if pArbitrary then TT else FF
  else
    let pivot = limit(fun n -> p(merge n arbitrary different) != pArbitrary) in
    let slice b = raw(fun f -> p(fun x -> if x = pivot then b else f(x)))
    in CC(pivot, slice true, slice false)

let rec cook = function
    TT -> print_string "T"
  | FF -> print_string "F"
  | CC(n,TT,FF) -> print_int n
  | CC(n,FF,TT) -> Printf.printf "!%i" n
  | CC(n,TT, y) -> Printf.printf "%i| " n  ; cook y
  | CC(n,FF, y) -> Printf.printf "!%i& " n ; cook y
  | CC(n, x,TT) -> Printf.printf "!%i| " n ; cook x
  | CC(n, x,FF) -> Printf.printf "%i& " n ; cook x
  | CC(n, x, y) ->
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
    CC(n, x, y) ->
     let ww = w *. golden in
     fun m -> W.update n (optadd w) (weights ww x (weights ww y m))
  | _ -> fun m -> m

let rec split p v = function
    CC(n, x, y) ->
    if n = p then if v then x else y
      else
        let x = split p v x and y = split p v y
        in if x = y then x else CC(n, x, y)
  | r -> r

let maxval k v (kk, vv) = if v > vv then (k,v) else (kk,vv)

let rec optimize = function
    TT -> TT
  | FF -> FF
  | r ->
     let (p, _) = W.fold maxval (weights 1. r W.empty) (0, 0.) in
     let stt = optimize(split p true r)
     and sff = optimize(split p false r)
     in if stt = sff then stt else CC(p, stt, sff)

let martin p = let
    narrow x y = if p(111111111111111 * x) then y else 0 in
  let n = narrow 1 1 + narrow 2 2 + narrow 3 4 + narrow 4 8
  in p(n) != p(n+1)
;;

cook(optimize(optimize(raw(martin))))
