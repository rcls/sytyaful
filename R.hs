module R where

import Data.Map hiding (map)
--import Numeric.Natural
type Nat = Word

--tree :: (Nat → a → a → a) → (Nat → a) → a
tree f l = after 0 where
  after b = f c (range b (b+1)) (after c) where c = 2 * b + 1
  range m 1 = l m
  range m c = f (m+b) (range m b) (range (m+b) b) where b = div c 2

merge n f g x = if x < n then f x else g x

cc q xx f = f(xx $ q . f)

-- best :: forall p . Integral p => ((p -> Bool) -> Bool) -> p -> Bool
best q = tree join leaf q where
  join n xx yy q = cc q xx $ cc q yy . merge n
  leaf n q = const $ q $ const True -- cc q ($True) const

limit p = tree join id where join n a b = if p n then b else a

data Raw = K Bool | C Nat Raw Raw deriving (Eq, Show)

raw p = if pArbitrary == p different then K pArbitrary
        else C pivot (slice True) (slice False) where
  arbitrary x = mod x 2 /= 0
  pArbitrary  = p arbitrary
  different   = best  $ \f -> p f /= pArbitrary
  pivot       = limit $ \n -> p (merge n arbitrary different) /= pArbitrary
  slice v     = raw   $ \f -> p $ \n -> if n == pivot then v else f n

data Graph =  T | F | IF Nat Graph Graph | AT Nat | NAT Nat |
  Graph :|: Graph | Graph :&: Graph deriving (Eq, Show)

cook(K b) = if b then T else F
cook(C n (K x) (K y)) =
  if x then if y then T else AT n else if y then NAT n else F
cook(C n (K x) r) = if x then  AT n :|: cook r else NAT n :&: cook r
cook(C n r (K x)) = if x then NAT n :|: cook r else  AT n :&: cook r
cook(C n r s) = IF n (cook r) (cook s)

-- golden = (sqrt(5) - 1) * 0.5 + 1e-10
golden = 0.61803398875

optimize g@(C _ _ _) = if sT == sF then sT else C pivot sT sF where
  sT = slice True
  sF = slice False
  slice b = optimize $ with g where
    with(C x l r) | x==pivot = if b then l else r
    with(C x l r) = if ll == rr then ll else C x ll rr where
      ll = with l
      rr = with r
    with a = a
  (_, pivot) = maximum $ map(\(a,b)->(b,a)) $ toList $ weights 1 g empty
  weights b (C x l r) = insertWith (+) x b . weights bb l . weights bb r where
    bb = b * golden
  weights _ _ = id
optimize g = g

graph = cook . optimize . optimize . raw

narrow p x y = if p(111111111111111 * x) then y else 0

ff p = p(n) /= p(n+1) where n = narrow p 1 1 + narrow p 2 2 + narrow p 4 4

gg p = p(n) /= p(n+1) where n = narrow p 1 1 + narrow p 2 2 + narrow p 3 4 + narrow p 4 8

main = print(graph(gg))
