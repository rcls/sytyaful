module R where

import Data.Map hiding (map)
import GHC.Exts(inline)
import System.Environment(getArgs)

merge n x y u = if u < n then x u else y u

cc q xx f = inline f $ xx $ q . f  -- The inline decreases runtime by about 10%.

{-# INLINE lift #-}
lift !n xx yy q = cc q xx $ cc q yy . merge n

range m p q | m+1==p = const $ q $ const True
range m p q = lift n (range m n) (range n p) q where n = div (m + p) 2

after m q = lift n (range m n) (after n) q where n = 2 * m + 1

limit f = aft 0 where
  aft m = if f n then aft n else bet m n where n = 2 * m + 1
  bet m p|m+1==p = m
  bet m p = if f n then bet n p else bet m n where n = div (m+p) 2

data Raw a = K !a | C !Word !(Raw a) !(Raw a) deriving (Eq, Functor, Show)

raw p = if pArbitrary == p different then K pArbitrary
        else C pivot (slice True) (slice False) where
  arbitrary x = mod x 2 /= 0
  pArbitrary  = p arbitrary
  different   = after 0 $ \f -> p f /= pArbitrary
  pivot       = limit $ \n -> p (merge n arbitrary different) /= pArbitrary
  slice v     = raw   $ \f -> p $ \n -> if n == pivot then v else f n

data Graph = Graph :|: Graph | Graph :&: Graph | Graph :^: Graph
  | IF Word Graph Graph | T | F | AT Word | NAT Word deriving Show

cook(K b) = if b then T else F
cook(C n (K True) (K False)) = AT n
cook(C n (K False) (K True)) = NAT n
cook(C n (K x) r) = if x then  AT n :|: cook r else NAT n :&: cook r
cook(C n r (K x)) = if x then NAT n :|: cook r else  AT n :&: cook r
cook(C n r s) | r==(not<$>s) = AT n :^: cook s
cook(C n r s) = IF n (cook r) (cook s)

-- golden = (sqrt(5) - 1) * 0.5 + 1e-10
golden = 0.61803398875

cond n x y = if x == y then x else C n x y

optimize g@(C _ _ _) = cond pivot (branch True) (branch False) where
  branch b = optimize $ slice g where
    slice (C x l r) | x==pivot = if b then l else r
    slice (C x l r) = cond x (slice l) (slice r)
    slice a = a
  (_, pivot) = maximum $ map(\(a,b)->(b,a)) $ toList $ weights 1 g empty
  weights b (C x l r) = insertWith (+) x b . weights bb l . weights bb r where
    bb = b * golden
  weights _ _ = id
optimize g = g

martin slow p = p(n) /= p(n+1) where
  n = narrow 1 1 + narrow 2 2 + narrow 3 4 + narrow 4 8
    + if slow then narrow 5 8 else 0
  narrow x y = if p(111111111111111 * x) then y else 0

main = do
  args <- getArgs
  let slow = length args >= 1 && head args == "slow"
  print $ cook $ optimize $ optimize $ raw $ martin slow
