module S where

import Control.Applicative hiding (empty)
import Data.Map hiding (map)

infixr 0 ?

newtype S a = S { (?) :: (a -> Bool) -> a }

cc :: (b -> Bool) -> S a -> (a -> b) -> b
cc q xx f = f (xx ? q . f)

instance Functor S where
  fmap f xx = S $ \q -> cc q xx f
  x <$ yy = S $ const x

instance Applicative S where
  pure = S . const
  ff <*> xx = S $ \q -> cc q ff $ cc q xx
  liftA2 f xx yy = S $ \q -> cc q xx $ cc q yy . f
  xx *> yy = yy
  xx <* yy = xx

instance Monad S where
  return = S . const
  xx >>= f = S $ \q -> cc q xx $ \x -> f x ? q
  xx >> yy = yy

tree :: (Word -> a -> a -> a) -> (Word -> a) -> a
tree join leaf = after 0 where
  after m = join p (range m p) (after p) where p = 2 * m + 1
  range m n | m+1==n = leaf m
  range m n = join p (range m p) (range p n) where p = m + div (n - m) 2

merge n f g x = if x < n then f x else g x

snat s q = tree (\n f g -> merge n <$> f <*> g) (\n -> const <$> s) ? q

best = snat $ S $ \f -> f True

-- Laziness allows us to use tree here...
limit p = tree join id where join n a b = if p n then b else a

data Raw = K Bool | C Word Raw Raw deriving (Eq, Show)

raw p = if pArbitrary == p different then K pArbitrary
        else C pivot (slice True) (slice False) where
  arbitrary x = mod x 2 /= 0
  pArbitrary  = p arbitrary
  different   = best  $ \f -> p f /= pArbitrary
  pivot       = limit $ \n -> p (merge n arbitrary different) /= pArbitrary
  slice v     = raw   $ \f -> p $ \n -> if n == pivot then v else f n

data Graph = Graph :|: Graph | Graph :&: Graph | IF Word Graph Graph
  | T | F | AT Word | NAT Word deriving Show

cook(K b) = if b then T else F
cook(C n (K x) (K y)) =
  if x then if y then T else AT n else if y then NAT n else F
cook(C n (K x) r) = if x then  AT n :|: cook r else NAT n :&: cook r
cook(C n r (K x)) = if x then NAT n :|: cook r else  AT n :&: cook r
cook(C n r s) = IF n (cook r) (cook s)

golden = 0.61803398875

cond n x y = if x == y then x else C n x y

optimize g@(C _ _ _) = cond pivot (branch True) (branch False) where
  branch b = optimize $ slice g where
    slice(C x l r) | x==pivot = if b then l else r
    slice(C x l r) = cond x (slice l) (slice r)
    slice a = a
  (_, pivot) = maximum $ map(\(a,b)->(b,a)) $ toList $ weights 1 g empty
  weights b (C x l r) = insertWith (+) x b . weights bb l . weights bb r where
    bb = b * golden
  weights _ _ = id
optimize g = g

graph = cook . optimize . optimize . raw

martin p = p(n) /= p(n+1) where
  n = narrow 1 1 + narrow 2 2 + narrow 3 4 + narrow 4 8
  narrow x y = if p(111111111111111 * x) then y else 0

main = print(graph(martin))
