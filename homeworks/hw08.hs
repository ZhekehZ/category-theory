-- Task 06

import Data.List ((\\), union, intersect, nub)
import Control.Monad (ap, liftM)
import Control.Applicative

class Group a where
    zro :: a
    (<>) :: a -> a -> a
    inv :: a -> a

data SetAbGroup a = SAG [a] [a] -- Grothendieck group of monoid of sets

instance Eq a => Group (SetAbGroup a) where -- Abelian group is the same
    zro = SAG [] []
    (SAG p1 m1) <> (SAG p2 m2) = SAG (p1 ++ p2) (m1 ++ m2) 
    inv (SAG p m) = SAG m p

instance Functor SetAbGroup where
    fmap = liftM

instance Applicative SetAbGroup where
    pure = return
    (<*>) = ap

instance Monad SetAbGroup where
    return x = SAG [x] []
    SAG ps ns >>= k = SAG (psp ++ nsn) (psn ++ nsp)
        where
            concatAll1 [x] = x 
            concatAll1 (SAG xs1 ys1 : SAG xs2 ys2 : ss) = concatAll1 (SAG (xs1 ++ xs2) (ys1 ++ ys2) : ss) 
            (SAG psp psn) = concatAll1 (k <$> ps)
            (SAG nsp nsn) = concatAll1 (k <$> ns)

instance Eq a => Eq (SetAbGroup a) where
    SAG p1 n1 == SAG p2 n2 = null diffPos && null diffNeg
        where 
            diff x1 x2 = (x1 \\ x2) `union` (x2 \\ x1)
            diffPos = diff p1 p2  
            diffNeg = diff n1 n2  


-- Task 09

data Term a = Var a
            | App (Term a) (Term a)
            | Lam (Term (Maybe a))
            deriving (Show, Eq)


instance Functor Term where
    fmap = liftM

instance Applicative Term where
    pure = return
    (<*>) = ap

instance Monad Term where
    return = Var
    term >>= k = case term of 
        Var x   -> k x
        App x y -> App (x >>= k) (y >>= k) 
        Lam x   -> Lam (x >>= maybe (Var Nothing) (fmap Just . k))

normalizeStep :: Term a -> Maybe (Term a)
normalizeStep (Var a) = Nothing
normalizeStep (App (Lam t) y) = Just $ t >>= maybe y Var
normalizeStep (App x y) = flip App y <$> normalizeStep x <|> App x <$> normalizeStep y
normalizeStep (Lam t) = Lam <$> normalizeStep t

normalize :: Term a -> Term a
normalize t = maybe t normalize (normalizeStep t) 
