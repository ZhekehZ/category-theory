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
            [p1', p2', n1', n2'] = nub <$> [p1, p2, n1, n2] 
            diff x1 x2 = (x1 \\ x2) `union` (x2 \\ x1)
            diffPos = diff (p1' \\ n1') (p2' \\ n2')  
            diffNeg = diff (n1' \\ p1') (n2' \\ p2')  


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



-- Task 07

newtype Semimodule a m = SM { unSM :: [(a, m)] }

(+++) :: Monoid a => Semimodule a m -> Semimodule a m -> Semimodule a m
SM a +++ SM b = SM $ a ++ b

(***) :: Monoid a => a -> Semimodule a m -> Semimodule a m
a *** SM asms = SM [(a `mappend` a', m) | (a', m) <- asms]

{-
    r *** (x +++ y) == (r *** x) +++ (r *** y) -- ok
    (r `mappend` s) *** x == r *** (s *** x)   -- ok
    mempty *** x = x                           -- ok
-}

interpretSemimodule :: (Monoid a, Monoid m) => (a -> m -> m) -> Semimodule a m -> m
interpretSemimodule p (SM asms) = foldMap (uncurry p) asms

instance Monoid a => Functor (Semimodule a) where
    fmap = liftM

instance Monoid a => Applicative (Semimodule a) where
    pure = return
    (<*>) = ap

instance Monoid a => Monad (Semimodule a) where
    return m = SM [(mempty, m)]
    SM ms >>= k = SM [(a1 `mappend` a2, nm) | (a1, om) <- ms, (a2, nm) <- unSM (k om)]

{-
    return a >>= h == h a                       -- ok
    m >>= return == m                           -- ok
    (m >>= g) >>= h == m >>= (\x -> g x >>= h)  -- ok  
-}


-- Task08

newtype Semimodule2 a m = SM2 { f :: a -> m } 
                                -- where f is homomorphism
                                -- f rzero == mempty
                                -- f (a `rplus` b) = f a `mappend` f b

class Ring r where
    rzero :: r
    rone :: r
    rplus :: r -> r -> r
    rmult :: r -> r -> r

szero :: (Ring a, Monoid m) => Semimodule2 a m
szero = SM2 $ \a -> mempty

(+-+) :: (Ring a, Monoid m) => Semimodule2 a m -> Semimodule2 a m -> Semimodule2 a m
SM2 f +-+ SM2 g = SM2 $ \a -> f a `mappend` g a

(*-*) :: (Ring a, Monoid m) => a -> Semimodule2 a m -> Semimodule2 a m
a *-* SM2 f = SM2 $ \a' -> f (a `rmult` a')

{-
    r *-* (x +-+ y) == (r *-* x) +-+ (r *-* y)      -- ok
    (r `rplus` s) *-* x == (r *-* x) +-+ (s *-* x)  -- ok
    rzero *-* x = mempty                            -- ok
    (r `rmult` s) *-* x == r *-* (s *-* x)          -- ok
    rone *-* x = x                                  -- ok
-}

instance Ring a => Functor (Semimodule2 a) where
    fmap = liftM

instance Ring a => Applicative (Semimodule2 a) where
    pure = return
    (<*>) = ap

instance Ring a => Monad (Semimodule2 a) where
    return m = SM2 (\a -> m)
    SM2 g >>= k = SM2 $ \a -> f (k (g a)) a 

{-
    return a >>= h == h a                       -- ok
    m >>= return == m                           -- ok 
    (m >>= g) >>= h == m >>= (\x -> g x >>= h)  -- ok   
-}
