module Common.Vector where
import Control.Applicative
import Data.Foldable

data Vector a = Vector a a a deriving Show

instance Functor Vector where
    fmap f (Vector x y z) = Vector (f x) (f y) (f z)

instance Applicative Vector where
    pure a = Vector a a a
    Vector f g h <*> Vector x y z = Vector (f x) (g y) (h z)

instance Foldable Vector where
    foldMap f (Vector x y z) = f x <> f y <> f z

instance Traversable Vector where
    traverse f (Vector x y z) = Vector <$> f x <*> f y <*> f z

instance Num a => Num (Vector a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate v = negate <$> v
    abs v = abs <$> v
    signum v = signum <$> v
    fromInteger i = pure $ fromInteger i
