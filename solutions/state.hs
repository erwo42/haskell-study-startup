{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module OwnState where

import Numeric.Natural
import Prelude hiding (replicate)
import qualified Prelude as P

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

replicate :: Integral b => a -> b -> [a]
replicate elem count = P.replicate (fromIntegral count) elem

natToRoman' :: OwnState Natural String
natToRoman' = do
  ms  <- replicate "M"  <$> countOf 1000
  cms <- replicate "CM" <$> countOf 900
  ds  <- replicate "D"  <$> countOf 500
  cds <- replicate "CD" <$> countOf 400
  cs  <- replicate "C"  <$> countOf 100
  xcs <- replicate "XC" <$> countOf 90
  ls  <- replicate "L"  <$> countOf 50
  xls <- replicate "XL" <$> countOf 40 
  xs  <- replicate "X"  <$> countOf 10 
  ixs <- replicate "IX" <$> countOf  9 
  vs  <- replicate "V"  <$> countOf  5
  ivs <- replicate "IV" <$> countOf  4
  is  <- replicate "I"  <$> countOf  1
  return $ concat $ concat [ms, cms, ds, cds, cs, xcs, ls, xls, xs, ixs, vs, ivs, is]

-- countOfCorrect
-- countOfCorrect amount = OwnState $ \s -> s `divMod` amount

countOf :: Natural -> OwnState Natural Natural
countOf amount = do
--   remainder <- get
--   let (mult, newRemainder) = remainder `divMod` amount
--   put newRemainder
  return 1

natToRoman :: Natural -> String
natToRoman n = let (_, value) = runOwnState natToRoman' n in value































-- First part, implementing State

data OwnState s a = OwnState { runOwnState :: s -> (s, a) }

instance Functor (OwnState s) where
  fmap f OwnState { runOwnState = g } = OwnState (\s -> let (s', a) = g s
                                                        in (s', f a))

instance Applicative (OwnState s) where
  pure v = OwnState (\s -> (s, v))
  OwnState f <*> OwnState value = OwnState (\s -> let
                                                      (s', func) = f s
                                                      (s'', val) = value s'
                                                  in (s'', func val))

instance Monad (OwnState s) where
  return = pure
  OwnState value >>= f = OwnState (\s -> let (s', val) = value s
                                             OwnState func = f val
                                         in func s')

-- get :: OwnState s s 
-- get = OwnState (\s -> (s, s))

-- put :: s -> OwnState s ()
-- put newState = OwnState (\_ -> (newState, ()))



--- Stuff for testing

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (OwnState s a) where arbitrary = OwnState <$> arbitrary
instance (Show s, Eq s, Eq a, Arbitrary s) => EqProp (OwnState s a) where (OwnState f) =-= (OwnState g) = forAll arbitrary (\s -> f s == g s)
instance Show (OwnState s a) where show _ = "<beep>"

checkOwnState :: IO ()
checkOwnState = quickBatch $ do
   functor (undefined :: OwnState Int (Int, Int, Int)) 
   applicative (undefined :: OwnState Int (Int, Int, Int)) 
   monad (undefined :: OwnState Int (Int, Int, Int)) 
   -- monadApplicative (undefined :: OwnState Int (Int, Int)) 




-- Perhaps draw some images how put and get work ?
















-- That works. Now we want to print something on the screen in countOf, or access a
-- database or something ...
-- So we need the ability to 'print' or generally do IO.
-- But maybe even some other Monad, whatever comes to mind.
--
-- So we need a function like `lift :: Monad m => m () -> State s ()`
--
--
-- For a monad, we don't know anything about, we can only inject things
-- with `fmap`, `<*>` or `>>=`. We can never get stuff out. So we have
-- to work in the foreign monad. (In our example later `IO`).
--
-- So we want to change countOf like that:



countOfT :: Natural -> NatToRomanT Natural
countOfT amount = do
  remainder <- get
  let (mult, newRemainder) = remainder `divMod` amount
  konfig <- ask
  liftIO $ putStrLn $ konfig ++ ":" ++ show remainder ++ " / " ++ show amount ++ " = (" ++ show mult ++ "," ++ show newRemainder ++ ")"
  put newRemainder
  return mult

data OwnStateT s m a = OwnStateT { runOwnStateT :: s -> m (s, a) }

instance Functor m => Functor (OwnStateT s m) where
  fmap f OwnStateT { runOwnStateT = g } = OwnStateT (\s -> fmap f <$> g s) -- second fmap is for tuples, so fmap :: (a -> b) -> (c, a) -> (c, b)

   -- g s :: IO (s, a)
   -- fmap f :: (s, a) -> (s, b)
   -- applySnd f <$> g s :: IO (s, b); IO (s, f a)
   --    where applySnd f (a, b) = (a, f b)

instance Monad m => Applicative (OwnStateT s m) where
  pure v = OwnStateT (\s -> pure (s, v))
  OwnStateT func <*> OwnStateT value = OwnStateT (\s -> do
                                                           (s', val) <- value s
                                                           (s'', f)  <- func s'
                                                           return (s'', f val))

instance Monad m => Monad (OwnStateT s m) where
  return = pure
  OwnStateT value >>= f = OwnStateT (\s -> do
                                              (s', val) <- value s
                                              let OwnStateT f' = f val
                                              f' s')


class Monad m => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO action = action

instance MonadIO m => MonadIO (OwnStateT s m) where
  liftIO action = OwnStateT $ \s -> do erg <- liftIO action
                                       return (s, erg)

instance MonadIO m => MonadIO (OwnReaderT r m) where
  liftIO action = OwnReaderT $ \r -> do erg <- liftIO action
                                        return erg

class Monad m => MonadReader r m | m -> r where
  ask :: m r

instance Monad m => MonadReader r (OwnReaderT r m) where
  ask = OwnReaderT $ \r -> return r

instance MonadReader r m => MonadReader r (OwnStateT s m) where
  ask = OwnStateT $ \s -> (\r -> (s, r)) <$> ask

instance MonadState s m => MonadState s (OwnReaderT r m) where
  get = OwnReaderT (\_ -> get)
  put newState = OwnReaderT (\_ -> put newState)

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

instance Monad m => MonadState s (OwnStateT s m) where
  get = OwnStateT (\s -> return (s, s))
  put newState = OwnStateT (\s -> return (newState, ()))

type NatToRomanT = OwnReaderT String (OwnStateT Natural IO)

natToRomanT' :: NatToRomanT String
natToRomanT' = do
  ms  <- replicate "M"  <$> countOfT 1000
  cms <- replicate "CM" <$> countOfT 900
  ds  <- replicate "D"  <$> countOfT 500
  cds <- replicate "CD" <$> countOfT 400
  cs  <- replicate "C"  <$> countOfT 100
  xcs <- replicate "XC" <$> countOfT 90
  ls  <- replicate "L"  <$> countOfT 50
  xls <- replicate "XL" <$> countOfT 40 
  xs  <- replicate "X"  <$> countOfT 10 
  ixs <- replicate "IX" <$> countOfT  9 
  vs  <- replicate "V"  <$> countOfT  5
  ivs <- replicate "IV" <$> countOfT  4
  is  <- replicate "I"  <$> countOfT  1
  return $ concat $ concat [ms, cms, ds, cds, cs, xcs, ls, xls, xs, ixs, vs, ivs, is]

natToRomanT :: Natural -> IO String
natToRomanT n = snd <$> runOwnStateT (runOwnReaderT natToRomanT' "Testkonfig") n

data OwnReaderT r m a = OwnReaderT { runOwnReaderT :: r -> m a }
instance Functor m => Functor (OwnReaderT r m) where fmap f (OwnReaderT val) = OwnReaderT (\r -> f <$> val r)
instance Applicative m => Applicative (OwnReaderT r m) where 
  pure val = OwnReaderT (\_ -> pure val)
  OwnReaderT f <*> OwnReaderT value = OwnReaderT $ \r -> f r <*> value r
instance Monad m => Monad (OwnReaderT r m) where
  return = pure
  OwnReaderT value >>= f = OwnReaderT $ \r -> value r >>= \v -> runOwnReaderT (f v) r
                                              -- do
                                              --   v <- value r
                                              --   runOwnReaderT (f v) r
                                                
                                              --   let OwnReaderT erg = f v
                                              --   erg r
