{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{- |
Module      :  Safe.Fail
Copyright   :  (c) Neil Mitchell 2007-2008, Jose Iborra 2009
License     :  BSD3

Maintainer  :  http://community.haskell.org/~ndm/
Stability   :  in-progress
Portability :  portable

A library for safe functions, based on standard functions that may crash.
This module reexports versions which produce exceptions in an arbitrary MonadFail monad.

-}

module Safe.Fail where

import Control.Exception
import Control.Monad.Failure
import Data.Maybe
import Data.Typeable


{-| @def@, use it to return a default value in the event of an error.

   E.g. you can define a version of @tail@ which returns a default
   value when the list is empty

>  tailDef defaultValue = def defaultValue . tail
-}

def :: a -> Maybe a -> a
def v = fromMaybe v

{-| @note@, use it to fail with an annotated runtime error
-}

note :: Exception e => String -> Either e a -> a
note msg = either (\e -> error (show e ++ ": " ++ msg)) id

data SafeException = forall e. Exception e => SafeException e
  deriving (Typeable)
instance Show SafeException where
    showsPrec p (SafeException e) = ("Safe Exception:" ++) . showsPrec p e
instance Exception SafeException

safeExceptionToException :: Exception e => e -> SomeException
safeExceptionToException   = toException . SafeException

safeExceptionFromException :: Exception e => SomeException -> Maybe e
safeExceptionFromException e = do { SafeException e' <- fromException e; cast e'}

liftFail :: MonadFail e m => (a -> Bool) -> e -> (a -> b) -> a -> m b
liftFail test e f val = if test val then failure e else return (f val)


data TailFail = TailFail deriving (Show,Typeable)
instance Exception TailFail where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

tail :: MonadFail TailFail m => [a] -> m [a]
tail = liftFail null TailFail Prelude.tail


data InitFail = InitFail deriving (Show,Typeable)
instance Exception InitFail where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

init :: MonadFail InitFail m => [a] -> m [a]
init = liftFail null InitFail Prelude.init


data HeadFail = HeadFail deriving (Show,Typeable)
instance Exception HeadFail where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

head :: MonadFail HeadFail m => [a] -> m a
head = liftFail null HeadFail Prelude.head


data LastFail = LastFail deriving (Show,Typeable)
instance Exception LastFail where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

last :: MonadFail LastFail m => [a] -> m a
last = liftFail null LastFail Prelude.last


data MinimumFail = MinimumFail deriving (Show,Typeable)
instance Exception MinimumFail where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

minimum  :: (Ord a, MonadFail MinimumFail m) => [a] -> m a
minimum  = liftFail null MinimumFail Prelude.minimum


data MaximumFail = MaximumFail deriving (Show,Typeable)
instance Exception MaximumFail where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

maximum  :: (Ord a, MonadFail MaximumFail m) => [a] -> m a
maximum  = liftFail null MaximumFail Prelude.maximum


data Foldr1Fail = Foldr1Fail deriving (Show,Typeable)
instance Exception Foldr1Fail where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

foldr1  :: MonadFail Foldr1Fail m => (a -> a -> a) -> [a] -> m a
foldr1 f = liftFail null Foldr1Fail (Prelude.foldr1 f)


data Foldl1Fail = Foldl1Fail deriving (Show,Typeable)
instance Exception Foldl1Fail where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

foldl1  :: MonadFail Foldl1Fail m => (a -> a -> a) -> [a] -> m a
foldl1 f = liftFail null Foldl1Fail (Prelude.foldl1 f)


data FromJustFail = FromJustFail deriving (Show,Typeable)
instance Exception FromJustFail where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

fromJust :: MonadFail FromJustFail m => Maybe a -> m a
fromJust  = liftFail isNothing FromJustFail Data.Maybe.fromJust


data IndexFail = IndexFail Int deriving (Show,Typeable)
instance Exception IndexFail where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

at :: MonadFail IndexFail m => [a] -> Int -> m a
at xs n
   | n < 0 = failure (IndexFail n)
   | otherwise = go xs n
  where
    go [] _    = failure (IndexFail n)
    go (x:_) 0 = return x


data ReadFail = ReadFail String deriving (Show,Typeable)
instance Exception ReadFail where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

read :: (Read a, MonadFail ReadFail m) => String -> m a
read s = maybe (failure (ReadFail s)) return (readMay s)
  where
    readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of
                [x] -> Just x
                _ -> Nothing


data LookupFail a = LookupFail a deriving (Show,Typeable)
instance (Typeable a, Show a) => Exception (LookupFail a) where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

-- |
-- > lookupJust key = fromJust . lookup key
lookup :: (Eq a, MonadFail (LookupFail a) m) => a -> [(a,b)] -> m b
lookup key = maybe (failure (LookupFail key)) return . Prelude.lookup key

