{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{- |
Module      :  Safe.Fail
Copyright   :  (c) Neil Mitchell 2007-2008, Jose Iborra 2009
License     :  BSD3

Maintainer  :  pepeiborra@gmail.com
Stability   :  in-progress
Portability :  portable

A library for safe functions, based on standard functions that may crash.

This module reexports versions which produce exceptions in an arbitrary 'Failure'.

-}

module Safe.Failure (
-- * List Functions
Safe.Failure.head, Safe.Failure.tail,
Safe.Failure.init, Safe.Failure.last,
Safe.Failure.minimum, Safe.Failure.maximum,
Safe.Failure.foldr1, Safe.Failure.foldl1,
Safe.Failure.at, Safe.Failure.lookup,
-- * Maybe functions
Safe.Failure.fromJust,
-- * Other Prelude functions
Safe.Failure.read,
-- * Useful combinators
def, note,
-- * Assertions
Safe.Failure.assert,
-- * Exceptions
SafeException(..),
HeadFailure(..), TailFailure(..), InitFailure(..), LastFailure(..),
MaximumFailure(..), MinimumFailure(..),
Foldl1Failure(..), Foldr1Failure(..),
IndexFailure(..), LookupFailure(..),
FromJustFailure(..),
ReadFailure(..),
) where

import Control.Exception
import Control.Failure
import Data.Maybe
import Data.Typeable
import Control.Applicative (pure)

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

liftFailure :: ApplicativeFailure e m
            => (a -> Bool)
            -> e
            -> (a -> b)
            -> a
            -> m b
liftFailure test e f val = if test val then failure e else pure (f val)


data TailFailure = TailFailure deriving (Show,Typeable)
instance Exception TailFailure where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

tail :: ApplicativeFailure TailFailure m => [a] -> m [a]
tail = liftFailure null TailFailure Prelude.tail


data InitFailure = InitFailure deriving (Show,Typeable)
instance Exception InitFailure where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

init :: ApplicativeFailure InitFailure m => [a] -> m [a]
init = liftFailure null InitFailure Prelude.init


data HeadFailure = HeadFailure deriving (Show,Typeable)
instance Exception HeadFailure where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

head :: ApplicativeFailure HeadFailure m => [a] -> m a
head = liftFailure null HeadFailure Prelude.head


data LastFailure = LastFailure deriving (Show,Typeable)
instance Exception LastFailure where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

last :: ApplicativeFailure LastFailure m => [a] -> m a
last = liftFailure null LastFailure Prelude.last


data MinimumFailure = MinimumFailure deriving (Show,Typeable)
instance Exception MinimumFailure where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

minimum  :: (Ord a, ApplicativeFailure MinimumFailure m) => [a] -> m a
minimum  = liftFailure null MinimumFailure Prelude.minimum


data MaximumFailure = MaximumFailure deriving (Show,Typeable)
instance Exception MaximumFailure where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

maximum  :: (Ord a, ApplicativeFailure MaximumFailure m) => [a] -> m a
maximum  = liftFailure null MaximumFailure Prelude.maximum


data Foldr1Failure = Foldr1Failure deriving (Show,Typeable)
instance Exception Foldr1Failure where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

foldr1  :: ApplicativeFailure Foldr1Failure m => (a -> a -> a) -> [a] -> m a
foldr1 f = liftFailure null Foldr1Failure (Prelude.foldr1 f)


data Foldl1Failure = Foldl1Failure deriving (Show,Typeable)
instance Exception Foldl1Failure where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

foldl1  :: ApplicativeFailure Foldl1Failure m => (a -> a -> a) -> [a] -> m a
foldl1 f = liftFailure null Foldl1Failure (Prelude.foldl1 f)


data FromJustFailure = FromJustFailure deriving (Show,Typeable)
instance Exception FromJustFailure where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

fromJust :: ApplicativeFailure FromJustFailure m => Maybe a -> m a
fromJust  = liftFailure isNothing FromJustFailure Data.Maybe.fromJust


data IndexFailure = IndexFailure Int deriving (Show,Typeable)
instance Exception IndexFailure where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

at :: ApplicativeFailure IndexFailure m => [a] -> Int -> m a
at xs n
   | n < 0 = failure (IndexFailure n)
   | otherwise = go xs n
  where
    go [] _     = failure (IndexFailure n)
    go (x:_)  0 = pure x
    go (_:xx) i = go xx (i-1)


data ReadFailure = ReadFailure String deriving (Show,Typeable)
instance Exception ReadFailure where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

read :: (Read a, ApplicativeFailure ReadFailure m) => String -> m a
read s =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                [x] -> pure x
                _   -> failure (ReadFailure s)

data LookupFailure a = LookupFailure a deriving (Show,Typeable)
instance (Typeable a, Show a) => Exception (LookupFailure a) where
  fromException = safeExceptionFromException
  toException   = safeExceptionToException

-- |
-- > lookupJust key = fromJust . lookup key
lookup :: (Eq a, ApplicativeFailure (LookupFailure a) m)
       => a -> [(a,b)] -> m b
lookup key = maybe (failure (LookupFailure key)) pure . Prelude.lookup key

-- | Assert a value to be true. If true, returns the first value as a succss.
-- Otherwise, returns the second value as a failure.
assert :: (ApplicativeFailure e m, Exception e)
       => Bool
       -> v
       -> e
       -> m v
assert b v e = if b then pure v else failure e
