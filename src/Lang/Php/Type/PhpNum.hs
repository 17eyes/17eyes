--
-- Copyright (c) 2012 by Tomasz Dudziak, Mateusz Kocielski
-- www.17eyes.com, hello@17eyes.com
--

{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Lang.Php.Type.PhpNum where

--
-- This module implements PHP numeric types - float and integer
--
-- Todo:
--
-- - what should we do on division by zero?
--

import Lang.Php.Ast.Common
import GHC.Real -- XXX: is there any way to get rid of this import?

data PhpNum = PhpNum (Either Integer Double)
  deriving (Eq, Show, Typeable, Data)

--
-- PHP does automatic casts if integer does not fit into Int type, the problem
-- with definition of bounds is that Int type on 32-bits machines is 4-bytes
-- and 8-bytes on 64-bits platforms, we do here small asumption that code is
-- running on 64-bits. Note that this Int is signed.
--

upperBound = 2^63-1
lowerBound = -(2^63)

instance Num PhpNum where
  abs (PhpNum (Left x)) = PhpNum $ Left (abs x)
  abs (PhpNum (Right x)) = PhpNum $ Right (abs x)

  signum (PhpNum (Left x)) = PhpNum $ Left (signum x)
  signum (PhpNum (Right x)) = PhpNum $ Right (signum x)

  fromInteger x =
    if x <= upperBound && x >= lowerBound
      then PhpNum $ Left x
      else PhpNum $ Right (fromInteger x)

  negate (PhpNum (Left x)) = PhpNum $ (Left $ -x)
  negate (PhpNum (Right x)) = PhpNum $ (Right $ -x)

  PhpNum (Right x) + (PhpNum (Right y)) = PhpNum $ Right (x+y)
  PhpNum (Left x) + (PhpNum (Left y)) = PhpNum $
    if x+y <= upperBound && x+y >= lowerBound
      then Left (x+y)
      else Right $ fromInteger (x+y)
  PhpNum (Left x) + (PhpNum (Right y)) = PhpNum $ Right (fromInteger x + y)
  -- in order to avoid redundancy
  PhpNum (Right x) + (PhpNum (Left y)) = PhpNum (Left y) + PhpNum (Right x)

  PhpNum (Right x) * (PhpNum (Right y)) = PhpNum $ Right (x*y)
  PhpNum (Left x) * (PhpNum (Left y)) = PhpNum $
    if x*y <= upperBound && x*y >= lowerBound
      then Left (x*y)
      else Right $ fromInteger (x*y)
  PhpNum (Left x) * (PhpNum (Right y)) = PhpNum $ Right (fromInteger x * y)
  -- avoid redundancy
  PhpNum (Right x) * (PhpNum (Left y)) = PhpNum (Left y) * PhpNum (Right x)

instance Fractional PhpNum where
  fromRational (x :% y) = PhpNum $
    if y == 0
      then (Left $ x)
      else (Right $ fromRational (x :% y))

  PhpNum (Right x) / (PhpNum (Right y)) = PhpNum $ Right (x/y)
  PhpNum (Left x) / (PhpNum (Left y)) = PhpNum $
    if x % y == 0 || y % x == 0
      then Left (x `div` y)
      else Right $ (fromInteger x / (fromInteger y))
  PhpNum (Left x) / (PhpNum (Right y)) = PhpNum $ Right (fromInteger x / y)
  -- in order to avoid redundancy
  PhpNum (Right x) / (PhpNum (Left y)) = PhpNum $ Right (x / (fromInteger y))

instance Ord PhpNum where
  compare (PhpNum (Left x)) (PhpNum (Left y)) = compare x y
  compare (PhpNum (Right x)) (PhpNum (Right y)) = compare x y
  compare (PhpNum (Left x)) (PhpNum (Right y)) = compare (fromInteger x) y
  compare (PhpNum (Right x)) (PhpNum (Left y)) = compare x (fromInteger y)

instance Real PhpNum where
  toRational (PhpNum (Right x)) = toRational x
  toRational (PhpNum (Left x)) = toRational x

instance Enum PhpNum where
  toEnum x = PhpNum $ Left (toInteger x)
  fromEnum (PhpNum (Left x)) = fromEnum x
  fromEnum (PhpNum (Right x)) = fromEnum x

instance Integral PhpNum where
  toInteger (PhpNum (Left x)) = x
  toInteger (PhpNum (Right x)) = truncate x

  quotRem (PhpNum (Left x)) (PhpNum (Left y)) = let 
    (quot, rem) = (quotRem x y) in
      (PhpNum $ Left quot, PhpNum $ Left rem)
  quotRem (PhpNum (Left x)) (PhpNum (Right y)) = let 
    (quot, rem) = (quotRem x $ truncate y) in
      (PhpNum $ Left quot, PhpNum $ Left rem)
  quotRem (PhpNum (Right x)) (PhpNum (Left y)) = let 
    (quot, rem) = (quotRem (truncate x) y) in
      (PhpNum $ Left quot, PhpNum $ Left rem) 
  quotRem (PhpNum (Right x)) (PhpNum (Right y)) = let 
    (quot, rem) = (quotRem (truncate x) (truncate y)) in
      (PhpNum $ Left quot, PhpNum $ Left rem) 

$(derive makeBinary ''PhpNum)
