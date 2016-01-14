{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fplugin=TypeNatSolver #-}
module Example where

import GHC.TypeLits
import Data.Proxy
import Unsafe.Coerce
import Control.Monad

vectorZipSumIO :: IO ()
vectorZipSumIO = do
  line1 <- getLine
  line2 <- getLine
  let readVec1 = (reads line1 :: [([Int], String)])
  let readVec2 = (reads line2 :: [([Int], String)])
  case (readVec1 , readVec2) of
    (((vec1list, _):_), ((vec2list,_):_)) -> do
      case (length vec1list == length vec2list) of
        True -> do
          let l = toInteger (length vec1list)
          let Just someNatL = someNatVal (l-1)
          case someNatL of
            SomeNat (_ :: Proxy n) -> do
              let vec1 = vCreate (toUNat (NatProxy :: NatProxy n)) vec1list 0    
              let vec2 = vCreate (toUNat (NatProxy :: NatProxy n)) vec2list 0  
              print $ vSum $ vZipWith vec1 vec2 (+)
        _    -> putStr "Fehler"
    _                                     -> putStr "Fehler"

matMultIO1 :: IO ()
matMultIO1 = do
    m <- readLn :: IO Integer
    n <- readLn :: IO Integer
    p <- readLn :: IO Integer
    line1 <- getLine
    let mat1list = (read line1 :: [[Int]])
    line2 <- getLine
    let mat2list = (read line2 :: [[Int]]) 
    let Just someNatM = someNatVal (m-1)
    let Just someNatN = someNatVal (n-1)
    let Just someNatP = someNatVal (p-1)
    case (someNatM, someNatN, someNatP) of
      (SomeNat (_ :: Proxy m), 
       SomeNat (_ :: Proxy n), 
       SomeNat (_ :: Proxy p)) -> do
             let natM = NatProxy :: NatProxy m 
             let natN = NatProxy :: NatProxy n 
             let natP = NatProxy :: NatProxy p  
             let mat1 = mCreate (toUNat natM) (toUNat natN) mat1list 0
             let mat2 = mCreate (toUNat natN) (toUNat natP) mat2list 0
             print (mMult mat1 mat2)

matMultIO2 :: IO ()
matMultIO2 = do
    line1 <- getLine
    let mat1list = (read line1 :: [[Int]])
    line2 <- getLine
    let mat2list = (read line2 :: [[Int]]) 
    let m = toInteger $ length mat1list
    let n = toInteger $ length (head mat1list)
    let p = toInteger $ length (head mat2list)
    let Just someNatM = someNatVal (m-1)
    let Just someNatN = someNatVal (n-1)
    let Just someNatP = someNatVal (p-1)
    case (someNatM, someNatN, someNatP) of
      (SomeNat (_ :: Proxy m), 
       SomeNat (_ :: Proxy n), 
       SomeNat (_ :: Proxy p)) -> do
             let natM = NatProxy :: NatProxy m 
             let natN = NatProxy :: NatProxy n 
             let natP = NatProxy :: NatProxy p  
             let mat1 = mCreate (toUNat natM) (toUNat natN) mat1list 0
             let mat2 = mCreate (toUNat natN) (toUNat natP) mat2list 0
             print (mMult mat1 mat2)

data Vec :: * -> Nat -> * where 
  Nil :: Vec a 0
  Cons :: a -> Vec a len -> Vec a (1 + len)

instance {-# OVERLAPPABLE #-} (Show a) => Show (Vec a n) where
  show Nil          = "Nil"
  show (Cons a Nil) = show a
  show (Cons a xs)  = (show a) ++ ", " ++ (show xs)

type Matrix a m n = Vec (Vec a n) m

instance {-# OVERLAPPING #-} (Show a) => Show (Vec (Vec a n) m) where
  show Nil = ""
  show (Cons vec xs) = show vec ++ "\n" ++ show xs

data UNat :: Nat -> * where
  UZero    :: UNat 0 
  USucc    :: UNat n -> UNat (1 + n) 

instance Show (UNat n) where
  show UZero     = "UZero"
  show (USucc x) = "USucc " ++ (show x)

data NatProxy (n :: Nat) = NatProxy

toUNat :: KnownNat n => NatProxy n -> UNat n
toUNat p = convert (natVal p)
  where convert :: Integer -> UNat m
        convert 0 = unsafeCoerce $ UZero
        convert x = unsafeCoerce $ USucc $ convert $ x - 1

-- Creates a vector of length (n+1) where the elements are taken from the list
-- When the list is empty it will fill the rest with the third argument 
vCreate :: UNat n -> [a] -> a -> Vec a (n+1)
vCreate UZero (x:_) _          = (Cons x Nil)
vCreate UZero []    a          = (Cons a Nil)
vCreate (USucc rest) (x:xs) a  = (Cons x $ vCreate rest xs a)
vCreate (USucc rest) [] a      = (Cons a $ vCreate rest [] a)

-- Creates a matrix of size (m+1) x (n+1) where the elements are taken from the lists
-- When a list is empty it will fill the rest with the fourth argument 
mCreate :: UNat m -> UNat n -> [[a]] -> a -> Matrix a (m+1) (n+1) 
mCreate UZero        nat (xs:_)   a = Cons (vCreate nat xs a) Nil
mCreate UZero        nat []       a = Cons (vCreate nat [] a) Nil
mCreate (USucc rest) nat (xs:xss) a = Cons (vCreate nat xs a) $ (mCreate rest nat xss a)
mCreate (USucc rest) nat []       a = Cons (vCreate nat [] a) $ (mCreate rest nat [] a)

-- Vectorfold
vFoldr :: (a -> b -> b) -> b -> Vec a n -> b
vFoldr _ b Nil         = b
vFoldr f b (Cons a xs) = f a (vFoldr f b xs)

-- Vectorsum
vSum :: Num a => Vec a l -> a
vSum vec = vFoldr (+) 0 vec 

-- Vectorlength as Integer
vLength :: KnownNat n => Vec a n -> Integer
vLength vec = natVal vec

-- Vectorlength as UNat
vLengthU :: KnownNat n => Vec a n -> UNat n
vLengthU _ = toUNat (NatProxy :: NatProxy n)

-- Append two vectors
vAppend :: Vec a n -> Vec a m -> Vec a (n + m)
vAppend Nil l = l
vAppend (Cons x xs) ys = (Cons x (vAppend xs ys))

-- Vector map
vMap :: Vec a n -> (a -> b) -> Vec b n
vMap Nil         _ = Nil
vMap (Cons x xs) f = Cons (f x) (vMap xs f)

-- zipWith for vectors
vZipWith :: Vec a n -> Vec b n -> (a -> b -> c) -> Vec c n
vZipWith Nil Nil _                 = Nil
vZipWith (Cons x xs) (Cons y ys) f = Cons (f x y) (vZipWith xs ys f)

-- Split a vector at position nat so that the first vector has the first nat elements
-- and the second vector has the rest
vSplitAt :: (1 <= l, nat <= l, (nat + n) ~ l) => Vec a l -> UNat nat -> (Vec a nat, Vec a n)
vSplitAt (Cons x xs) (USucc UZero)        = ((Cons x Nil), xs)
vSplitAt (Cons x xs) (USucc (USucc rest)) = 
  let (left, right) = vSplitAt xs (USucc rest) 
  in ((Cons x left), right)

-- get the nth element of a vector
vNth :: ((m + 1) <= n) => Vec a n -> UNat m -> a
vNth (Cons x _) UZero = x 
vNth (Cons _ xs) (USucc rest) = vNth xs rest

-- reverse a vector
vReverse :: Vec a n -> Vec a n
vReverse Nil         = Nil
vReverse (Cons x xs) = vAppend (vReverse xs) (Cons x Nil)

-- zipWith for matrices
mZipWith :: Matrix a m n -> Matrix b m n -> (a -> b -> c) -> Matrix c m n
mZipWith xss yss f = vZipWith xss yss (\xs ys -> vZipWith xs ys f)

-- Matrix map
mMap :: Matrix a m n -> (a -> b) -> Matrix b m n
mMap xss f = vMap xss (\xs -> vMap xs f)

-- Add two matrices
mAdd :: (Num a) => Matrix a m n -> Matrix a m n -> Matrix a m n
mAdd xss yss = mZipWith xss yss (+)

-- Get the nth row of a matrix
mNthRow :: ((l + 1) <= m) => Matrix a m n -> UNat l -> Vec a n
mNthRow = vNth 

-- Get the nth column of a matrix
mNthCol :: ((l + 1) <= n) => Matrix a m n -> UNat l -> Vec a m
mNthCol Nil nat             = Nil
mNthCol (Cons row rows) nat = Cons (vNth row nat) (mNthCol rows nat)

-- Matrix Multiplication
mMult :: forall a m n p . (Num a, 1 <= m, 1 <= n, 1 <= p) => Matrix a m n -> Matrix a n p -> Matrix a m p
mMult m n = let nTrans =  (mTranspose n) in 
            vMap m (\vec -> mMult' vec nTrans)
            where 
              mMult' :: Vec a n -> Matrix a q n -> Vec a q
              mMult' vec Nil               = Nil
              mMult' vecA (Cons vecB vecs) = Cons (vSum (vZipWith vecA vecB (*))) (mMult' vecA vecs)

-- Cut the first row from a matrix
mTail :: Matrix a m n -> Matrix a m (n - 1)
mTail (Cons (Cons a xs) Nil) = (Cons xs Nil) 
mTail (Cons (Cons a xs) ys) = Cons xs (mTail ys)

-- Transpose a matrix
mTranspose :: (1 <= m, 1 <= n) => Matrix a m n -> Matrix a n m
mTranspose mat@(Cons (Cons _ Nil) _) = (Cons (mNthCol mat UZero) Nil)
mTranspose mat@(Cons (Cons _ (Cons _ _)) _) = Cons (mNthCol mat UZero) (mTranspose (mTail mat))  

--------------------------Test Data--------------------------------
vectorOne   = (Cons 3 (Cons 2 (Cons 1 (Cons 7 (Cons 9 (Cons 3 (Cons 8 Nil)))))))
vectorTwo   = (Cons 3 (Cons 8 (Cons 2 (Cons 4 (Cons 8 (Cons 1 Nil))))))
(vectorThree, vectorFour) = vSplitAt vectorOne (toUNat (NatProxy :: NatProxy 3))
(vectorFive, vectorSix)   = vSplitAt vectorTwo (toUNat (NatProxy :: NatProxy 2))
vectorSeven   = vAppend vectorThree vectorSix --Length 7
vectorEight = vAppend (vAppend vectorFour vectorFive) (Cons 1 Nil)
vectorSum = vSum (vZipWith vectorSeven vectorEight (+))

mVectorOne = Cons 3 (Cons 2 (Cons 1 Nil))
mVectorTwo = Cons 1 (Cons 0 (Cons 2 Nil))
mVectorThree = Cons 1 (Cons 2 Nil)
mVectorFour = Cons 0 (Cons 1 Nil)
mVectorFive = Cons 4 (Cons 0 Nil)
matrixOne = Cons mVectorOne (Cons mVectorTwo Nil)
matrixTwo = Cons mVectorThree (Cons mVectorFour (Cons mVectorFive Nil))
matrixMult = mMult matrixOne matrixTwo
