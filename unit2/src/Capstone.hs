module Capstone where

import qualified Data.Bits     as Bit
import           System.Random

data FourLetterAlphabet
   = L1
   | L2
   | L3
   | L4
   deriving (Show, Enum, Bounded)

data ThreeLetterAlphabet
   = Alpha
   | Beta
   | Kappa
   deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

fourLetterMessage :: [FourLetterAlphabet]
fourLetterMessage = [L3, L1, L2, L3, L3, L4]

rotNEncoder :: (Bounded a, Enum a) => Int -> a -> a
rotNEncoder size letter = toEnum rotated
  where
    halfSize = size `div` 2
    offset = fromEnum letter + halfSize
    rotated = offset `mod` size

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder = map rot3
  where
    rot3 = rotNEncoder alphaSize
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)

fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder = map rot4
  where
    rot4 = rotNEncoder alphaSize
    alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)

rotNDecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNDecoder size letter = toEnum rotated
  where
    halfSize = size `div` 2
    offset =
       if even size
          then fromEnum letter + halfSize
          else 1 + fromEnum letter + halfSize
    rotated = offset `mod` size

-- there's no need for a four letter one, it's symetrical by default
threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder = map rot3
  where
    rot3 = rotNDecoder alphaSize
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)

-- using regular strings
rotEncoder :: String -> String
rotEncoder = map rotChar
  where
    rotChar = rotNEncoder (1 + fromEnum (maxBound :: Char))

rotDecoder :: String -> String
rotDecoder = map rotCharDec
  where
    rotCharDec = rotNDecoder (1 + fromEnum (maxBound :: Char))

xor :: Bits -> Bits -> Bits
xor = zipWith Bit.xor

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
   if remainder == 0
      then False : intToBits' nextVal
      else True : intToBits' nextVal
  where
    remainder = n `mod` 2
    nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = replicate (maxBits - bitListSize) False ++ bitList
  where
    bitList = reverse . intToBits' $ n
    bitListSize = length bitList

bitsToInt :: Bits -> Int
bitsToInt bitList = sum . map (\x -> 2 ^ snd x) $ trueBitList
  where
    reversedBitList = reverse bitList
    bitListInd = zip reversedBitList [0 ..]
    trueBitList = filter fst bitListInd

charToBits :: Char -> Bits
charToBits = intToBits . fromEnum

bitsToChar :: Bits -> Char
bitsToChar = toEnum . bitsToInt

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = zipWith xor padBits plainTextBits
  where
    padBits = map charToBits pad
    plainTextBits = map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
  where
    bitList = applyOTP' pad plaintext

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Cipher a where
   encode :: a -> String -> String
   decode :: a -> String -> String

data Rot =
   Rot

instance Cipher Rot where
   encode Rot = rotEncoder
   decode Rot = rotDecoder

newtype OneTimePad =
   OTP String

instance Show OneTimePad where
   show (OTP p) = p

instance Cipher OneTimePad where
   encode (OTP pad) = applyOTP pad
   decode (OTP pad) = applyOTP pad

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

-- this I THINK creates a random infinite pad
randomOTP :: Int -> OneTimePad
randomOTP seed = OTP rString
  where
    rLetter = random (mkStdGen seed)
    rString = fst rLetter : randomOTP' (snd rLetter)

randomOTP' :: StdGen -> String
randomOTP' seed = fst rLetter : randomOTP' (snd rLetter)
  where
    rLetter = random seed
