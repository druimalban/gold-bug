-- file test/Spec.hs
-- Test functionality.

module Main where

import Data.List.NonEmpty (NonEmpty, fromList)
import System.Keyboard.Combinators (KeyCode(..), Input(..))
import Test.QuickCheck ( Arbitrary
                       , arbitrary
                       , elements
                       , oneof
                       , quickCheck
                       , suchThat
                       , withMaxSuccess)

-- Define an instance of arbitrary for NonEmpty.
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = fromList <$> suchThat arbitrary (\x -> length x >= 1)

-- Generator for arbitrary key-codes.
instance Arbitrary KeyCode where
  arbitrary = elements [P, Q, R, Space]

instance Arbitrary Input where
  arbitrary = oneof [ Key   <$> arbitrary
                    , Chord <$> arbitrary
                    , return Neutral ]

-- Test associativity and identity laws.
prop_Input :: Input -> Input -> Input -> Bool
prop_Input p q r = p <> (q <> r) == (p <> q) <> r &&
                   p <> mempty == p && mempty <> p == p &&
                   q <> mempty == q && mempty <> q == q &&
                   r <> mempty == r && mempty <> r == r

main :: IO ()
main = quickCheck (withMaxSuccess 1000 prop_Input)
