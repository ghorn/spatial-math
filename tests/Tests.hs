{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Main ( main ) where

import Control.Applicative
import qualified Data.Foldable as F
import qualified Data.Monoid as Mo
import Test.Framework
       ( Test, ColorMode(..), RunnerOptions'(..), TestOptions'(..)
       , defaultMainWithOpts, testGroup )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
-- import Test.QuickCheck.Gen

import SpatialMath

main :: IO ()
main = defaultMainWithOpts tests opts

close :: forall f . (F.Foldable f, Applicative f) => Double -> f Double -> f Double -> Bool
close eps f0 f1 = all (\x -> abs x <= eps) deltas
  where
    delta :: f Double
    delta = (-) <$> f0 <*> f1

    deltas = F.toList delta

closeDcm :: Double -> M33 Double -> M33 Double -> Bool
closeDcm eps f0 f1 = all (\x -> abs x <= eps) deltas
  where
    delta :: V3 (V3 Double)
    delta = (-) <$> f0 <*> f1

    deltas = concatMap F.toList (F.toList delta)

instance Arbitrary (Euler Double) where
  arbitrary = do
    yaw <- choose (-0.99*pi, 0.99*pi)
    pitch <- choose (-0.9*pi/2, 0.9*pi/2)
    roll <- choose (-0.99*pi, 0.99*pi)
    return
      Euler
      { eYaw = yaw
      , ePitch = pitch
      , eRoll = roll
      }

instance Arbitrary (Quaternion Double) where
--  arbitrary = quatOfEuler321 <$> arbitrary
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    let norm = sqrt (w*w + x*x + y*y + z*z)
        ret
          | norm == 0 =
              elements
              [ Quaternion 1 (V3 0 0 0)
              , Quaternion 0 (V3 1 0 0)
              , Quaternion 0 (V3 0 1 0)
              , Quaternion 0 (V3 0 0 1)
              , Quaternion (-1) (V3 0 0 0)
              , Quaternion 0 (V3 (-1) 0 0)
              , Quaternion 0 (V3 0 (-1) 0)
              , Quaternion 0 (V3 0 0 (-1))
              ]
          | otherwise = return $ Quaternion (w/norm) (V3 (x/norm) (y/norm) (z/norm))
    ret

instance Arbitrary (V3 (V3 Double)) where
  arbitrary = dcmOfEuler321 <$> arbitrary

testConversion :: (F.Foldable f, Applicative f, Show (f Double))
                  => Double -> (f Double -> f Double) -> f Double
                  -> Property
testConversion eps f x0 = counterexample msg (close eps x0 x1)
  where
    msg = init $ unlines
          [ "original:  " ++ show x0
          , "converted: " ++ show x1
          ]
    x1 = f x0

prop_e2q2e :: Euler Double -> Property
prop_e2q2e = testConversion 1e-9 (euler321OfQuat . quatOfEuler321)

prop_e2d2e :: Euler Double -> Property
prop_e2d2e = testConversion 1e-9 (euler321OfDcm . dcmOfEuler321)

testDoubleConversion :: (Show f, Show g) => f -> g -> g -> Bool -> Property
testDoubleConversion orig res0 res1 ret = counterexample msg ret
  where
    msg = init $ unlines
          [ "original: " ++ show orig
          , "first route:  " ++ show res0
          , "second route: " ++ show res1
          ]

prop_e2d_e2q2d :: Euler Double -> Property
prop_e2d_e2q2d euler = testDoubleConversion euler dcm0 dcm1 (closeDcm 1e-9 dcm0 dcm1)
  where
    dcm0 = dcmOfEuler321 euler
    dcm1 = dcmOfQuat (quatOfEuler321 euler)

prop_e2q_e2d2q :: Euler Double -> Property
prop_e2q_e2d2q euler = testDoubleConversion euler quat0 quat1 (close 1e-9 quat0 quat1)
  where
    quat0 = quatOfEuler321 euler
    quat1 = quatOfDcm (dcmOfEuler321 euler)

prop_q2e_q2d2e :: Quaternion Double -> Property
prop_q2e_q2d2e quat = testDoubleConversion quat euler0 euler1 (close 1e-9 euler0 euler1)
  where
    euler0 = euler321OfQuat quat
    euler1 = euler321OfDcm (dcmOfQuat quat)

prop_q2d_q2e2d :: Quaternion Double -> Property
prop_q2d_q2e2d quat = testDoubleConversion quat dcm0 dcm1 (closeDcm 1e-9 dcm0 dcm1)
  where
    dcm0 = dcmOfQuat quat
    dcm1 = dcmOfEuler321 (euler321OfQuat quat)

prop_d2e_d2q2e :: M33 Double -> Property
prop_d2e_d2q2e dcm = testDoubleConversion dcm euler0 euler1 (close 1e-9 euler0 euler1)
  where
    euler0 = euler321OfDcm dcm
    euler1 = euler321OfQuat (quatOfDcm dcm)

prop_d2q_d2e2q :: M33 Double -> Property
prop_d2q_d2e2q dcm = testDoubleConversion dcm quat0 quat1 (close 1e-9 quat0 quat1)
  where
    quat0 = quatOfDcm dcm
    quat1 = quatOfEuler321 (euler321OfDcm dcm)

tests :: [Test]
tests =
  [ testGroup "inverses"
    [ testProperty "(euler -> quat -> euler) == euler" prop_e2q2e
    , testProperty "(euler -> dcm -> euler) == euler" prop_e2d2e
    ]
  , testGroup "two routes"
    [ testProperty "(euler -> dcm) == (euler -> quat -> dcm)" prop_e2d_e2q2d
    , testProperty "(euler -> quat) == (euler -> dcm -> quat)" prop_e2q_e2d2q
    , testProperty "(quat -> euler) == (quat -> dcm -> euler)" prop_q2e_q2d2e
    , testProperty "(quat -> dcm) == (quat -> euler -> dcm)" prop_q2d_q2e2d
    , testProperty "(dcm -> euler) == (dcm -> quat -> euler)" prop_d2e_d2q2e
    , testProperty "(dcm -> quat) == (dcm -> euler -> quat)" prop_d2q_d2e2q
    ]
  ]

opts :: RunnerOptions' Maybe
opts =
  Mo.mempty
  { ropt_color_mode = Just ColorAlways
  , ropt_threads = Just 1
  , ropt_test_options = Just my_test_opts
  }

my_test_opts :: TestOptions' Maybe
my_test_opts =
  Mo.mempty
  { topt_timeout = Just (Just 15000000)
  }
