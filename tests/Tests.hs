{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Main ( main ) where

import qualified Data.Foldable as F
import Data.Functor.Compose ( Compose(..) )
import qualified Data.Monoid as Mo
import Test.Framework
  ( Test, ColorMode(..), RunnerOptions'(..), TestOptions'(..)
  , defaultMainWithOpts, testGroup )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck
import Text.Printf ( printf )

import SpatialMath
  ( Dcm(..), Euler(..), Quat(..), Quaternion(..), V3T(..), V3(..)
  , euler321OfQuat
  , euler321OfDcm
  , quatOfEuler321
  , dcmOfQuat
  , dcmOfEuler321
  , quatOfDcm
  )


main :: IO ()
main = defaultMainWithOpts tests opts

closeEuler :: forall f g . Double -> Euler f g Double -> Euler f g Double -> Maybe Double
closeEuler eps f0 f1
  | all (\x -> abs x <= eps) deltas = Nothing
  | otherwise = Just $ maximum $ map abs deltas
  where
    delta :: Euler f g Double
    delta = (-) <$> f0 <*> f1

    deltas = F.toList delta

closeQuat :: forall f g . Double -> Quat f g Double -> Quat f g Double -> Maybe Double
closeQuat eps f0 f1
  | worstDelta <= eps = Nothing
  | otherwise = Just worstDelta
  where
    deltas0 :: Quat f g Double
    deltas0 = (-) <$> f0 <*> f1

    deltas1 :: Quat f g Double
    deltas1 = (-) <$> f0 <*> (negate <$> f1)

    worstDelta =
      min
      (maximum (map abs (F.toList deltas0)))
      (maximum (map abs (F.toList deltas1)))

closeDcm :: forall f g . Double -> Dcm f g Double -> Dcm f g Double -> Maybe Double
closeDcm eps (DcmUnitVectors f0) (DcmUnitVectors f1)
  | all (\x -> abs x <= eps) deltas = Nothing
  | otherwise = Just $ maximum $ map abs deltas
  where
    delta :: V3T g (V3T f Double)
    Compose delta = (-) <$> Compose f0 <*> Compose f1

    deltas = F.toList (Compose delta)

instance Arbitrary (Euler f g Double) where
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

instance Arbitrary (Quat f g Double) where
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
    Quat <$> ret

instance Arbitrary (Dcm f g Double) where
  arbitrary = dcmOfEuler321 <$> arbitrary

testConversion :: (Show a, Show b)
                  => (b -> b -> Maybe Double)
                  -> (a -> b) -> (a -> b) -> a
                  -> Property
testConversion toErr f0 f1 x = counterexample msg ret
  where
    y0 = f0 x
    y1 = f1 x
    (ret, errmsg) = case toErr y0 y1 of
      Nothing -> (True, [])
      Just worstErr -> (False, [printf "worst error: %.3g" worstErr])
    msg = init $ unlines $
          [ "original:  " ++ show x
          , "first route:  " ++ show y0
          , "second route: " ++ show y1
          ] ++ errmsg

-- inverses
prop_e2q2e :: Euler f g Double -> Property
prop_e2q2e = testConversion (closeEuler 1e-9) id (euler321OfQuat . quatOfEuler321)

prop_e2d2e :: Euler f g Double -> Property
prop_e2d2e = testConversion (closeEuler 1e-9) id (euler321OfDcm . dcmOfEuler321)

prop_d2e2d :: Dcm f g Double -> Property
prop_d2e2d = testConversion (closeDcm 1e-9) id (dcmOfEuler321 . euler321OfDcm)

prop_d2q2d :: Dcm f g Double -> Property
prop_d2q2d = testConversion (closeDcm 1e-9) id (dcmOfQuat . quatOfDcm)

prop_q2e2q :: Quat f g Double -> Property
prop_q2e2q = testConversion (closeQuat 1e-9) id (quatOfEuler321 . euler321OfQuat)

prop_q2d2q :: Quat f g Double -> Property
prop_q2d2q = testConversion (closeQuat 1e-9) id (quatOfDcm . dcmOfQuat)

-- two routes
prop_e2d_e2q2d :: Euler f g Double -> Property
prop_e2d_e2q2d = testConversion (closeDcm 1e-9) dcmOfEuler321 (dcmOfQuat . quatOfEuler321)

prop_e2q_e2d2q :: Euler f g Double -> Property
prop_e2q_e2d2q =
  testConversion (closeQuat 1e-9) (makeScalarPositive . quatOfEuler321) (quatOfDcm . dcmOfEuler321)

prop_q2e_q2d2e :: Quat f g Double -> Property
prop_q2e_q2d2e = testConversion (closeEuler 1e-9) euler321OfQuat (euler321OfDcm . dcmOfQuat)

prop_q2d_q2e2d :: Quat f g Double -> Property
prop_q2d_q2e2d = testConversion (closeDcm 1e-9) dcmOfQuat (dcmOfEuler321 . euler321OfQuat)

prop_d2e_d2q2e :: Dcm f g Double -> Property
prop_d2e_d2q2e = testConversion (closeEuler 1e-7) euler321OfDcm (euler321OfQuat . quatOfDcm)

prop_d2q_d2e2q :: Dcm f g Double -> Property
prop_d2q_d2e2q = testConversion (closeQuat 1e-5) quatOfDcm (makeScalarPositive . quatOfEuler321 . euler321OfDcm)

makeScalarPositive :: Quat f g Double -> Quat f g Double
makeScalarPositive quat0'@(Quat (Quaternion q0 _))
  | q0 < 0 = fmap negate quat0'
  | otherwise = quat0'

tests :: [Test]
tests =
  [ testGroup "inverses"
    [ testProperty "euler == (euler -> quat  -> euler)" prop_e2q2e
    , testProperty "euler == (euler -> dcm   -> euler)" prop_e2d2e
    , testProperty "dcm   == (dcm   -> euler -> dcm  )" prop_d2e2d
    , testProperty "dcm   == (dcm   -> quat  -> dcm  )" prop_d2q2d
    , testProperty "quat  == (quat  -> euler -> quat )" prop_q2e2q
    , testProperty "quat  == (quat  -> dcm   -> quat )" prop_q2d2q
    ]
  , testGroup "two routes"
    [ testProperty "(euler -> dcm  ) == (euler -> quat  -> dcm  )" prop_e2d_e2q2d
    , testProperty "(euler -> quat ) == (euler -> dcm   -> quat )" prop_e2q_e2d2q
    , testProperty "(quat  -> euler) == (quat  -> dcm   -> euler)" prop_q2e_q2d2e
    , testProperty "(quat  -> dcm  ) == (quat  -> euler -> dcm  )" prop_q2d_q2e2d
    , testProperty "(dcm   -> euler) == (dcm   -> quat  -> euler)" prop_d2e_d2q2e
    , testProperty "(dcm   -> quat ) == (dcm   -> euler -> quat )" prop_d2q_d2e2q
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
  , topt_maximum_generated_tests = Just 1000
  }
