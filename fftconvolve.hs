import Data.List(genericLength)
import Data.Complex(Complex((:+)), cis, realPart)
import Control.Monad(replicateM)
import Control.Arrow((***))
import Control.Applicative((<$>))
import Test.QuickCheck(arbitrary, Gen)

-- naiveDftH :: RealFloat a => (a -> a) -> [Complex a] -> [Complex a]
-- naiveDftH f xs =
--   [ sum (zipWith rotate angles xs)
--   | i <- take l $ iterate (+1) 0
--   , let jump = f (i * 2*pi / fromIntegral l)
--         angles = iterate (+ jump) 0 ]
--   where
--     l = length xs
--     rotate angle num = cis angle * num

-- naiveDft :: RealFloat a => [Complex a] -> [Complex a]
-- naiveDft = naiveDftH id

-- naiveIDft :: RealFloat a => [Complex a] -> [Complex a]
-- naiveIDft xs = map (/genericLength xs) $
--                naiveDftH negate xs

fftH :: RealFloat a => (a -> a) -> [Complex a] -> [Complex a]
fftH _ [] = []
fftH _ [x] = [x]
fftH f xs = zipWith (+) fEvens fOdds ++
            zipWith (-) fEvens fOdds
  where
    baseJump = cis . f $ 2*pi / fromIntegral l
    jumps = iterate (*baseJump) 1
    l = length xs
    fixOdds = zipWith (*) jumps
    (fEvens, fOdds) = fftH f *** fixOdds . fftH f $ evensOdds xs

    evensOdds [] = ([], [])
    evensOdds [_] = error "FFT must be given power of 2 elements"
    evensOdds (x:y:pairs) = (x : evens, y : odds)
      where
        (evens, odds) = evensOdds pairs

fft :: RealFloat a => [Complex a] -> [Complex a]
fft = fftH id

ifft :: RealFloat a => [Complex a] -> [Complex a]
ifft xs = map (/genericLength xs) $ fftH negate xs

prop_fft_ifft :: [Complex Double] -> Bool
prop_fft_ifft xs =
  all (< 0.01) .
  map (realPart . abs) $
  zipWith (-) ((ifft . fft) xs) xs

prop_fft_ifft64 :: Gen Bool
prop_fft_ifft64 = prop_fft_ifft <$> replicateM 64 arbitrary

isInt :: Int -> Int
isInt = id
isDouble :: Double -> Double
isDouble = id

higherPower :: Int -> Int -> Int
higherPower radix =
  (radix^) . isInt . ceiling . isDouble .
  logBase (fromIntegral radix) . fromIntegral

pad :: Int -> a -> [a] -> [a]
pad count x xs = xs ++ replicate (count - length xs) x

convolve :: [Complex Double] -> [Complex Double] -> [Complex Double]
convolve x y = ifft $ zipWith (*) (fft x') (fft y')
  where
    l = (2*) . higherPower 2 $ max (length x) (length y)
    x' = pad l 0 $ x
    y' = pad l 0 $ y

intToComplex :: Int -> Complex Double
intToComplex x = fromIntegral x :+ 0

multiply :: Int -> [Int] -> [Int] -> [Int]
multiply base x y =
  reverse . dropWhile (0==) . carry base . reverse .
  map (round . realPart) $
  convolve (map intToComplex x)
           (map intToComplex y)

carry :: Int -> [Int] -> [Int]
carry base = carry' 0
  where
    carry' 0 [] = []
    carry' n [] = [n]
    carry' n (x:xs) = x' `mod` base : carry' (x' `div` base) xs
      where
        x' = n+x

-- > concatMap show $ multiply 10 [1,2,1,5,6,7,2,1,6,9,3,5,2] [1,2]
-- "14588066032224"

-- > 1215672169352 * 12
-- 14588066032224