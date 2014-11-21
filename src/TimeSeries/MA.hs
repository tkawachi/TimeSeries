module TimeSeries.MA where

import           Safe.Exact (takeExactMay)

-- | Moving Average
class MA a where
    ma :: a -> [Double] -> [Maybe Double]

-- | Simple Moving Average
data SMA = SMA { smaShift :: Int }

instance MA SMA where
    ma sma xs = reverse . ma' $ reverse xs
        where
            shift = smaShift sma
            ma' [] = []
            ma' (a @ (_ : xs')) =  ((takeExactMay shift a) >>= average) : ma' xs'

-- | Average
average :: (Num a, Fractional a) => [a] -> Maybe a
average [] = Nothing
average xs = Just $ sum xs / fromIntegral (length xs)

main :: IO ()
main = print $ ma (SMA 5) [1..10]
