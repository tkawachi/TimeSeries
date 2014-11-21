module TimeSeries.MA where

import           Safe.Exact (takeExactMay)

class MA a where
    ma :: a -> [Double] -> [Maybe Double]

-- | Simple Moving Average
data SMA = SMA { smaShift :: Int }

instance MA SMA where
    ma sma xs = take (length xs) (shiftRight shift (ma' xs))
        where
            shift = smaShift sma
            ma' [] = []
            ma' (a @ (_ : xs')) =  ((takeExactMay shift a) >>= average) : ma' xs'

data EMA = EMA {}

-- | Average
average :: (Num a, Fractional a) => [a] -> Maybe a
average [] = Nothing
average xs = Just $ sum xs / fromIntegral (length xs)

shiftRight :: Int -> [Maybe Double] -> [Maybe Double]
shiftRight n xs
    | n <= 0 = xs
    | otherwise = Nothing : shiftRight (n - 1) xs

main :: IO ()
main = print $ take 10 $ ma (SMA 5) [1..1000]
