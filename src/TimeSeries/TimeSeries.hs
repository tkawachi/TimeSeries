module TimeSeries.TimeSeries where

import Data.Time.Clock (UTCTime)

data TimeSeries = TimeSeries {
    tsOpen :: Double
    , tsClose :: Double
    , tsLow :: Double
    , tsHigh :: Double
    , tsVolume :: Double
    , tsTime :: UTCTime
}

class HasOpen a where
    open :: a -> Double

instance HasOpen TimeSeries where
    open = tsOpen

