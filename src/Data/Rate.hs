module Data.Rate
    ( Rate
    , mkRate
    , updateBytes
    , extractRate
    ) where

import Data.Time.Clock


data Rate = Rate
    { _rRate  :: Double      -- ^ Текущая скорость
    , _rCount :: Integer     -- ^ Кол-во байт переданных с момета последнего обнуления
    , _rSince :: UTCTime     -- ^ Момент начала отсчета скорости
    , _rLast  :: UTCTime     -- ^ Момент последнего обновления скорости
    }

mkRate :: UTCTime -> Rate
mkRate timestamp = Rate
    { _rRate  = 0
    , _rCount = 0
    , _rSince = timestamp
    , _rLast  = timestamp
    }

-- Минимальное промежуток времени, за который считается средняя скорость
minRatePeriod :: NominalDiffTime
minRatePeriod = fromInteger 20 -- Секунды

updateBytes :: Integer -> Rate -> Rate
updateBytes numBytes rate = rate { _rCount = _rCount rate + numBytes }

extractRate :: UTCTime -> Rate -> (Integer, Double, Rate)
extractRate timestamp rate =
    let last' = _rLast rate
        since = _rSince rate
        bytes = _rCount rate
        oldPeriod = realToFrac $ diffUTCTime last' since
        newPeriod = realToFrac $ diffUTCTime timestamp since
        rateNum = (_rRate rate * oldPeriod + fromIntegral bytes) / newPeriod
        newRate = rate
            { _rRate  = rateNum
            , _rCount = 0
            , _rLast  = timestamp
            , _rSince = max since (addUTCTime (-minRatePeriod) timestamp)
            }
        in (bytes, rateNum, newRate)
