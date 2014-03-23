module Rate
    ( Rate
    , mkRate
    , updateBytes
    , extractRate
    , extractCount
    ) where

import Data.Time.Clock


data Rate = Rate
    { _rRate  :: Double      -- ^ Текущая скорость
    , _rBytes :: Integer     -- ^ Кол-во байт переданных за минимальный период
    , _rCount :: Integer     -- ^ Кол-во байт переданных с момета последнего обнуления
    , _rSince :: UTCTime     -- ^ Момент начала отсчета скорости
    , _rLast  :: UTCTime     -- ^ Момент последнего обновления скорости
    }


-- Минимальное промежуток времени, за который считается средняя скорость
minRatePeriod :: NominalDiffTime
minRatePeriod = fromInteger 20 -- Секунды


mkRate :: UTCTime -> Rate
mkRate timestamp = Rate
    { _rRate  = 0
    , _rBytes = 0
    , _rCount = 0
    , _rSince = timestamp
    , _rLast  = timestamp
    }


updateBytes :: Integer -> Rate -> Rate
updateBytes numBytes rate = rate
    { _rBytes = _rBytes rate + numBytes
    , _rCount = _rCount rate + numBytes
    }


extractRate :: UTCTime -> Rate -> (Double, Rate)
extractRate timestamp rate = let
    last' = _rLast rate
    since = _rSince rate
    bytes = _rBytes rate
    oldPeriod = realToFrac $ diffUTCTime last' since
    newPeriod = realToFrac $ diffUTCTime timestamp since
    rateNum = (_rRate rate * oldPeriod + fromIntegral bytes) / newPeriod
    newRate = rate
        { _rRate  = rateNum
        , _rBytes = 0
        , _rLast  = timestamp
        , _rSince = max since (addUTCTime (-minRatePeriod) timestamp)
        }
    in (rateNum, newRate)


extractCount :: Rate -> (Integer, Rate)
extractCount rate = (count, rate { _rCount = 0 })
  where
    count = _rCount rate


