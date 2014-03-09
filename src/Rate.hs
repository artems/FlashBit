module Rate
    ( Rate
    , mkRate
    , updateBytes
    , extractRate
    , extractCount
    ) where


import Data.Time.Clock


data Rate = Rate
    { rRate :: Double       -- ^ Текущая скорость
    , rBytes :: Integer     -- ^ Кол-во байт переданных за минимальный период
    , rCount :: Integer     -- ^ Кол-во байт переданных с момета последнего обнуления
    , rSince :: UTCTime     -- ^ Момент начала отсчета скорости
    , rLast :: UTCTime      -- ^ Момент последнего обновления скорости
    }


-- Минимальное промежуток времени, за который считается средняя скорость
minRatePeriod :: NominalDiffTime
minRatePeriod = fromInteger 20 -- Секунды


mkRate :: UTCTime -> Rate
mkRate timestamp = Rate
    { rRate = 0
    , rBytes = 0
    , rCount = 0
    , rSince = timestamp
    , rLast = timestamp
    }


updateBytes :: Integer -> Rate -> Rate
updateBytes numBytes rate = rate
    { rBytes = rBytes rate + numBytes
    , rCount = rCount rate + numBytes
    }


extractRate :: UTCTime -> Rate -> (Double, Rate)
extractRate timestamp rate = let
    last' = rLast rate
    since = rSince rate
    bytes = rBytes rate
    oldPeriod = realToFrac $ diffUTCTime last' since
    newPeriod = realToFrac $ diffUTCTime timestamp since
    rateNum = (rRate rate * oldPeriod + fromIntegral bytes) / newPeriod
    newRate = rate
        { rRate = rateNum
        , rBytes = 0
        , rLast = timestamp
        , rSince = max since (addUTCTime (-minRatePeriod) timestamp)
        }
    in (rateNum, newRate)


extractCount :: Rate -> (Integer, Rate)
extractCount rate = (count, rate { rCount = 0 })
  where
    count = rCount rate

