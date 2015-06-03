module FlashBit.Tracker.Chan
    ( TrackerMessage(..)
    ) where

import Control.Concurrent

data TrackerMessage
    = Stop               -- ^ Сообщить трекеру об остановке скачивания
    | Start              -- ^ Сообщить трекеру о начале скачивания
    | Complete           -- ^ Сообщить трекеру об окончании скачивания
    | Tick Integer       -- ^ Новый раунд
    | Shutdown (MVar ())
