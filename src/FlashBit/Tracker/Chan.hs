module FlashBit.Tracker.Chan
    ( TrackerMessage(..)
    ) where

data TrackerMessage
    = Stop         -- ^ Сообщить трекеру об остановке скачивания
    | Start        -- ^ Сообщить трекеру о начале скачивания
    | Complete     -- ^ Сообщить трекеру об окончании скачивания
    | Tick Integer -- ^ Следующий раунд
