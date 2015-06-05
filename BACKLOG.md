### HIGH
  * timeout for connect to peer

### MEDIUM
  * endgame
    * отмена блоков, скачанных другими пирами

### LOW
  * mkPeerId: use not only uppercase chars.
  * blacklisting slow peers
  * хранить "их" `peer id` и `capabilities`
  * Сократить "холодный старт".
    Закачка не начинается до первого choke-раунда.
    Имеет смысл подключить несколько пиров с трекера сразу же
  * Использовать MonadThrow вместо Maybe, когда обрабатывается торрент файл.
    Чтобы можно было понять, в каком месте произошла ошибка, при разрборе файла.
