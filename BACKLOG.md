  * use config
    * local port
    * download folder
  * graceful shutdown
    * close listen socket
    * close all peers sockets
  * Обработать исключения при обращению к трекеру
  * remove torrent
  * Переместить старт группы в апи
  * Переместить процесс с консолью в клиент
  * Показывать % скачанного в консоле
  * blacklisting slow peers
  * Избавиться от "холодного старта"
        Закачка не начинается до первого choke-раунда
        Имеет смысл подключить несколько пиров с трекера сразу же
