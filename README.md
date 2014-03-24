FlashBit
========

= TODO
[ ] - FS.openAndCheckFile
        небезопасный pattern-matching
[ ] - Tracker
        в случае ошибки, отправить запрос трекеру об остановке
[ ] - Status
        зачем torrentManager'у обращается к status'у, чтобы узнать есть уже такой торрент или нет
        эта функция возложена не на тот процесс

= REFACTOR
[x] Status
[x] Listen
[x] Console
[x] Channel
[x] FileAgent
[x] Tracker
[ ] Peer
[ ] Peer.Sender
[ ] Peer.SenderQ
[ ] Peer.Receiver
[ ] PeerManager
[ ] PieceManager
[ ] ChokeManager
[ ] TorrentManager
