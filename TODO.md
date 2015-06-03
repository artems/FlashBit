[x] Main
[x] Version
    [x] Listen
    [x] Console
    [ ] PeerManager
        [ ] Peer
            [ ] PeerMain
            [ ] PeerSender
            [ ] PeerReceiver
    [x] TorrentManager
        [x] Tracker
        [x] FileAgent
        [x] PieceManager
        [ ] ChokeManager
    [x] TorrentDatabase
    [x] TorrentThreadDatabase


Peer
    - хранить "их" `peer id` и `capabilities`

Peer.Main
    - endgame
        - отмена блоков, скачанных другими пирами
    - при отключении пира, он не посылает сообщение PeerUnhave, PeerPutBackBlock

PieceManager
    - нет логики про заверение скачивания и переход в режим сидирования
