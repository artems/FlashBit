[ ] Main
[ ] Version
    [ ] Listen
    [ ] Console
    [ ] PeerManager
        [ ] Peer
            [ ] PeerMain
            [ ] PeerSender
            [ ] PeerReceiver
    [ ] TorrentManager
        [ ] Tracker
        [ ] FileAgent
        [ ] PieceManager
        [ ] ChokeManager
    [ ] TorrentDatabase
    [ ] TorrentThreadDatabase

### Settings
bind-address-ipv4
bind-address-ipv6
cache-size-mb
dht-enabled
download-dir
incomplete-dir
open-file-limit: 32
peer-limit-global: 200
peer-limit-per-torrent 60
peer-port: 11523
peer-port-random-high 65535
peer-port-random-low 49152
peer-port-random-on-start false
start-added-torrents true,
umask: 0
upload-slots-per-torrent 14
