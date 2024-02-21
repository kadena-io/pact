;; Test hyperlane builtins.

(expect "computes the correct message id" "0x97d98aa7fdb548f43c9be37aaea33fca79680247eb8396148f1df10e6e0adfb7" (hyperlane-message-id {"destinationDomain": 1,"nonce": 325,"originDomain": 626,"recipient": "0x71C7656EC7ab88b098defB751B7401B5f6d8976F","sender": "0x6b622d746f6b656e2d726f75746572","tokenMessage": {"amount": 10000000000000000000.0,"recipient": "0x71C7656EC7ab88b098defB751B7401B5f6d8976F"},"version": 1}))
