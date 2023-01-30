# What is cl-web3?


cl-web3 is general web3 rpc access client by common lisp; Applicable to all evm compatible blockchains.

[sourceCode](https://github.com/Wilfrider/cl-web3.git).


## Licence

MIT.

## History


## How to pre config for debian?

    apt install libcurl4-gnutls-dev

    git clone -b main --depth 1 https://github.com/Wilfrider/cl-web3.git
    pushd ./DependLibs
        bash CreateLib.sh
    popd

    (push "cl-web3 Absolute path" asdf:*central-registry*)
    (ql:quickload "cl-web3")

## How to usage
[ref](https://github.com/Wilfrider/cl-web3/blob/main/example/web3Example.lisp).

## Implemented JSON-RPC methods
-   `eth_gasPrice`
-   `eth_blockNumber`
-   `eth_getTransactionCount`
-   `eth_call`
-   `eth_getBalance`
-   `eth_sendRawTransaction`
