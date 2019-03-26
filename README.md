# blockchain

Simple client-server blockchain implementation in Haskell. Features transaction signing, block validation, proof-of-work, reward management and wallet control. Uses [hactors](http://hackage.haskell.org/package/hactors-0.0.3.1) library for concurrent programming, [RSA](http://hackage.haskell.org/package/RSA-2.3.1) for security and [hashable-generics](https://hackage.haskell.org/package/hashable-generics-1.1.3) for generic hashing functions.

## Setup

First of all you will need to install `stack` tool. You may do it using your favourite package manager or by running `curl -sSL https://get.haskellstack.org/ | sh` (bad habit tho).

Next, `cd` into project dir and run
```
stack build
```

## Testing

After fighting off all dependency issues and reading all the warnings you may in the splendor of glory run
```
stack ghci
```
The REPL should appear.

Create two users (their private&public key pair):

```
radekkp <- newKeyPair
(_, michalpub) <- newKeyPair
```

Create server actor (the blockchain is not distributed yet, sorry :( ):
```
serv <- runServer
```
Create miner connected to the server that will use radek's key pair:
```
m <- runMiner radekkp serv
```
...and watch it mine! The server and the client will log some events. To kill the party you may just shut down the `ghci`, or send `ServerStop` and `ClientStop` to the actors accordingly.

You may manually add new transactions. To make radek pay michal 30 radcoins do:
```
serv ! PushTransaction (makeTransaction radekkp michalpub 30)
```
Note that you may need to `import Control.Concurrent.Actor` to use `!` operator.
