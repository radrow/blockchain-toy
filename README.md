# blockchain

## Setup

First of all you will need to install `stack` tool. Do it using your favourite package manager.

Then `cd` into project dir and run
```
stack build
```

## Testing

After fighting off all dependency issues and reading all the warnings you may in the splendor of glory run
```
stack ghci
```
The REPL should appear.

Create server actor (it is not yet distributed blockchain, sorry :( )
```
serv <- runServer
```
Create client connected to the server:
```
cl <- runClient serv
```
...and watch it mine! The server and the client will log some events. To kill the party you may just shut down the `ghci`, or send `ServerStop` and `ClientStop` to the actors accordingly.
