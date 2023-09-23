# Mau-Mau game web sockets server

This is a web sockets server for managing multi-player matches of the [Czech version](https://en.wikipedia.org/wiki/Mau-Mau_(card_game)#Czech_Republic) of the [Mau-Mau game](https://en.wikipedia.org/wiki/Mau-Mau_(card_game)).

It supports:
* multiple parallel games
* log-in with user name only
* games are in-memory only so after restarting the server all games are gone

It is intended as a BE for the [mau-mau React app](https://github.com/juliamariachkina/mau-mau). Its main purpose is for me to get more experience with Haskell and try new concepts (e.g. creating web socket servers).

# Getting Started

To run the server you need to:
1. [Install Stack](https://docs.haskellstack.org/en/stable/#how-to-install-stack)
1. Run the `stack run` command
The command will install all the necessary tooling (including `ghc`) and run the web sockets server. You can connect to it on a port that will be printed to the console right after the server is started.
1. Run a compatible client app or [get started implementing your own client](#implementing-a-client).

# Implementing a client

To get more information on how the server communicates with clients [see docs/API.md](docs/API.md) for an overview of important state transitions from the client perspective.
You can also find there details about each message exchanged between the client and the server

# Feedback

If you have any feedback regarding this application or its documentation, please, submit an issue here or drop me a mail at `lojdaj@seznam.cz`.
