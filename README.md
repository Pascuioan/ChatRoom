
# ChatRoom

A simple chat room application with a terminal user interface written in Haskell using the [Brick library](https://hackage.haskell.org/package/brick). You have the option to either host a server or join one using an IP and a port. Inside the application you can see all connected users using the `:con` command and scroll using `Ctrl+Up/Down`. 

### Installation

Download the files by using `git clone https://github.com/Pascuioan/ChatRoom` or by downloading the [source code](https://github.com/Pascuioan/ChatRoom/archive/refs/tags/v1.0.0.zip) and then open the application using `cabal run`(you must have the [Haskell Toolchain](https://www.haskell.org/downloads/) installed).

### Shortcomings

This project was for fun so little safety precautions were taken:
- The messages are not encrypted so maybe don't exchange your deepest secrets using this application
- There is no limit to the number of connections the server will allow
- The server filters closed connections only after it recieves a message so sometimes `:con` might display the users incorecctly
- The application relies on the OS for socket clean-up
- The app was only (briefly) tested on Windows 11 and Ubuntu
