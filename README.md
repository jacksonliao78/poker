# OCaml-Poker

OCaml-Poker is a terminal-based multiplayer poker project written in OCaml. It
includes a terminal UI, full poker betting and winner logic, and multi-game sessions.

## Running the project

The Dune project root is in `poker/`, not at the repository root.

To run the poker server/client: 

```sh
cd poker
dune exec ./bin/server.exe <port>
dune exec ./bin/client.exe <ip_addr> <port>
```

## Team members

- Jackson Liao-Cheng (jwl323)
- Ethan Ngai (emn65)
- JD Krasnick (jdk342)
- Andy Do (abd228)
