# OCaml-Poker

OCaml-Poker is a terminal-based multiplayer poker project written in OCaml. It
includes a terminal UI, full poker betting and winner logic, and (coming soon) bot players.

## Running the project

The Dune project root is in `poker/`, not at the repository root.

From the repository root, run commands with `--root poker`:

```sh
dune exec --root poker ./bin/server.exe -- 9000
dune exec --root poker ./bin/client.exe -- 127.0.0.1 9000
```

Or change into the project directory first:

```sh
cd poker
dune exec ./bin/server.exe -- 9000
dune exec ./bin/client.exe -- 127.0.0.1 9000
```

## Team members

- Jackson Liao-Cheng (jwl323)
- Ethan Ngai (emn65)
- JD Krasnick (jdk342)
- Andy Do (abd228)
