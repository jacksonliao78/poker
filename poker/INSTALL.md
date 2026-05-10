# Installation and Running

This project is an OCaml/Dune implementation of a multiplayer terminal poker
program. It has one server executable, one client executable.

## Required Dependencies

- `lwt`: cooperative threading and I/O library used by the server and client.
- `lwt_ppx`: syntax support for `let%lwt`.
- `ounit2`: test framework used by `test/test_poker.ml`.

## Build

```sh
dune build
```

This compiles the library, server, client, default executable, and tests.

## Run the Server

Start the server in one terminal:

```sh
dune exec ./bin/server.exe
```

The server listens on port `9000` by default. To choose a different port:

```sh
dune exec ./bin/server.exe <port>
```

Keep this process running while clients are connected.

## Run Clients

Open a separate terminal for each player. To connect to a local server on the
default port:

```sh
dune exec ./bin/client.exe
```

To connect to a specific host and port:

```sh
dune exec ./bin/client.exe <host_ip> <port>
```

The client prompts for a player name after connecting. In the client, plain text
sends chat messages. Commands include:

- `/start`: starts the game as the host
- `/name <new name>`: change your displayed player name.
- `/fold` or `/f`: fold when folding is legal.
- `/call` or `/c`: call when calling is legal.
- `/check` or `/x`: check when checking is legal.
- `/raise <amount>` or `/r <amount>`: raise by the given amount.
- `/pref recent show|hide|clear`: control the recent activity panel.
- `/quit`: disconnect from the server.

## Run Tests

```sh
dune runtest
```

This runs the OUnit test suite in `test/test_poker.ml`.
