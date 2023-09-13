# Ca-tty, an IRC client as an unikernel

`catty` is an unikernel which launches an SSH server with an IRC client. The
goal of this unikernel is to provide an easy way to deploy and run an IRC client
and let the user to _idle_ in IRC with the smallest resource as he/she can have.

The project is experimental.

## How to test it?

Currently, we provide also a simple binary which can be used as an IRC client:
```shell-session
$ git clone https://github.com/robur-coop/catty.git
$ cd catty
$ opam pin add -yn .
$ opam install --deps-only catty
$ dune exec bin/catty.exe -- \
  --nickname dinosaure --nickname dinosaure1 --nickname dinosaure2 \
  --realname "Romain Calascibetta"
```

The executable produces a `log.txt` to help to debug.

## How to use the interface?

The UI is like a Vim interface:
- you have a _mode_
  1) the normal mode (the default one)
  2) the insertion mode (from the normal mode, you can tap `i`)
  3) the command mode (from the normal mode, you can tap `:`)
  4) you can come back to the normal mode with Esc

The normal mode let you to set the input. We advise the user to look into
`src/prompt.ml` to see modes and key bindings.

## Possible commands?

Currently, `catty` implements few commands:
- `connect <server>` to connect to an IRC server via TLS
- `whoami [<server>]` to know which nickname you got
- `join [<server>] <channel>` to join a channel

A basic usage will be:
1) `:`, `connect irc.libera.chat`
2) `join #mirage`
3) Esc, `i`, tap your message 

## How to participate?

The most interesting way to participate is to try it and ask to implement
features or implement by yourself what you want and propose a PR!

Currently, we use:
- `cri` which is a basic implementation of the IRC protocol
  https://github.com/dinosaure/cri
- a fork of `lwd` and `nottui` from this PR:
  https://github.com/let-def/lwd/pull/44
- a unreleased version of `art`
  https://github.com/dinosaure/art

## GitHub mirror

Currently, the GitHub repository is a mirror to
https://git.robur.coop/robur/catty. You can make a pull-request on GitHub or
directly on https://git.robur.coop/ if you want.
