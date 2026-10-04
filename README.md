# reprise

A terminal client for the [Music Player Daemon](https://www.musicpd.org)
(MPD).

reprise is in early development. [DESIGN.md](DESIGN.md) describes what it will
be and why.

## Packages

- [`reprise`](reprise): the client.
- [`mpd-protocol`](mpd-protocol): a library for the MPD protocol, usable
  without reprise.
- `mpd-test-server`: starts a real MPD for the test suites. It is not
  released.

## Building

reprise needs GHC 9.6 or newer, and MPD 0.23 or newer to connect to.

```
cabal build all
```

The test suites need `mpd` and `flac` on the `PATH`:

```
cabal test all
```
