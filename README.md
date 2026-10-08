# reprise

A terminal client for the [Music Player Daemon](https://www.musicpd.org)
(MPD).

reprise is in early development. [DESIGN.md](DESIGN.md) describes what it will
be and why.

It reads its configuration from `$XDG_CONFIG_HOME/reprise/config.yaml`.

## Building

reprise needs GHC 9.6 or newer, and MPD 0.24 or newer to connect to.

```
cabal build all
```

The test suites need `mpd` and `flac` on the `PATH`:

```
cabal test all
```

The benchmarks measure what was slow once. [DESIGN.md](DESIGN.md#benchmarks)
describes how to compare them before and after a change:

```
cabal bench all
```
