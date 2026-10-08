# reprise

[![CI](https://github.com/arybczak/reprise/actions/workflows/haskell-gha.yml/badge.svg)](https://github.com/arybczak/reprise/actions/workflows/haskell-gha.yml)

A terminal client for the [Music Player Daemon](https://www.musicpd.org)
(MPD), meant to succeed [ncmpcpp](https://github.com/ncmpcpp/ncmpcpp). It
follows ncmpcpp's screens and keys, and adds key sequences with a panel that
shows what comes next, synced lyrics, and a visualizer that blends its
colors.

reprise is in early development. It already serves for everyday listening;
the search engine and the media library come later.

## Features

- **The queue**, as columns or as a line for each song, in any format.
  Select songs, move them up and down, to the cursor, to the beginning, to
  the end or after the playing song, set their priority, shuffle them, and
  save them as a stored playlist.
- **The browser** of MPD's database, sorted by type, name, modification
  time or a format. It opens stored playlists and playlist files such as cue
  sheets, and adds, plays or saves what is selected.
- **Lyrics** from [LRCLIB](https://lrclib.net) and
  [tekstowo.pl](https://www.tekstowo.pl), stored for later. Synced lyrics
  follow the song line by line. Edit them in your editor, with or without
  times.
- **The visualizer:** a spectrum, a stereo ellipse, or the wave of each
  channel, which stands still for a steady sound.
- **The outputs** of MPD, switched on and off.
- **The song info,** with its tags, its audio and its ReplayGain.
- **Find** in every list, with regular expressions that ignore case and
  diacritics.
- **Key sequences,** e.g. `t r` to toggle repeat, with a panel that lists the
  keys that can follow. `f1` lists every key of every screen.
- **One configuration file** in YAML, with styles, formats and key bindings.

## Installing

reprise needs:

- [GHC](https://www.haskell.org/ghcup/) 9.6 or newer and cabal;
- the [ICU](https://icu.unicode.org) libraries and headers, found through
  pkg-config, e.g. `libicu-dev` on Debian and Ubuntu, or `icu` on Arch;
- MPD 0.24 or newer to connect to.

```
cabal install
```

installs the `reprise` executable into cabal's `installdir`, by default
`~/.local/bin`.

## Getting started

### Connecting to MPD

`reprise` finds MPD in this order: `--host` and `--port`, `mpd.host` and
`mpd.port` in the configuration, `$MPD_HOST` and `$MPD_PORT`, the sockets
`$XDG_RUNTIME_DIR/mpd/socket` and `/run/mpd/socket`, and `localhost:6600`. A
host can be a socket path, and `password@host` gives the password too.

```
reprise --host /run/user/1000/mpd/socket
```

### Screens

| Key | Screen |
|---|---|
| `1` | the queue |
| `2` | the browser |
| `7` | the outputs |
| `8` | the visualizer |
| `l` | the lyrics of the song under the cursor |
| `i` | the info of the song under the cursor |
| `f1` | the help: every key of every screen |
| `tab`, `shift-tab` | the next and the previous of the screens with numbers |

`escape` goes back from the lyrics, the song info and the help. `q` quits,
and so does `ctrl-q`, from anywhere.

### Playing

`p` pauses and resumes, `s` stops, `<` and `>` go to the previous and the
next song. `f` and `b` seek; hold them to go further. `+` and `-`, or the
arrows `right` and `left`, change the volume. `enter` plays the song under
the cursor.

### Adding music

In the browser, `enter` enters a directory and plays a song, and
`backspace` goes up. `space` adds what is under the cursor to the queue, or
removes it if it is there already. The group `a` adds the selection at a
place: `a e` at the end, `a b` at the beginning, `a n` after the playing
song, and `a p` adds and plays it.

### Selecting and editing the queue

`insert` selects the song under the cursor, `shift-up` and `shift-down`
select while moving, and in the queue `space` selects too. `V` clears the
selection. The group `v` selects more: `v r` a range, `v a` the album, and
`v i` the opposite. An action acts on the selection, or on the song under
the cursor without one.

In the queue, `delete` removes songs, `m` and `n` move them up and down,
and `M` moves the selection above the cursor. The group `e` edits:

- `e c` clears the queue, after asking, and `e s` shuffles it;
- `e w` saves the queue, or the selection, as a stored playlist;
- `e m e`, `e m b` and `e m n` move songs to the end, to the beginning, or
  after the playing song;
- `e p` sets the priority of songs, which MPD plays first in random mode.

`o` moves the cursor to the playing song, and `g b` shows the song under the
cursor in the browser.

### Finding

`/` finds forward and `?` backward, with a regular expression that ignores
case and diacritics. `.` and `,` go to the next and the previous match.

### Toggles

The group `t` toggles MPD's options and reprise's own: `t r` repeat, `t z`
random, `t s` single, `t c` consume, `t x` crossfade, `t d` the display of
the queue or the browser, and in the queue `t f` moves the cursor to each
song that plays.

### Running an action by its name

`:` asks for an action and its arguments, e.g. `volume 50`, `seek 1:30` or
`add_path some/directory`, and shows what an action takes while you type
it. [doc/config.yaml](doc/config.yaml) shows the action of every key.

### Lyrics

`l` shows the lyrics of the song under the cursor. reprise looks for them in
its directory, `~/.local/share/reprise/lyrics`, and then asks LRCLIB and
tekstowo.pl, and stores what it finds. Synced lyrics highlight the line
that is sung. On the lyrics screen, `space` follows the song that plays,
`` ` `` fetches the lyrics again, and `e e` edits them in `$VISUAL` or
`$EDITOR`.

### The visualizer

The visualizer reads the samples that MPD plays from a fifo output. Add one
to `mpd.conf`:

```
audio_output {
    type    "fifo"
    name    "visualizer"
    path    "/tmp/mpd.fifo"
    format  "44100:16:2"
}
```

and tell reprise where it is:

```yaml
visualizer:
  data_source: /tmp/mpd.fifo
```

`8` shows the visualizer, and `space` goes through the spectrum, the
ellipse and the wave.

## Configuration

reprise reads `~/.config/reprise/config.yaml`
(`$XDG_CONFIG_HOME/reprise/config.yaml`), or the file that `--config` names.
The file lists only what you change. [doc/config.yaml](doc/config.yaml)
lists every option with its default value and what it does; copy from it
what you want to change, not all of it, so that your file keeps getting
the defaults of later versions.

```yaml
queue:
  display: classic               # one line for each song instead of columns

lists:
  cursor_style: black on yellow

keys:
  global:
    x: toggle random             # a new key
    p: ~                         # no key for pause
    t:
      y: toggle single           # another key in the group of t
  queue:
    D: delete                    # a key of the queue only
```

A key in your file replaces the default binding of that key, a group adds
to the default group, and `~` removes a binding or a whole group.

reprise writes a log to `~/.local/state/reprise/reprise.log`
(`$XDG_STATE_HOME/reprise/reprise.log`), e.g. of a lyrics fetcher that
failed.

## Coming from ncmpcpp

[doc/ncmpcpp.md](doc/ncmpcpp.md) lists the keys that moved and what each
ncmpcpp option became.

## Development

```
cabal build all
```

The test suites need `mpd` and `flac` on the `PATH`:

```
cabal test all
```

The benchmarks measure what was slow once:

```
cabal bench all
```

## License

BSD-3-Clause. See [LICENSE](LICENSE).
