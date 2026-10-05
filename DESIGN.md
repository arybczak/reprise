# reprise: design

reprise is a terminal client for MPD, written in Haskell. It draws on
[ncmpcpp](https://github.com/ncmpcpp/ncmpcpp), which the same author started
about 15 years ago in C++.

This document records the decisions made so far and why. Keep it updated when
a decision changes.

## Contents

1. [Background](#background)
2. [Principles](#principles)
3. [Terms](#terms)
4. [Features](#features)
5. [Architecture](#architecture)
6. [User interface](#user-interface)
7. [Format language](#format-language)
8. [Configuration](#configuration)
9. [Testing](#testing)
10. [Project structure and conventions](#project-structure-and-conventions)
11. [Milestones](#milestones)
12. [Postponed decisions](#postponed-decisions)

## Background

ncmpcpp is still in daily use, but it has become tiring to maintain:
- It has no tests, so any change can bring back an old bug.
- Its design relies on global mutable state: screen singletons, the MPD
  connection, a config that toggles mutate at run time.
- Several features grew by special cases rather than design: binding chains,
  split screens, the format language.
- It has too many config options, and many of them let users make bad
  choices.

reprise is not a line-by-line port. It is a new client that the author wants to
use every day. It keeps ncmpcpp's good ideas and drops the old design and
anything the author doesn't use.

The code is written from scratch, not translated from ncmpcpp. reprise uses
the BSD-3-Clause license, and ncmpcpp is GPL-2.0-or-later with code from other
contributors.

## Principles

- **Tests from the first commit.** Most of the code is pure and tested that
  way. Every bug fix starts with a test that reproduces the bug (see
  [Testing](#testing)).
- **Few config options.** An option exists only if it is a real preference.
  If one choice is clearly right, or an action argument or keymap entry covers
  it, there is no option. Options are nested YAML keys named by what they do.
- **Destructive actions always ask for confirmation,** with no option to turn
  that off (see [Destructive actions](#destructive-actions)).
- **reprise never deletes files from disk.**
- **Query MPD rather than copying its database.** The only exception is the
  media library's 2-column mode and mtime sort, which need every song (see
  [Later](#later)).
- **One screen at a time, but nothing assumes it.** Split screens are dropped,
  but an Emacs-like window framework must stay possible (see
  [Screens and views](#screens-and-views)).

Out of scope for now: the clock screen. The tag editor comes later.

## Terms

- **Screen:** a kind of content with its own state and actions: the queue, the
  browser, the search engine, ...
- **View:** how a screen is shown: cursor, scroll offset, size. Today there is
  one view; see [Screens and views](#screens-and-views).
- **Action:** a named operation with typed arguments, e.g. `volume +2`. Every
  action is in the **action registry**, which the keymaps, the `:` prompt and
  the help screen all read.
- **Verb:** a generic action that each screen implements its own way, e.g.
  `activate` or `delete`.
- **Keymap:** a tree of key bindings, one global and one per screen. A
  **prefix** is a key that leads to more keys, as in Emacs.
- **Mirror:** reprise's local copy of MPD's status and queue.
- **Format:** a string in the [format language](#format-language) that
  describes how a song is shown.
- **Style:** colors and attributes, e.g. `yellow on 24` or `black bold`.

## Features

### Core

The core is what the author needs to use reprise every day instead of
ncmpcpp.

**Command line**
- `--host`, `--port`, `--config <file>`, `--help` (also `-h`), `--version`.
- ncmpcpp's `-h` meant the host. In reprise, `-h` is help, the convention
  optparse-applicative follows.
- `--screen` and the other ncmpcpp options are not carried over; the config
  covers them.

**MPD connection**
- TCP and unix socket, password, `MPD_HOST`/`MPD_PORT`.
- Reconnect automatically (see [Errors and logging](#errors-and-logging)).
- Updates driven by `idle`.
- Elapsed time is interpolated on the client and corrected on each player
  event. ncmpcpp polls `status` every second while playing.

**Playback controls**
- Play, pause, stop, next, previous, replay song.
- Seeking: hold the key to move a target on the progress bar, faster the
  longer it's held, then seek once. Also jump to a position (`m:ss`, `N%`,
  ...).
- Volume up/down and set volume.
- Toggles: repeat, random, single, consume, crossfade, replay gain mode.
- Database update.

**Queue screen** (called "playlist" in ncmpcpp; renamed to match MPD)
- Classic and columns display.
- Now playing highlight, album separators, total and remaining time in the
  title.
- Jump to the playing song, or follow it automatically, and jump to it at
  the start.
- Delete, move, clear, shuffle, set priority.

**Browser (MPD database)**
- `lsinfo` navigation, with a `..` entry. The header shows the path.
- Songs show in the classic display or in columns, as `browser.display`
  says, and `toggle display` switches the browser's display for the session,
  as it switches the queue's in the queue. Directories and playlists span
  the columns.
- `activate` enters a directory, opens a playlist and plays a song. A song
  that is already in the queue plays there instead of being added again.
- Going up puts the cursor on the directory or the playlist that was left.
- Playlists open like directories, with their songs and a `..` entry:
  - **Which playlists:** stored playlists, which `lsinfo` lists at the root,
    and playlist files in the music directory, e.g. `.m3u` files and cue
    sheets.
  - **Stored playlists come from `lsinfo` only.** MPD has deprecated listing
    them at the root. When it stops, they leave the browser, and the
    playlist editor shows them.
  - **A cue sheet shows twice,** as MPD lists it: as a playlist and as a
    directory of its tracks. The browser shows what MPD lists, without
    special cases.
  - **Songs of an open playlist are added with `load` and a range,** not with
    `add`. The tracks of a cue sheet are ranges of one file, and adding the
    file would add all of it.
  - **The browser deletes no playlists.** Only the playlist editor will.
- Sort by type, name, mtime, a custom format, or not at all, and
  `next_sort_mode` goes through the modes for the session, as in ncmpcpp:
  - Every mode but none puts the directories first, then the songs, then
    the playlists. By type, each kind stays in MPD's order, which isn't
    alphabetical.
  - Names and formats sort by the rules of the user's locale, as ncmpcpp
    does, and `lists.ignore_leading_the` applies to them. Without a locale,
    ICU sorts capitals first.
  - The songs of a playlist keep its order.
- Add, add and play, add or remove. A directory is added with its
  subdirectories, and a playlist is loaded.
- Update the database for the current directory.
- An idle `database` event lists the directory again, and so does a
  `stored_playlist` event at the root or in a playlist, and a new
  connection. The cursor stays on its entry, or where it was if the entry is
  gone. If the directory is gone, the browser goes up until it finds one.
- `jump_to_browser` opens the directory of the song under the cursor, with
  the cursor on that song, as ncmpcpp's `G` does.

**Search engine**
- The constraint form, searching the database or the queue. The fields are
  ncmpcpp's: any tag, artist, album artist, title, album, filename, composer,
  performer, genre, date, comment.
- Two modes, contains and exact. MPD runs database searches; queue searches
  run on the client, because the queue is already loaded.

**Lists in general**
- A jump to an item puts it in the middle of the list, so that its
  neighbours show on both sides: jumping to the playing song or following
  it, moving to the previous or next album or artist, and later finding.
  Moving by a line or a page scrolls only as far as the cursor needs.
- Selection: item, range, reverse, clear, album, artist, found items.
  - An action applies to the selected items, or to the item under the cursor
    without a selection.
  - The queue keeps the selection by song id, so it follows the songs when
    they move and loses the songs that leave the queue.
  - The browser keeps the selection by entry while it lists the same
    directory or playlist, also when it lists it again. Leaving it clears
    the selection. `..` can't be selected.
  - Select range fills the selection between the last two items that the
    user selected, so selecting both ends and then the range works as in
    ncmpcpp. ncmpcpp fills between the first and the last selected item,
    which also selects everything between the new range and an earlier
    selection. Without two such items, reprise does what ncmpcpp does.
  - Select album selects the songs around the cursor with the same album
    artist (or artist) and album. ncmpcpp compares the album only, so two
    albums called "Greatest Hits" next to each other were one.
  - Select artist selects the songs around the cursor with the same album
    artist (or artist), which is also how the previous and next artist
    moves tell artists apart. A compilation is then one artist, not one
    for each song. ncmpcpp has no select artist, and its artist moves
    compare the artist only.
  - Shuffling the selection needs selected songs next to each other, because
    MPD shuffles a range.
- Incremental find (next/previous), wrapping around at the end. The last
  pattern is shared by all screens, as in Emacs and Vim, so a pattern found
  in one screen can be found again in another. ncmpcpp keeps one for each
  screen.
- Filter.
- Find and filter always ignore diacritics.
- Songs that are in the queue have `lists.queued_style` in the other
  screens, bold by default, which ncmpcpp hardcodes. A song is in the queue
  if the queue has its file, and for a track of a cue sheet, the same part
  of the file. The style lies over the row's like the other states of a
  row, so it works the same in the classic display and in columns.
- Sorting can ignore a leading "the".

**Layout**
- Classic header, status bar, progress bar. The alternative header comes later.
- Prompts with line editing.
- Status bar messages that time out.
- Terminal window title.
- One screen at a time, with popups that stack on top.

**Key bindings**
- Keymaps for each screen, key sequences, and the which-key panel (see
  [Key bindings](#key-bindings)).
- The `:` prompt runs any action with the same syntax as the keymaps.

**Other screens**
- Outputs screen (enable/disable). It is small and used often.
- Help screen, generated from the keymaps. It has a section for the global
  keymap and for each screen's keymap, and each group of a prefix key gets
  its own heading with the whole key sequences, e.g. `ctrl-t r`. It is text
  without a cursor, so the movement actions scroll it.

**Visualizer**
- It reads MPD's fifo output, as ncmpcpp does, in the format `44100:16:2`,
  or `44100:16:1` with `visualizer.in_stereo` off. `visualizer.data_source`
  is the path of the fifo, and the visualizer says how to set it when it is
  missing. A worker thread opens the fifo while the visualizer shows, and
  only then.
- It has two visualizations, the spectrum and the ellipse. It starts with
  `visualizer.visualization`, the spectrum as in ncmpcpp, and `toggle
  visualization` switches, on space in the visualizer as in ncmpcpp.
- **The spectrum** shows the levels of the frequencies as bars.
  - **The worker computes it,** in its own thread, which can call C
    without `unsafePerformIO`, and sends the magnitudes of the bins. The
    layout maps them to the columns, which only it knows.
  - **The window** is the last 16384 samples, about a third of a second,
    with a Blackman window, padded with silence to 32768: ncmpcpp's with
    the author's `visualizer_spectrum_dft_size` of 2. Fewer samples would
    blur the low frequencies, and more would make the spectrum lag.
  - **pocketfft computes the FFT,** in C, in `cbits/pocketfft`. It is under
    BSD-3, the FFT of NumPy and SciPy, and one file. FFTW is GPL, so every
    binary of reprise would be too, and it is a system library to install.
    The worker makes the plan and the coefficients of the window once.
  - **The worker keeps its buffers:** the buffer of the transform, and the
    last samples, which new ones push out in place. Made anew for each
    frame, they were about 700 KB a frame of large objects, and GHC's heap
    grew to 20 MiB on the spectrum, 8 MiB of it lost to fragmentation,
    against 8 MiB on the ellipse. Now it is 12 MiB. The rest are the
    magnitudes that the worker sends, 128 KiB a channel. Mapping them to
    the columns in the worker would save that, but the worker would need
    the width of the screen and the layout's mapping.
  - **The columns** go from 20 Hz to 20 kHz, the range of human hearing, on
    a log scale. A column has the mean of the magnitudes of its bins. The
    lowest columns are narrower than a bin, so they interpolate between
    the bins around their middles.
  - **The levels** are in decibels, from -100 dB for an empty bar to
    -10 dB for a full one: ncmpcpp's with the author's
    `visualizer_spectrum_gain` of 10.
  - **The bars** are the block characters `▁▂▃▄▅▆▇█`, so they have eight
    steps in a cell. In stereo, the left channel rises from the middle and
    the right one hangs from it. The colors go from the foot of a bar to
    its top.
  - **A hanging bar ends in upper blocks,** `▔🮂🮃▀🮄🮅🮆`, most of them from
    Symbols for Legacy Computing, as ncmpcpp does with the author's
    `visualizer_spectrum_smooth_look_legacy_chars`. A lower block in
    reverse video looks the same, but the terminal fills the rest of the
    cell with its default background, which is black on a transparent or
    themed terminal. ncmpcpp's characters for four and five eighths are
    five eighths and a sextant of four sixths, out of order, so reprise
    uses `▀` and `🮄`.
  - **Silence fills the window once MPD stopped writing,** e.g. paused,
    which it did if two writes didn't come. So the bars fall, and once the
    window is silent, the worker sends no more.
- **The ellipse** draws the left channel across and the right one up, as
  ncmpcpp's stereo ellipse does. Mono is a diagonal, and stereo widens it.
  - **Not a goniometer.** A goniometer turns the picture by 45°, so mono is
    a vertical line and its width is the difference between the channels.
    Most music is close to mono, so on a wide terminal it was a narrow
    column in the middle.
  - **Braille dots,** two by four in a cell, so the screen has 8 times as
    many points as cells.
  - **It fills the screen.** The width and the height scale apart, as in
    ncmpcpp's ellipse, so a circle is an ellipse as wide as the terminal.
    Scaled alike, it would leave the sides of a wide screen empty.
  - **The colors show how loud the samples are,** as in ncmpcpp's mono
    ellipse: `visualizer.colors` go from the center to the edges, by the
    root mean square of the channels, and a cell has the color of its
    loudest sample. Colors by age showed little: the newest frame alone
    has a dot in nearly every cell of the picture, so it hid the older
    ones.
  - **The samples of the last frames stay** for `visualizer.trail`.
- **A frame shows the samples of its own time,** `44100 / fps` of them on
  average, e.g. 367 or 368 at 120 frames a second. The worker waits for
  each frame on the clock, so a late frame doesn't make the next ones late.
  - **The wait is `clock_nanosleep`,** in C. GHC's timers wake up to a
    millisecond late, and at 120 frames a second the frames came 7.3 ms to
    10 ms apart instead of 8.3 ms. With it, they come 7.5 ms to 9.2 ms
    apart: the rest is the wait for the one capability of the RTS, which
    the UI may be drawing on.
  - **MPD writes in writes longer than a frame:** 503 samples every 11.4 ms
    on the author's MPD for an mp3, and 924 every 21 ms for a MIDI file. A
    frame that took what had come showed 0 samples in one frame of three
    at 120 frames a second, and the spectrum fell through them.
  - **So the worker buffers two writes.** Frames show samples once the
    buffer holds a frame and two writes more, and until it runs out. With
    one write, a write that came a little late, 19.3 ms to 22.7 ms after
    the last instead of 21 ms, ran the buffer out a few times a second.
    The size of a write comes from the reads: a pipe gives a write that
    short whole, so the shortest read is a write. It costs two writes of
    lag, 23 ms for the mp3.
  - **The clocks of MPD and reprise drift,** so the buffer would grow.
    Beyond a frame and three writes, the oldest samples are dropped, and
    the picture doesn't fall behind the sound.
  - **A rate that divides the screen's refresh rate is the smoothest.** At
    120 frames a second on a screen of 160 Hz, a frame shows for two
    refreshes, then one, then one; at 160 or 80 each frame shows as long as
    the others.
  - **The samples that the fifo held before the visualizer showed are
    old,** so the worker drops them. ncmpcpp resets the output for that,
    which needs its name in the config.
- A frame that the UI can't take in time is dropped: the worker doesn't
  wait for brick's channel of events.
- Measured at 160×45 at 60 fps, 60 frames a second without a drop:
  - **The ellipse,** on a private MPD that played generated pink noise:
    about 7% of a core, and about 140 KiB a second to the terminal.
  - **The spectrum,** on the author's MPD with music: about 9.5% of a core,
    and about 780 KiB a second to the terminal, as most of its cells change
    in every frame.
  - **A frame** (see [Benchmarks](#benchmarks)): the worker computes the
    spectra of the two channels in 160 µs, the layout draws the spectrum in
    163 µs and the ellipse in 205 µs, and vty writes them in 41 µs and
    51 µs.
- **A row is as few texts as it can be.** A cell in the drawing is a color
  by its index, or blank, and a run of a color is one text. A space looks
  the same in every foreground color, so a blank joins the run that it is
  in, unless a color has a background, an underline or reverse video, which
  show on a space. A row of the spectrum is then one text. At 480×106,
  cells of styles and a text for every run took 3.2 ms to draw the spectrum
  and 3.8 ms for the ellipse, and this takes 1.1 ms and 1.2 ms. At 60
  frames a second, reprise then took 12% of a core for the spectrum,
  against ncmpcpp's 10%.
- **vty writes every row that changed whole,** where ncurses writes only
  the cells that changed. At 480×106 a frame of the spectrum is about 72 KB.
  Writing only the changed parts of a row would take a change to vty-unix.
- **The C code is built with `-O3`,** which makes the spectra of a frame
  18% faster, 171 µs instead of 208 µs. `-ffast-math` made no difference
  beyond the noise, so it isn't worth giving up exact floating point in
  pocketfft.
- ncmpcpp's sound wave and its mono ellipse come later, if ever.

**Lyrics**
- **`l` shows the lyrics of the song under the cursor** of a list, and on
  the lyrics screen goes back to the screen that showed them, as ncmpcpp's
  `show_lyrics` does. A screen without songs says so, rather than showing
  the lyrics of another song, e.g. the playing one, so that `l` always
  means the song that the user points at.
- **The lyrics are text that scrolls,** like the help screen, with long
  lines wrapped at spaces. The header shows the song.
- **They are stored as ncmpcpp stores them,** a text file for each song in
  `lyrics.directory`: `<artist> - <title>.txt`, of the first artist and the
  first title, or without both the name of the song's file without its
  extension. The name leaves out the characters that Windows forbids, as
  ncmpcpp does with its default `generate_win32_compatible_filenames`. So
  the author's 394 lyrics from ncmpcpp show as they are, and ncmpcpp reads
  the ones that reprise stores. A file that isn't UTF-8 shows, with
  replacement characters for its bytes that aren't.
- **A worker thread reads them,** as the handlers can't do IO. It takes only
  the newest request, so the songs that the screen passed by aren't read,
  and a token drops the lyrics of a song that the screen left.
- **Lyrics that aren't stored are fetched** from `lyrics.fetchers`, in
  order, and stored, so that they are fetched once. The screen says
  "Fetching the lyrics…" only once the worker found none stored, so stored
  lyrics show without a flash of it.
  - **"Not there" is remembered** until reprise exits: no lyrics, or an
    instrumental. So the songs that a fetcher doesn't have aren't asked for
    each time they show. An instrumental isn't stored, as ncmpcpp has no
    way to read that back.
  - **A failure is neither stored nor remembered,** e.g. no network, or a
    busy server: LRCLIB answers 503 at times. A later request tries again,
    and the next fetcher is asked meanwhile.
  - **`` ` `` fetches the lyrics again,** as in ncmpcpp, and stores them
    anew, e.g. over wrong ones.
  - **A file is written whole:** to a temporary file, then renamed over the
    old one, with the permissions of a new file.
- **The first fetcher is lrclib.net.** It has a public JSON API, and plain and
  timed lyrics. ncmpcpp's fetchers scrape pages that Google finds, and
  Google itself shows the lyrics at the top of a search, but it serves
  search only to clients with JavaScript since 2025: a fetch of "lyrics
  Radiohead Karma Police" got a page without them, with curl's user agent
  and with Firefox's. Of 40 random songs of the author's library, LRCLIB had
  17, 13 of them with timed lyrics. Most of the others were classical
  music and instrumentals.
  - **The lookup:** by the artist, the title, the album and the length,
    which LRCLIB matches within 2 seconds. Else a search of the artist and
    the title, which takes a result within those 2 seconds, with lyrics
    rather than an instrumental. Else both again with the title without
    what follows it in brackets. In the author's library the titles end in
    `(Bonus Track)`, `(Remix)`, `[Film Score]` and the like, which LRCLIB
    doesn't have. Titles that end in ` - Allegro` and other movements are
    the most common ending, but those are part of the title, so they stay.
  - **The replies decode with yamlet,** with types derived generically. Each
    of 52 songs in five replies had the same keys, and only the lyrics
    could be null. Unknown keys are allowed, as LRCLIB adds some.
  - **LRCLIB is HTTPS only,** so reprise needs `http-client-tls`. The
    requests name reprise in the user agent, as LRCLIB asks.
  - **A request gives up after 10 seconds.** LRCLIB answered 30 requests in
    0.86 s at most, and the worker serves one request at a time, so one
    that hangs holds up the next.
  - **The tests replay LRCLIB's answers** with other lyrics in them, through
    a fake of the HTTP request.
- **The second fetcher is tekstowo.pl,** a site of lyrics that has some
  that LRCLIB doesn't, e.g. Polish songs. ncmpcpp found its pages through
  Google's "I'm Feeling Lucky", which no longer redirects, so all of
  ncmpcpp's fetchers of sites find nothing now. reprise uses the site's own
  search.
  - **It has no API,** so the fetcher reads its pages with tagsoup, which
    parses real pages leniently and decodes their entities. It breaks when
    the site changes its pages.
  - **The search, `/szukaj?search-query=`,** for the artist and the title
    without what follows it in brackets. The songs that it found are the
    links of their section, up to the next heading; the page links other
    songs elsewhere, e.g. the popular ones.
  - **Only the same artist and title is taken,** without case, with or
    without what follows the title in brackets. Another song's lyrics would
    be stored as the song's, so a close result is worse than none.
  - **The lyrics are the first text of the song's page;** the second is the
    translation. `<br />` ends a line.
  - **A search that finds nothing redirects** to the advanced search, which
    the site's robots.txt asks bots not to read. Redirects aren't followed
    by any fetcher, and this one is "not found".
  - **The tests read trimmed copies** of a search, a song's page and the
    redirect, saved once, with other lyrics in them.
- **Timed lyrics show the line being sung** while their song plays, in
  `lists.playing_style`, as the song that plays shows in a list. The screen
  keeps that line in its middle, until the user scrolls, and follows again
  for the next lyrics that it shows.
  - **The times are stored in LRC,** in `<artist> - <title>.lrc` next to the
    `.txt`, as LRCLIB sends them. The `.txt` stays for ncmpcpp, which
    doesn't read LRC. Lyrics stored anew without times remove the `.lrc`,
    whose times would go with the old ones.
  - **A line of LRC can have several times,** e.g. a chorus, and tags
    without a time, e.g. `[ar:Artist]`, aren't lines. A time is read
    exactly, as `62.345` through a `Double` came to 62.344 s.
  - **The screen is drawn again when the next line is sung,** besides the
    redraws of the elapsed time. Where the screen starts is worked out when
    it is drawn, so following the song needs no events of its own.
- **`lyrics.fetch_in_background` fetches the lyrics of each song that
  plays,** as ncmpcpp's `fetch_lyrics_for_current_song_in_background`
  does, so that they are stored when the screen shows them. The worker
  takes a request of the screen first, so the screen doesn't wait for the
  background. A failure in the background goes to the log.
- **`lyrics.follow_playing` shows the lyrics of each song that plays** while
  the lyrics screen shows, as ncmpcpp's `follow_now_playing_lyrics` does.
  Only a new song moves the screen, so `l` still shows the lyrics of
  another song until then. Space on the lyrics screen toggles it, as in
  ncmpcpp, with `toggle follow_playing`, which toggles the queue's on the
  other screens, as `toggle display` does the browser's in the browser.
  Turned on, it shows the lyrics of the song that plays at once.
- **`e` edits the stored lyrics,** as in ncmpcpp: the `.lrc` if the times
  show, else the `.txt`, which the editor makes if there is none, e.g. for
  lyrics pasted from elsewhere. The lyrics show again when the editor exits.
  - **The editor is `editor.command`,** else `$VISUAL`, else `$EDITOR`.
    ncmpcpp's default was `nano`; with the environment, a user who set an
    editor for every program needs no config.
  - **`sh` runs the command,** as it can have arguments, e.g. `emacs -nw`,
    with the file as `"$1"`, so that a song's title in the file's name is
    never read as shell code. reprise suspends the UI while it runs, the way
    [`run_in_terminal`](#external-commands-later) will.

### Later

Implemented when the author misses them.

**Media library** (the first screen to add after the core)
- 3-column mode (tag, albums, songs) and 2-column mode (albums, songs).
- Choice of primary tag, multi-value tags, albums split by date.
- "All tracks" and `<no album>` entries.
- Sort by name or mtime.
  - The 2-column mode and the mtime sort load the whole database with
    `listallinfo /`, as ncmpcpp does. MPD has no aggregate "latest mtime per
    album", so nothing smaller works.
  - On a large library the reply exceeds MPD's `max_output_buffer_size`
    (8 MiB by default). The documentation recommends raising it in
    `mpd.conf`.
  - When MPD refuses the reply, reprise shows an error that names
    `max_output_buffer_size`. It doesn't silently fall back to another mode
    the way ncmpcpp does.
- Lazy loading of the next column.

**Playlist editor**
- Two columns: stored playlists and their contents.
- Load, add, rename, delete, move, clear.
- Until then, the browser can open and load stored playlists.

**Tag editor and tiny tag editor**
- They write tags into the files with TagLib, so the music directory option
  comes back with them.
- The tag editor's screen key is `6`, as in ncmpcpp.

**Lyrics in the files' tags**, ncmpcpp's `tags` fetcher. MPD's
`readcomments` gives them without the music directory.

**Last.fm artist info**
- ncmpcpp uses a hardcoded API key and regex scraping of the wiki page. This
  needs a redesign too.

**Smaller screens and actions**
- Song info screen, using MPD tags only. Bitrate, sample rate and ReplayGain
  read directly from the file need TagLib, which comes with the tag editor.
- Server info popup.
- Selected items adder popup. MPD 0.23 relative positions (`addid uri +0`)
  make "after current song" trivial.
- Sort queue dialog. MPD has no sort command, so this stays client-side. It
  issues the minimal set of `move` commands instead of ncmpcpp's `swap` per
  quicksort step.
- Add random items: songs, or tags such as artists and albums.

**Interaction**
- Mouse support: seek on the progress bar, scroll, click to select or play.

**External commands, hooks and command line**
- Key bindings that run external programs (see
  [External commands](#external-commands-later)).
- Commands to run on song change and on player state change
  (`execute_on_song_change`, `execute_on_player_state_change`).
- `--current-song[=FORMAT]`.

**Display**
- The alternative header design.
- Runtime toggles for the display mode, bitrate visibility, and the header
  design.

### Dropped

**The clock** is out of scope, with its options.

**Split screens** (locked screen, master/slave)
- They are constrained, rarely used, and caused a lot of maintenance and
  bugs in ncmpcpp.
- Their actions and options go too: `toggle_screen_lock`,
  `master_screen`/`slave_screen`, `locked_screen_width_part`,
  `ask_for_locked_screen_width_part`, `startup_slave_screen`,
  `startup_slave_screen_focus`.
- If something comes back, it is a general window framework (see
  [Screens and views](#screens-and-views)).

**Local filesystem browse mode.** It needs TagLib and only works with a unix
socket.

**Deleting files from disk** (`allow_for_physical_item_deletion`). No screen
implements it.

**The search engine's regex mode.** It downloads the whole database to match
on the client.

**Binding chains and alternative bindings** (`require_screen`,
`require_runnable`, `push_character(s)`, `update_environment`,
`def_command`). Keymaps for each screen and actions with arguments replace
them (see [Key bindings](#key-bindings)).

**ncmpcpp's file formats:** the config file, the bindings file and the format
language (`%a`, `{...}|{...}`, `$(color)`, `$b`, `$R`). An importer for the old
files could come later, if ever.

**Reversing the queue** (`reverse_playlist`). It is rarely useful, and MPD
has no command for it.

**Cropping** (`crop_main_playlist`, `crop_playlist`). Inverting the
selection and deleting takes as many keys, and shows what goes before it
goes, so it needs no confirmation.

**Old compatibility tricks**
- MPD versions older than 0.23.1. In return reprise gets filter expressions,
  relative positions in `addid`, `searchadd`/`findadd` with a position, and
  `load` with a position.
- The "add and play" trick (`play <old length>` after the add). reprise sends
  `add` or `load` at the queue's length N and `play N` in one command list,
  for songs, directories and playlists alike. MPD runs a command list without
  other clients' commands in between, so the songs start at N even if the
  mirror missed a change. Only a queue that got shorter than N fails, with
  MPD's error.
- `--test-lyrics-fetchers`.

Dropped config options are listed in [Configuration](#dropped-options).

## Architecture

### Packages and modules

reprise is one package. The MPD protocol is a private library of it,
`mpd-protocol`, in `src/mpd-protocol`. As a library of its own, it can't
import the rest of reprise, so it can become a package again when another
project needs it. A second private library, `mpd-test-server` in
`tests/mpd-test-server`, starts a real `mpd` for both test suites (see
[Testing](#testing)). It talks to MPD with a few raw protocol lines rather than
through `mpd-protocol`: a harness that shares the library's code would share
its bugs.

The module tables below are a starting layout, not a fixed one. Module
boundaries, names and types are expected to change while the code is written.

**`mpd-protocol`** is an MPD protocol library with no UI dependencies. Its
modules are under `Reprise.Mpd.Protocol`.

| Module | Contents |
|---|---|
| `Reprise.Mpd.Protocol.Types` | `Song`, `Status`, `Stats`, `Output`, `Tag`, `Subsystem`, `PlayerState`, `SongId`/`SongPos` newtypes, `MpdError` (with ACK codes) |
| `Reprise.Mpd.Protocol.Response` | Pure parser for `key: value` lines, `OK`/`ACK`/`list_OK`, and splitting songs on `file:`/`directory:`/`playlist:` keys |
| `Reprise.Mpd.Protocol.Request` | Command serialization and argument quoting |
| `Reprise.Mpd.Protocol.Filter` | Typed builder for filter expressions (`(artist == "x") AND ...`) |
| `Reprise.Mpd.Protocol.Command` | `Command a` and the typed commands |
| `Reprise.Mpd.Protocol.Connection` | TCP/unix connect, handshake and version check, password, timeouts. `run :: Connection -> Command a -> IO a`, which throws `MpdError` |
| `Reprise.Mpd.Protocol.Idle` | `idle` and `noidle` |

`Command a` has three properties:
- **It is `Applicative`,** so `f <$> c1 <*> c2` runs as one
  `command_list_ok_begin` round trip, with each part parsed at its `list_OK`.
- **It is not a `Monad`.** A command that depends on an earlier result needs a
  second round trip, and the types make that visible.
- **It holds its request lines as plain data** (with `Eq` and `Show`), next to
  the parser for its reply. Tests compare the commands an action issued
  without running them.

`mpd-protocol` has a plain `IO` API, so it is usable without effectful.
- **Failures are exceptions.** `connect`, `run`, `idle` and `noidle` throw
  `MpdError`, also for an I/O error of the socket, so a caller catches one
  type. An operation normally succeeds: a failure is a lost connection, a
  timeout, a protocol violation, or an `ACK`, which a correct client rarely
  causes. A caller that expects an `ACK` catches it. `displayException`
  describes an `MpdError` for people, which reprise shows and logs.
- **The pure parsers return `Either`,** e.g. `parseReply` and
  `parseCommandReply`.

**`reprise`** is the client. The binary is `reprise`, and the config lives in
`$XDG_CONFIG_HOME/reprise/config.yaml`.

| Module | Contents |
|---|---|
| `Main` (`app/Main.hs`) | CLI options (optparse-applicative), loading the config, starting the workers and brick |
| `Reprise.App` | The brick application: a thin adapter between brick's events and the handlers |
| `Reprise.Effect.*` | The app's own effects (`MpdRequest`, `UiRequest`, `Mpd`), one module each |
| `Reprise.Config` | Config types, yamlet decoders, defaults, the default keymaps |
| `Reprise.Format` | The format language: parser and renderer to styled spans. Pure, with golden tests |
| `Reprise.Style` | Parsing styles into vty `Attr` |
| `Reprise.Keys` | Key spec parsing (`ctrl-x`, `alt-shift-tab`, ...) |
| `Reprise.Keymap` | Keymaps, key sequences and lookup, and the listings of keymaps: the which-key entries and the lines of the help screen |
| `Reprise.Action` | The action registry: actions as data, with their names, argument parsers, descriptions, and whether they are destructive |
| `Reprise.Handler` | The handlers of events, keys, prompts and the actions of every screen, e.g. playback and toggles. It passes the actions of a screen on to the screen's module |
| `Reprise.Handler.Core` | What the handlers and the screens share: the effects, access to the state, messages, prompts, and keeping a view's cursor in its list |
| `Reprise.State` | `AppState`: the mirror, screens, views, layout and focus, status bar message, prompt. `AppEnv`: what doesn't change while reprise runs, the config, the keymaps, the colors, the order of text, the directory of lyrics and the editor. Queries of the state that both the handlers and the layout need, such as whether the cursor shows, or the header's title and whether it scrolls |
| `Reprise.Mpd.Mirror` | Pure updates of the mirror from MPD replies, such as `plchanges` plus truncation to `playlistlength` |
| `Reprise.Mpd.Worker` | Connection threads. They read a request queue and write events to brick's `BChan` |
| `Reprise.Visualizer.Worker` | The thread that reads MPD's fifo output while the visualizer shows, and sends the samples of each frame |
| `Reprise.Visualizer.Samples` | The format of the samples that the worker reads and the visualizer draws |
| `Reprise.Visualizer.Spectrum` | The spectrum of the samples, with pocketfft's FFT in `cbits` |
| `Reprise.Lyrics` | Where the lyrics of a song are stored, as ncmpcpp stores them |
| `Reprise.Lyrics.Worker` | The thread that loads the lyrics that the lyrics screen asks for: the stored ones, else fetched ones, which it stores |
| `Reprise.Lyrics.Http` | The HTTP requests of the fetchers, over HTTPS, without redirects |
| `Reprise.Lyrics.Lrclib` | The lyrics of a song from lrclib.net |
| `Reprise.Lyrics.Tekstowo` | The lyrics of a song from the pages of tekstowo.pl |
| `Reprise.Mpd.Address` | Where MPD is: the command line, the config, `MPD_HOST`, the usual sockets |
| `Reprise.Event` | The brick custom event type, which every continuation produces |
| `Reprise.Find` | The patterns of find and filter: ICU regular expressions with diacritics folded |
| `Reprise.Collation` | The order of text by the rules of a locale, with a leading "the" ignored if the config says so. The tests use ICU's root rules, so that they don't depend on the locale |
| `Reprise.Groups` | Neighbouring songs of the same artist or album, between which the moves to the previous and the next album or artist go, in every list, and runs of consecutive positions |
| `Reprise.Selection` | The selection of a list by the keys of its items, with the ends of the next range. `Reprise.Handler.Core` applies the select actions to any list with it |
| `Reprise.LineEdit` | The line that a prompt edits, with Emacs-style keys |
| `Reprise.Width` | The width of text in terminal columns, cutting and wrapping text to a width, and the table of character widths that reprise installs for vty |
| `Reprise.UI.SongList` | Rows of songs, rendered classic or in columns, for every screen that lists songs, and rows of other items, e.g. directories |
| `Reprise.UI.Layout` | The frame: header, status bar, progress bar, the which-key panel, popups. The focused screen's module draws the main view |
| `Reprise.Screen.*` | A module for each screen, as in ncmpcpp, with the screen's actions and its drawing: `Queue` (with `Queue.Edits`, which plans the MPD commands that change several songs), `Browser`, `Visualizer`, `Lyrics` and `Help`. Later: `SearchEngine`, `Outputs`, `MediaLibrary`, `PlaylistEditor`, `SongInfo`, `ServerInfo`, ... |

The modules form layers, and an import only goes down:

```
Reprise.App
 ├─► Reprise.Handler ──┐
 └─► Reprise.UI.Layout ┴─► Reprise.Screen.* ─► Reprise.Handler.Core ─► Reprise.State, and the rest
```

- **Only `Reprise.App` imports `Reprise.Handler`.** The tests and the
  benchmarks run events through it too.
- **Only `Reprise.Handler` and `Reprise.UI.Layout` import a screen.** A
  screen doesn't import another screen; its own submodules, such as
  `Queue.Edits`, are part of it.
- **`Reprise.Handler.Core` imports no screen and no module of `Reprise.UI`.**
  What generic code needs to know about every screen, such as the length of
  its list, comes from the state, e.g. the help screen's lines come from the
  keymaps.
- **The modules under `Reprise.Handler.Core`,** such as the state, the
  config, the mirror and the format language, import none of the modules
  above them. `Reprise.UI.SongList` is drawing that the screens share, so
  it sits under the screens too.
- **`mpd-protocol` imports nothing from reprise.** It is a library of its
  own, so cabal enforces this.

`LayerTests` checks the other rules: it reads the imports of the sources and
names each one that crosses a layer, with the rule.

Libraries:

| Need | Package |
|---|---|
| UI | `brick`, plus `vty`/`vty-unix` |
| Sockets | `network` |
| Config | `yamlet` |
| CLI | `optparse-applicative` |
| Regex, diacritics folding, collation | `text-icu` (see [Find and filter](#find-and-filter)) |
| HTTP (lyrics, later artist info) | `http-client` and `http-client-tls` |
| HTML (lyrics from tekstowo.pl) | `tagsoup` |
| Effects | `effectful` (see [Effects](#effects)) |
| FFT | pocketfft, in `cbits` (see [Visualizer](#core)) |
| Record updates | `optics-core` (see [Code](#code)) |

### MPD connection

reprise opens two connections to MPD:
- **Idle connection.** A loop that runs `idle` and sends
  `MpdChanged [Subsystem]` events to the UI. It never sends anything else.
- **Command connection.** A worker that runs requests one at a time from a
  queue.

Each thread is a plain blocking loop. ncmpcpp uses one connection and sends
`noidle` before every command instead.

Requests are asynchronous: a request carries a `Command a` and a continuation
`a -> AppEvent`, so the UI never blocks.
- If the command fails, the worker catches the `MpdError` and sends the
  request's failure event instead of the continuation's event. An exception
  can't reach the UI thread, so this is where it becomes a value. Unless a
  query names its own (see below), the failure event is `MpdFailed` with the
  request lines, so errors are handled in one place: the status bar shows
  them, and later the password prompt can retry the failed request there.
- Mutations (play, delete, move, ...) don't need the reply. The new state comes
  back through `idle`.
- Queries (`lsinfo`, `find`, ...) deliver their result to the screen that asked
  for it. A reply to a query that a newer one has replaced, e.g. the listing
  of a directory the user has already left, is dropped.
- A query can name its own event for a failure, when the screen can recover.
  The browser goes up when the directory it lists is gone, instead of only
  showing MPD's error.

MPD closes a connection that was unused for longer than its
`connection_timeout`, 60 seconds by default, and the command connection is
often unused that long. MPD closes it before it reads the next command, so a
command that finds the connection closed before any reply runs once more on a
new connection.

A key runs exactly one action, so there are no chains to keep in order. When
one operation needs a reply before its next step, it is a single action that
continues inside its continuation. An example is toggling the replay gain
mode, which asks MPD for the current mode and sets the next one when the
reply comes.

### Effects

The IO surface is small and sits at the edges: the MPD sockets, the worker
threads, reading the config, the clock, and later HTTP, the lyrics cache,
external commands and the editor. Most of the code is pure:
formats, config decoding, key parsing, list, filter and selection logic, the
mirror, layout, and the decision part of almost every action.

Brick's `EventM` is a separate monad, a state monad over IO with `MonadIO`, so
brick handlers don't run in `Eff`. brick stays the outer loop, and effectful is
used in two places:

1. **Actions are `Eff` code with a small, mostly pure stack,** for example
   `(State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es) => Eff es ()`.
   - `AppState` is what changes while reprise runs. `AppEnv` is what doesn't:
     the config, the keymaps, the colors of the terminal and the collator of
     the locale. The handlers
     read it through `Input`, which, unlike `Reader`, has no `local`, so no
     handler can change it, not even for a part of its work.
   - Logic that only changes the state is a pure function, e.g.
     `jumpTo :: Int -> AppEnv -> AppState -> AppState`, and the handlers
     apply it. Only what messages the user or requests something from MPD
     or brick is `Eff` code.
   - `MpdRequest` queues commands with continuations.
   - `UiRequest` covers what only brick can do: halting, suspending for an
     external program, the terminal title, timers that send an event later,
     and skipping the redraw after an event that changed nothing on the
     screen. The timers end a seek, expire messages, hide the queue's cursor,
     and redraw the elapsed time. A timer whose token is stale skips the
     redraw. One timer hides the cursor for a run of keys: when it fires
     early, it waits for the rest of the delay after the last key, so
     holding a key doesn't add a redraw per key.
   - Prompts and confirmations are state, not requests. An action can't stop
     halfway and wait for the user, because brick is the outer loop. So it
     opens a prompt with a continuation, e.g. `confirm msg onYes`, and the
     pending prompt lives in `AppState`. When the user answers, the
     continuation runs.
   - **Every continuation is data**: an `AppEvent` for an MPD reply, a timer
     and a confirmation, and what a line prompt asks for, e.g. a volume,
     which the handler runs with the answer. The state holds them, so they
     must be data: a function in `AppState` would need the effects, whose
     operations carry the continuations, and the modules would form a
     cycle. As data, they also have `Eq` and `Show` for the tests.
   - The brick handler is a thin adapter. It reads the state, runs the event
     with handlers that collect the requests, writes the new state back, and
     then performs the collected requests.
   - The event runs in `IO` with `runEff`, not `runPureEff`, though the
     handlers can't do IO without `IOE`. `runPureEff` hides the IO of `Eff`
     behind `unsafeDupablePerformIO`, and the benchmark of the down key
     takes 1.77 µs with it against 0.92 µs with `runEff`. The tests and the
     benchmarks run events in `IO` too.
   - Tests run the same events with pure handlers, without MPD or a terminal.
     This is the main payoff.
2. **Worker threads are ordinary `Eff` programs** with IO-backed effects (the
   MPD connection, and later HTTP, processes and logging). Each thread runs
   its own `runEff`.

Actions are data, in `Reprise.Action`, and their handlers are in
`Reprise.Handler` and the modules of the screens. The config holds actions in
its keymaps, and the handlers need `AppEnv`, which holds the config, so an
action with its handler inside would make the modules a cycle too.

The screen is drawn as one vty image from the state and the environment, with
brick as the event loop around it. The image is a pure function of them, so
the snapshot tests render it to text.

The elapsed time is redrawn by a timer, not by polling. After each event, the
handler computes when the screen next changes, the next whole second or the
next cell of the progress bar, and sets a timer for then. A stream without a
length has no progress bar, so only the seconds count. The same timer
scrolls the header's title, also while nothing plays.

The worker threads use `mpd-protocol` through a small `Mpd` effect. Actions
never use it directly; they go through `MpdRequest`.

### Errors and logging

- **Config errors.** reprise prints every error yamlet reports, with file,
  line and column, and exits with a non-zero status. There is no
  `--ignore-config-errors`: a config that only half applies is worse than a
  clear error. A missing config file is not an error; the defaults apply.
- **Lost connection.** The header shows that reprise is disconnected, and both
  connections retry every second, as ncmpcpp does. The screens keep their
  content until the connection is back, then the mirror is fetched again. The
  player's status is cleared instead: a frozen "Playing" would look like a
  hang, and MPD may not be playing at all. So the status bar, the progress
  bar and the mode flags are empty, and actions that need the status say
  that reprise isn't connected.
- **MPD errors** (`ACK` replies) appear in the status bar, with the command
  that failed.
- **Permission errors** (`ACK [4@...]`) prompt for the password, send it, and
  retry the command. Cancelling the prompt leaves reprise running but
  read-only.
- **Logging.** Unexpected exceptions and protocol errors go to
  `$XDG_STATE_HOME/reprise/reprise.log`, never to the terminal while the UI is
  running. Each run appends to the file.

Without effectful, the same design would work with actions as plain functions
`AppState -> (AppState, [Request])`. The effectful version reads better once
actions grow: an action can issue several requests in the middle of its logic,
or reuse helpers that need state and requests, and there are no tuples to
thread by hand.

### Screens and views

Each screen module holds its state record, its render function, and its
actions. There is no shared class hierarchy. A screen is a record of functions
where polymorphism is needed, e.g. the "has songs" operations that bulk actions
use, and the [verbs](#key-bindings). A screen can leave a verb unimplemented.

Split screens are dropped. If they come back, it will be as a general window
framework with the basics of what Emacs does:
- split a window horizontally or vertically, and close it;
- move focus to the other window, or in a direction;
- show any screen in any window, including the same screen in two windows;
- resize and balance.

Building that later is cheap only if the core doesn't assume there is one
screen. ncmpcpp assumed it, which is why its split screens grew into a pile of
special cases (`myScreen`, `myLockedScreen`, `myInactiveScreen`,
`getWindowResizeParams`). So the core follows four rules from the start,
without building any of the framework:

1. **Screen state and view state are separate,** like Emacs buffers and
   windows.
   - A screen holds its content: the queue mirror, the browser's directory
     listing, the selection.
   - A view holds how that content is shown: the cursor, the scroll offset,
     the size it was last rendered at.
   - A view remembers the cursor and the offset of each screen it showed, so
     switching back returns to the same place, as an Emacs window does with
     its previous buffers. A change of a screen that the view doesn't show,
     e.g. following the playing song, moves the remembered position.
   - Today there is exactly one view. A second view of the queue would only
     need a second view record.
2. **Size comes from the view.** Page up/down, centering and scrolling read
   the height of their view. Nothing reads the terminal size directly, except
   the layout code.
3. **Actions target the focused view.** Every action gets its screen and view
   through one function. The keymap lookup works the same way.
4. **Layout is data.** The layout is a value in `AppState`, today
   `Single ViewId`. A window tree would add a `Split` constructor; the
   renderer and the focus functions would change, the screens would not.

The cost now is a `View` record and the discipline of passing it around.

## User interface

### Key bindings

In ncmpcpp, several bindings for one key are tried in order until one runs.
Mostly this picks an action based on the current screen. ncmpcpp's defaults
show it:
- `enter` tries enter directory, then toggle output, then run action, then play
  item.
- `delete`, `c`, `C`, `e`, `ctrl-s`, `m` and `n` each have one variant per
  screen.
- `right` means next column, else slave screen, else volume up.
- `4` shows the media library, or toggles its columns mode if it is already
  shown.

Which action wins depends on the order of the alternatives and on each action's
hidden "runnable" check. Only two defaults are real chains of actions:
`shift-up` and `shift-down` (select the item, then move).

reprise replaces this with three parts.

**1. Keymaps for each screen.** There is a `global` keymap and one for each
screen.
- A key is looked up in the focused screen's keymap first, then in the global
  one.
- A key can be bound only once per keymap. A duplicate YAML key is already a
  yamlet error.
- "Show the media library, or toggle its mode if it's already shown" becomes
  `4: toggle_columns_mode` in the `media_library` keymap.
- The help screen lists exactly what each screen's keymap does.

Key sequences (Emacs-style `ctrl-p a`) are supported from the start:
- A keymap is a tree. A binding is either an action or a prefix with its own
  keymap, and in YAML a prefix is simply a nested mapping. YAML already makes
  "both an action and a prefix" impossible.
- The pending prefix lives in `AppState`. It is not a blocking read, so MPD
  events keep updating the screen in the middle of a sequence.
- The status bar shows the prefix (`ctrl-p -`), and the
  [which-key panel](#which-key-panel) lists the possible next keys.
- `escape` or `ctrl-g` cancels. An unbound next key cancels with a message.
  There is no timeout.
- Layering works one key at a time. The screen's and the global keymaps are
  walked in parallel:
  - if both bind the key to a prefix, the walk continues into both groups, so
    the queue's `ctrl-q` group adds keys to the global one;
  - otherwise the screen's binding wins. If the screen binds `ctrl-p` to an
    action, it shadows the global `ctrl-p` prefix.
- Some keys look the same to a terminal: `ctrl-i` is `tab`, `ctrl-m` is
  `enter`, `ctrl-[` is `escape`, and `ctrl-h` is often `backspace`. The key
  parser rejects these names with a hint ("`ctrl-i` is the same key as `tab`
  in a terminal; use `tab`"). It doesn't silently convert them, because then
  two different-looking entries could collide.

**2. Verbs that each screen implements,** e.g. `activate`, `delete`,
`add_or_remove`, `parent`, `next_column`/`previous_column`.
- The browser's `activate` enters a directory, the outputs screen's toggles an
  output, the queue's plays the song.
- If a screen doesn't implement a verb, the status bar says so instead of
  falling through to an unrelated action.

**3. Actions take arguments,** e.g. `volume +2`, `seek -10s`, `select down`
(replaces the shift-arrow chains), `show browser`, `priority 5`,
`toggle repeat`.
- Each action in the registry has a typed argument parser.
- The keymaps and the `:` prompt use the same syntax, so `:volume 40` just
  works and named commands (`def_command`) aren't needed.

This covers all of ncmpcpp's alternative bindings except the `right` fallback
to volume. That one is a quirk, and `+`/`-` already change the volume.

**Default keymap rule.**
- Single keys for actions used many times per session: play/pause,
  next/previous, volume, seek, navigation, selection, `activate`, screen
  switching, find.
- Prefix groups by topic for everything else, e.g. `ctrl-t` for toggles and
  `ctrl-q` for queue operations.
- The full layout is in [Keymaps](#keymaps).

The author once considered making ncmpcpp's bindings file a Python script.
Scripting is not planned for reprise, but the action registry (name, argument
parser, description, handler) is exactly what a scripting layer would expose.
If one is ever wanted, Lua through `hslua` is much lighter to embed than
Python.

### Screen layout

From top to bottom, as in ncmpcpp's classic design:
- **Header, line 1:** the screen's title on the left (for the queue, with the
  song count and the total and remaining time), the volume on the right. A
  title has a part that stays, e.g. `Queue ` or `Browse: `, and a part that
  scrolls by a character each second if it doesn't fit, e.g. the queue's
  song count and times or the browser's path, as in ncmpcpp. It scrolls from
  its start whenever the title shows another screen, or the browser lists
  another directory or playlist.
- **Header, line 2:** a horizontal line with the mode flags on the right:
  repeat, random, single, consume, crossfade, and "updating the database".
- **The main view.**
- **Progress bar.**
- **Status bar:** the player state, the playing song (`status_bar.song`,
  scrolling when it doesn't fit), the bitrate if enabled, and
  elapsed/total time. Messages, prompts and the pending key prefix take
  this line over while they're active.

Without a mixer in MPD, the volume shows as "n/a" and the volume actions say
so.

### Character widths

vty computes every width with its built-in table from Unicode 5.0, in which
an emoji is narrow. Terminals draw it wide, so a row with one would overflow
the line. At the start, reprise installs a table built from the C library's
`wcwidth`, in the user's locale or `C.UTF-8`, as ncmpcpp measures with
`wcwidth`. Building it takes about 0.1 s. Without a UTF-8 locale, vty keeps
its built-in table.

reprise doesn't read vty's own config, so a table that `vty-build-width-table`
measured in a terminal isn't used. A terminal can disagree with the C
library, e.g. when it draws ambiguous-width characters wide, but ncmpcpp had
the same limit and it never showed. Widths are for single characters, so a
sequence that a terminal draws as one emoji, e.g. with a zero-width joiner,
can be measured wrong.

### Prompts

Prompts (find, filter, `:`, confirmations) sit in the status bar. While a prompt is open, keys go to it, not to the keymaps. `enter`
accepts, `escape` or `ctrl-g` cancels.

A line prompt edits its line with Emacs-style keys, and the terminal's cursor
shows where:
- `left`/`ctrl-b`, `right`/`ctrl-f`, `home`/`ctrl-a`, `end`/`ctrl-e`;
- `backspace`, `delete`/`ctrl-d`, `ctrl-k` to the end, `ctrl-u` to the start;
- `ctrl-w` deletes to the previous space, as in a shell;
- `alt-b`, `alt-f`, `alt-d` and `alt-backspace` work on words of letters and
  digits, as in readline.

The editor is reprise's own pure code, not brick's editor. brick's editor
handles events in brick's monad, outside the handlers that the tests run.

The `:` prompt runs an action. It is also how a key asks for a value: the
`command` action takes the start of the line, so `ctrl-p s: command seek`
opens `:seek ` and the user types the time. One prompt and the registry's
parsers serve every action with arguments, instead of a prompt action for
each, such as ncmpcpp's `set_volume`. Any action with arguments can become
an "ask me" binding without new code.
- **A hint** on the right of the line tells how to go on: the usage of the
  action that the line names, e.g. `seek +Ns | -Ns | m:ss | N%`, and, once
  the line parses, what `enter` will do, e.g. "seek to 1:30". The line comes
  first; the hint gets the room that is left.
- **An answer that doesn't parse** shows the registry's error with the usage.
  An empty line does nothing.
- **Arguments** are words and lists, except for `add_path` and `command`,
  which take the rest of the line, because a path holds spaces and brackets.

Find (`/`, `?`):
- **On every key,** the cursor moves to the first match after where the find
  started, so the result doesn't depend on how the pattern was typed, e.g.
  with corrections. A match centers the cursor, as every jump does.
- **The visible matches** have `lists.found_style` while the prompt is open.
- **The status bar notes** that the find wrapped around, found nothing, or
  that the pattern is incomplete.
- **Cancelling** goes back to where the find started. ncmpcpp stays at the
  match, which makes `escape` the same as `enter`.
- **`enter`** keeps the pattern for find next and previous (`.` and `,`),
  which go forward and backward, as in ncmpcpp. An empty find repeats the
  last pattern, as in Vim.
- **Select found** selects every song that the last pattern matches.

Filter (`ctrl-f`) hides the items that don't match, also on every keystroke;
filtering with an empty pattern removes the filter.

### Seeking

A terminal reports no key releases, only the repeated presses while a key is
held. So `seek` works in two phases:
- each press moves a target on the progress bar, by a step that grows the
  longer the presses keep coming;
- when no press has arrived for a while, reprise sends one `seek` to the
  target.

The pause that ends a seek must be longer than the terminal's initial key
repeat delay. Otherwise the gap between the first press and the first repeat
would end the seek early. It is a named constant whose comment names its
source: common key repeat settings, e.g. X11's default of a 660 ms delay and
25 repeats per second. The cost is that a single tap seeks after that pause,
not at once.

### External commands (later)

External commands come with the hooks, not in the core. When they arrive:
- **`run` starts a program without a shell,** with its arguments as written,
  e.g. `run notify-send "now playing"`.
- **Song data is never spliced into the command line,** so a song title can't
  inject shell code. The program gets the playing song and player state in its
  environment instead: `REPRISE_ARTIST`, `REPRISE_TITLE`, `REPRISE_ALBUM`,
  `REPRISE_FILE`, `REPRISE_STATE`, and so on. A user who wants a shell writes
  `run sh -c '...'` and uses the variables.
- **`run_in_terminal`** does the same for programs that need the terminal:
  reprise suspends the UI, runs the program, and redraws when it exits. It
  replaces ncmpcpp's `run_external_console_command`.
- **The hooks** (on song change, on player state change) use the same
  mechanism.

### Which-key panel

After a prefix key, a panel lists the possible next keys.
- **Placement.** The panel is attached to the bottom of the screen and grows
  upward from the status bar, which shows the prefix itself. It is drawn over
  the bottom rows of the main view instead of shrinking it, so the list doesn't
  jump.
- **Size.** The panel spans the full width, and entries fill as many columns
  as fit. Its height is just what the entries need, so a small group takes one
  line and looks like a status bar message. Paging is added only if a group
  ever outgrows the screen.
- **Entries.** An action shows its description from the registry, with its
  arguments filled in, e.g. `r  toggle repeat` or `+  change volume by +2`. A
  prefix shows `+` and the group's name.
  - A group gets its name from a reserved `name` entry in the YAML mapping. No
    key is called `name`, so it can't clash. A group without a name shows
    `+prefix`.
- **Layering.** The panel shows the screen's and the global keymaps merged.
  Entries that the screen shadows are hidden, and the screen's own entries are
  marked.
- **Timing.** The panel appears right away. If that flickers while typing
  sequences quickly, a delay becomes a config option. Then the delay is a user
  preference, not a hardcoded value.

The `:` prompt uses the same idea: as the user types, it shows the action's
usage or description (see [Prompts](#prompts)). Completing action names from
the registry comes later.

### Destructive actions

Every destructive action asks for confirmation, and no option turns that off.
An action is destructive if it throws away something the user didn't point at,
or if it changes a stored playlist, which lives on disk:

| Action | Why it asks |
|---|---|
| Clear the queue | Removes every song |
| Shuffle or sort the whole queue | Loses the current order |
| Delete a stored playlist | Removes it from disk |
| Save over an existing stored playlist | Replaces its contents |
| Clear a stored playlist, or remove songs from it | Changes it on disk |

Not destructive:
- deleting the selected or highlighted songs from the queue (the user pointed
  at them);
- shuffling or sorting only the selected songs;
- moving songs;
- playback controls.

Whether an action is destructive is part of its entry in the registry. The
confirmation is a `y/n` prompt in the status bar that names what will be lost,
e.g. "Clear 1243 songs from the queue? [y/n]". A test checks the pending
confirmation in the new state and runs its continuation to answer it.

### Find and filter

Find and filter always use ICU regular expressions, which follow Perl syntax,
and always ignore diacritics. There is no option for either.

`text-icu` (BSD-3-Clause; ICU 62 or newer through pkg-config) provides all
the pieces:
- **Regular expressions:** `regex'` from `Data.Text.ICU` checks a pattern,
  and `Data.Text.ICU.Regex` matches it (see the work limit below).
- **Diacritics folding:** decompose with `nfd` and drop the combining marks,
  in the pattern and in the text. Case is ignored with the `CaseInsensitive`
  option, because folding the pattern's case would turn escapes such as `\D`
  into others, here `\d`. A letter of its own, such as `ł`, stays.
- **"Ignore leading the" sorting:** strip the article, then compare with a
  `Collator` (`collator`, `collate`, `sortKey`).

Details for incremental find:
- **Compile with `regex'`, not `regex`.** While the user types, the pattern is
  often invalid for a moment, e.g. right after `(`. `regex'` returns an
  `Either`, so the status bar shows "incomplete pattern" and find resumes once
  the pattern is valid again.
- **Set a `WorkLimit`,** so a pattern with catastrophic backtracking stops with
  an error instead of freezing the UI on a long list. The limit belongs to a
  matcher, and text-icu's pure matching clones the matcher for every match,
  which loses the limit. So reprise matches with the IO interface, with one
  matcher for a whole search, under `unsafePerformIO` as text-icu's pure
  interface does.
- **An empty pattern is not a pattern.** ICU rejects it with an exception
  rather than a parse error, so the empty prompt of a find must not reach
  ICU.
- **The rows are kept between finds.** A find runs on every key, and without
  a match near its start it goes through the whole queue. The text of each
  row, with its diacritics folded, is made once, when a find first reaches
  it, and kept until the queue or its display changes. In a queue of 4254
  songs, this took a key that finds nothing from 6.9 ms to 1.8 ms. The
  browser keeps the rows of each listing the same way, from the listing
  until the next one or a new sort.

There is no plain-text mode. To match a literal string with special
characters, escape them (`AC/DC \(Live\)`) or quote it with ICU's `\Q...\E`.

## Format language

A format describes how a song is shown: in a list, in the status bar, in the
window title, in a column.

### Syntax

| Syntax | Meaning |
|---|---|
| `%{artist}` | A tag. Multi-value tags are joined with `lists.tag_separator` |
| `%{title:30}` | At most 30 terminal columns wide (wide characters count as 2), shortened with an ellipsis |
| `[ ... ]` | An optional section: printed only if every tag inside is present |
| `[ A \| B \| C ]` | Alternatives: the first one whose tags are all present, else nothing |
| `<red>...</>`, `<bold>`, `<red on black>`, `<green bold underline>` | A styled span. `</>` closes the innermost span; the parser checks that spans are balanced |
| `%%`, `%[`, `%]`, `%\|`, `%<` | A literal special character. `%` followed by any other character is an error, so an old-style `%a` gets a hint ("tags are written `%{artist}`") instead of printing a literal `a` |

Tag names: `artist`, `albumartist`, `album`, `title`, `track`, `track_raw`,
`disc`, `date`, `year`, `genre`, `composer`, `performer`, `comment`,
`priority`, `length`, `file`, `filename`, `directory`.
- `track` is the normalized number (`01` for `1/12`), like ncmpcpp's `%n`;
  `track_raw` is the tag as stored, like `%N`.
- `year` is the year part of `date`. It replaces ncmpcpp's `%4y` truncation
  trick.

Examples, the defaults for the status bar and for list rows:
```
[[%{artist}[ "%{album}"[ (%{year})]] - ]%{title}|%{filename}]
[%{artist} - ][%{title}|<white>%{filename}</>]
```

### Semantics

Rendering returns either "missing" or a list of styled text spans.
- A missing tag makes the enclosing sequence missing, up to the nearest
  `[...]`.
- `[...]` tries its alternatives in order. It renders the first one that is not
  missing, or nothing.
- A missing tag outside any `[...]` renders as `lists.missing_tag`, with
  `lists.missing_tag_style`. In the columns display, the marker takes the
  column's style instead, so that it looks like the rest of the column.

That is the whole algebra: one pass and no dry runs. Styles are ranges in the
syntax tree, so they can't be unbalanced.

Three more rules keep features out of the string:
- **Right alignment is structure.** A list row format is a YAML mapping with
  `left` and `right` formats.
- **Plain-text contexts can't contain styles.** The window title, the browser
  sort key and `--current-song` use `Format Void`: the syntax tree is
  parameterized by the style type, so a plain format can't contain a styled
  span. Their decoder rejects `<...>` with an error that points at it.
- **Columns use the same language.** A column's content is a format, so
  ncmpcpp's `{t|f}` column becomes `'[%{title}|%{filename}]'`.

Escaping uses `%` as its prefix rather than doubling each character. Doubled
brackets would be ambiguous: `[[` opens two nested sections and `]]` closes
two, and the default status bar format uses both. `%` already introduces tags
and `%%` is familiar from ncmpcpp. It also avoids `\`, which YAML
double-quoted strings treat as their own escape character.

### Why not ncmpcpp's language

What makes ncmpcpp's format language (`src/format.cpp`, `src/format_impl.h`)
convoluted:
- **Results have three values.** Each node returns Empty, Missing or Ok, and
  these combine by a table of rules. A group is evaluated twice: once without
  output to find its result, then again to print it. Nested groups repeat that
  at every level.
- **Alternatives are easy to misread.** `{A}|{B}` builds a FirstOf, but a
  single `{A}` is also a FirstOf with one element, and a missing tag inside it
  becomes Empty, not Missing. For example, the default `song_status_format` is
  `{{%a{ "%b"{ (%y)}} - }{%t}}|{%f}`. For a song with an artist but no title,
  it prints `Artist "Album" - ` instead of falling back to the filename.
- **Styles are commands on a stream, not ranges.** `$(red)` pushes a color and
  `$(end)` pops it; `$b`/`$/b` are reference-counted toggles. Nothing checks
  that they're balanced, and a group made only of colors silently prints
  nothing.
- **`$R` is a side channel.** It switches all later output to a second buffer.
  It counts as "Ok" inside groups and works only when printing a menu row.
- **Output targets are templates.** Writing to a string or a tag vector is a
  template specialization, and a style reaching those targets throws a
  `logic_error` at run time. Which features each option allows is a set of
  parse flags.
- **Width limits have special cases.** `%4y` truncates the date, which is the
  way to get a year. Length is truncated the same way, and every other tag is
  shortened with `..`.
- **There are two escapes,** `%%` and `$$`, and no way to escape `{`, `}` or
  `|`.

## Configuration

The config is a YAML file, `$XDG_CONFIG_HOME/reprise/config.yaml`, decoded with
[yamlet](https://github.com/arybczak/yamlet). A user's file only lists what it
changes; everything else has a default.

### Conventions

- **Options are grouped by the part of the UI they affect,** and named by what
  they do, not by how ncmpcpp implemented them.
- **Formats are strings** in the [format language](#format-language).
- **Every style option ends in `_style` or sits in `styles`.** A style is a
  space-separated list: an optional foreground color, attributes (`bold`,
  `underline`, `italic`, `reverse`), and `on <color>` for a background, e.g.
  `yellow on 24` or `black bold`. `default` means the terminal's own colors.
- **Colors** are names (`red`, `cyan`, ...) or numbers 0–255, the standard
  numbering of terminal color charts. ncmpcpp numbers them from 1.
- **Styles combine.** A row's style is its own style with the cursor,
  selection or playing style laid over it. Each one changes only what it sets,
  e.g. the background.
- **Durations** are written with a unit, e.g. `5s`.

### Defaults

The built-in defaults are the author's ncmpcpp setup: the author's ncmpcpp
config, plus ncmpcpp's defaults for every option it doesn't set. So an empty
config file gives the client the author uses today.

Only the options for features in the core are listed. Options of later
features get their defaults when the feature arrives, again from the author's
ncmpcpp config where it sets them.

```yaml
mpd: {}   # host, port, password, timeout (5s); see "MPD connection defaults"

startup_screen: queue
window_title: '[%{artist} - ][%{title}|%{filename}]'   # remove to disable

songs:                           # how a song looks in a list
  classic:
    left: '[%{artist} - ][%{title}|<white>%{filename}</>]'
    right: '<green>%{length}</>'
  columns:
    show_titles: false
    list:
    - {width: 20%, style: 221, format: '%{artist}'}
    - {width: 6, style: 77, format: '[%{track_raw}]'}
    - {width: 50%, style: white, format: '[%{title}|%{filename}]', title: Title}
    - {width: 20%, style: cyan, format: '%{album}'}
    - {width: 5, style: 203, format: '%{length}', align: right}

lists:                           # every list screen
  style: yellow
  cursor_style: yellow reverse
  inactive_cursor_style: yellow on 237   # the cursor in a column without focus
  selected_style: yellow on 24
  found_style: underline         # the matches while a find is typed
  playing_style: bold
  queued_style: bold             # songs in the queue, in the other screens
  keep_cursor_centered: false
  ignore_leading_the: false
  missing_tag: '<empty>'
  missing_tag_style: cyan        # also empty fields of the search engine and song info;
                                 # not in columns, which use their own style
  tag_separator: ' | '

queue:
  display: columns               # or classic
  album_separators: false
  follow_playing: false          # move the cursor to each new song
  show_remaining_time: false     # in the title, next to the total time

browser:
  display: classic
  sort:
    by: type                     # type, name, mtime, format or none
    format: '%{artist} - %{title}'
  playlist_prefix: '<red>playlist</> '

search_engine:
  display: classic

header:
  style: default
  title_style: bold              # ncmpcpp always bolds the title
  volume_style: default
  flags_style: bold
  line_style: default

status_bar:
  song: '[[%{artist}[ "%{album}"[ (%{year})]] - ]%{title}|%{filename}]'
  style: default
  state_style: bold
  time_style: bold
  show_remaining_time: false
  show_bitrate: false

progress_bar:
  chars: ▅▅▅                     # elapsed, the current position, remaining
  style: 236
  elapsed_style: 28

visualizer:
  data_source: ~                 # the fifo of MPD's fifo output; ~/ is the home directory
  in_stereo: true                # the fifo's format is 44100:16:2, or 44100:16:1
  visualization: spectrum        # or ellipse, which toggle visualization switches to
  fps: 60
  trail: 150ms                   # how long the samples of the ellipse stay
  colors: [46, 82, 118, 154, 190, 226, 220, 214, 208, 202, 196, 160]   # quiet to loud

lyrics:
  directory: ~                   # $XDG_DATA_HOME/reprise/lyrics; ncmpcpp's is ~/.lyrics
  fetchers: [lrclib, tekstowo]   # in order; [] shows only the stored lyrics
  fetch_in_background: false     # fetch the lyrics of each song that plays
  follow_playing: false          # show the lyrics of each song that plays

editor:
  command: ~                     # $VISUAL, else $EDITOR; sh runs it with the file

styles:                          # used across screens
  label: white                   # field names, e.g. in the search engine
  value: green                   # field values
  popup_border: green
```

A column's width is relative if it ends in `%` and fixed otherwise.

Options of later features get names in the same spirit, e.g.
`media_library.primary_tag`, `hooks.on_song_change`, `mouse.scroll_lines`,
`header.design`.

### MPD connection defaults

The author's MPD host, port and music directory are not defaults, because they
describe one machine. Users set their socket path in their own config.

Without a host in the config, reprise tries these in order:
1. `$MPD_HOST`/`$MPD_PORT`;
2. the usual socket locations, `$XDG_RUNTIME_DIR/mpd/socket` and
   `/run/mpd/socket`;
3. `localhost:6600`.

There is no music directory option yet. No core feature needs it, and file
deletion is never implemented. It comes back with the first feature that reads
files directly: the tag editor, or lyrics stored next to the song.

### Keymaps

Keymaps live in the same file as the rest of the config, under `keys`.
ncmpcpp's separate bindings file (and its `-b` flag) is gone.
- **One file is enough.** A user's file only lists what differs from the
  defaults, so both parts stay short.
- **One decoder,** one place for errors, one file to share.

A user's keymap is merged with the default one key by key:
- a key in the user's keymap replaces the default binding of that key;
- a prefix group in the user's keymap merges with the default group, so
  `ctrl-t: {y: toggle single}` adds one key to the toggles;
- `null` (written `~`) removes a default binding, e.g. `p: ~`, or a whole
  group, e.g. `ctrl-d: ~`.

The default keymap follows the [default keymap rule](#key-bindings):
- **Everyday actions keep ncmpcpp's single keys.**
- **Everything else is in prefix groups.** Each group is a Ctrl key whose
  letter names its topic, so a sequence reads like a short phrase: `ctrl-a n`
  is "add next", `ctrl-t r` is "toggle repeat".

Action names and arguments are a first draft; they settle when the actions
are written.

The defaults are a Haskell value, not YAML, so that the compiler checks
their actions. The block below shows them in the config's format. A test
checks that a config can name every default key.

```yaml
keys:
  global:
    # moving around
    up: move up
    down: move down
    page_up: move page_up
    page_down: move page_down
    home: move first
    end: move last
    "[": move previous_album
    "]": move next_album
    "{": move previous_artist
    "}": move next_artist
    o: jump_to_playing
    G: jump_to_browser           # the song under the cursor

    # selecting
    shift-up: select up          # toggle the selection, then move
    shift-down: select down
    insert: select

    # verbs; each screen implements them its own way
    enter: activate              # play, enter a directory, toggle an output
    space: add_or_remove         # add to the queue, or remove if already there
    delete: delete

    # playback
    p: pause
    s: stop
    "<": previous
    ">": next
    f: seek +1s                  # hold to go further
    b: seek -1s
    "+": volume +2
    "-": volume -2
    right: volume +2             # screens with columns move between columns
    left: volume -2

    # find and filter
    /: find forward
    "?": find backward
    .: find next
    ",": find previous
    ctrl-f: filter

    # screens; 4, 5 and 6 are kept for the media library, the playlist editor
    # and the tag editor
    1: show queue
    2: show browser
    3: show search_engine
    7: show outputs
    8: show visualizer
    l: show lyrics               # of the song under the cursor; on the lyrics, back
    tab: next_screen [browser, visualizer, media_library]
    shift-tab: previous_screen [browser, visualizer, media_library]
    f1: show help
    ":": command
    q: quit

    ctrl-a:
      name: add
      e: add end
      n: add next                # after the playing song
      b: add beginning
      p: add_and_play
      /: command add_path        # asks for the path in the : prompt
    ctrl-q:
      name: queue
      c: clear
      s: shuffle
      w: save                    # as a stored playlist
    ctrl-s:
      name: selection
      r: select range
      i: select invert
      c: select none
      a: select album
      A: select artist           # as { and } are the shifted [ and ]
      f: select found
    ctrl-t:
      name: toggle
      r: toggle repeat
      z: toggle random
      s: toggle single
      c: toggle consume
      x: toggle crossfade 5         # 0 or 5 seconds
      g: toggle replay_gain
      d: toggle display
      a: toggle album_separators
      f: toggle follow_playing
      b: toggle bitrate
    ctrl-p:
      name: playback
      r: replay
      s: command seek            # asks for m:ss, N% or ±Ns
      v: command volume
      x: command crossfade
    ctrl-d:
      name: database
      u: update current
      U: update all

  queue:
    space: select down           # as in the author's ncmpcpp bindings
    backspace: replay
    m: move_selection up
    n: move_selection down
    ctrl-q:                      # merged with the global ctrl-q group
      m: move_selection cursor
      e: move_selection end
      p: command priority        # asks for 0–255

  browser:
    backspace: parent
    ctrl-t:
      o: next_sort_mode

  visualizer:
    space: toggle visualization  # as in ncmpcpp

  lyrics:
    "`": refetch_lyrics          # as in ncmpcpp
    e: edit_lyrics               # in editor.command, $VISUAL or $EDITOR
    space: toggle follow_playing # of the lyrics here, as in ncmpcpp
```

Later features add to it: the queue's `ctrl-q o` (sort dialog), the media
library's `4` and left/right between columns, `ctrl-d r` (add random songs).
`ctrl-w` is reserved for the window framework, as in Vim, and stays unbound
until then. `ctrl-g` and `escape` cancel a pending prefix.

Compared to ncmpcpp:
- **Toggles left the single keys:** `r` `z` `y` `R` `x` `Y` `#` `P` `!` `U`
  are under `ctrl-t`.
- **Queue operations left the single keys:** `c` `Z` `M` `S` `ctrl-p` are
  under `ctrl-q`. `C` (crop) and `ctrl-r` (reverse) are gone.
- **Selection left the single keys:** `v` `V` `B` `ctrl-v` `ctrl-_` are under
  `ctrl-s`.
- **`shift-up`/`shift-down`** toggle the selection and move, without the
  binding chains ncmpcpp needs for it.
- **`g`, `u`, `A`, `a`, `w` and `e` are free** for later features.
- **`escape` is not bound to pause.** That was the author's own binding, and
  pausing by accident is a surprising result of trying to back out of
  something.

Two things to check early in milestone 2:
- **`ctrl-q` and `ctrl-s`** are flow control (XON/XOFF) in a terminal's default
  mode. vty switches the terminal to raw mode, so they should reach the app,
  the way `ctrl-s` already works in ncmpcpp.
- **`ctrl-a`** is GNU screen's prefix key, so screen users have to press it
  twice. tmux's default prefix is `ctrl-b`.

Many single-character keys collide with YAML syntax:
- An unquoted `~` is null and a digit is a number. The key decoder accepts
  number scalars, so `1:` works without quotes.
- These YAML indicators must be quoted: `-` `>` `|` `?` `:` `#` `!` `@` `%`
  `&` `*` `,` `[` `]` `{` `}` `` ` `` `'` `"`.

### Decoding

- Generic deriving with `yamlDefault` everywhere, so a user file only lists
  overrides.
- `rejectUnknownFields`, so typos are reported with yamlet's "did you mean"
  hints.
- Each config section's type has its own `yamlDefault`. When a section is
  present, yamlet takes its missing keys from the default of the section's
  type, not from the outer default.
- Hand-written decoders only for formats, styles, key specs and actions. They
  parse their own small languages, and their errors point at the YAML node.
- **Keymaps are decoded as overrides and merged afterwards.** yamlet's
  defaults fill in missing *fields of a record*. A keymap is a map with
  arbitrary keys, not a record, and a present map is decoded as a whole. So
  `keys.global` with one entry would decode to a keymap with one binding.
  Instead, the config holds the user's overrides, a map from key to an
  optional binding (`Nothing` for `~`). A pure function merges them with the
  default keymap, as described in [Keymaps](#keymaps). That keeps the
  standard map decoder, and the merge gets its own unit tests.

### ncmpcpp options

#### Renamed

| ncmpcpp | reprise |
|---|---|
| `mpd_host`, `mpd_port`, `mpd_password`, `mpd_connection_timeout` | `mpd.host`, `mpd.port`, `mpd.password`, `mpd.timeout` |
| `startup_screen` | `startup_screen` |
| `song_window_title_format` + `enable_window_title` | `window_title` |
| `song_list_format` | `songs.classic` (`left`, `right`) |
| `song_columns_list_format` | `songs.columns.list` |
| `titles_visibility` | `songs.columns.show_titles` |
| `main_window_color` | `lists.style` |
| `current_item_prefix`/`suffix` | `lists.cursor_style` |
| `current_item_inactive_column_prefix`/`suffix` | `lists.inactive_cursor_style` |
| `selected_item_prefix`/`suffix` | `lists.selected_style` |
| `now_playing_prefix`/`suffix` | `lists.playing_style` |
| `centered_cursor` | `lists.keep_cursor_centered` |
| `ignore_leading_the` | `lists.ignore_leading_the` |
| `empty_tag_marker`, `empty_tag_color` | `lists.missing_tag`, `lists.missing_tag_style` |
| `tags_separator` | `lists.tag_separator` |
| `playlist_display_mode` | `queue.display` |
| `playlist_separate_albums` | `queue.album_separators` |
| `autocenter_mode` | `queue.follow_playing` |
| `playlist_show_remaining_time` | `queue.show_remaining_time` |
| `browser_display_mode` | `browser.display` |
| `browser_sort_mode`, `browser_sort_format` | `browser.sort.by`, `browser.sort.format` |
| `browser_playlist_prefix` | `browser.playlist_prefix` |
| `search_engine_display_mode` | `search_engine.display` |
| `header_window_color`, `volume_color`, `state_flags_color`, `state_line_color` | `header.style`, `header.volume_style`, `header.flags_style`, `header.line_style` |
| `song_status_format` | `status_bar.song` |
| `statusbar_color`, `player_state_color`, `statusbar_time_color` | `status_bar.style`, `status_bar.state_style`, `status_bar.time_style` |
| `display_remaining_time`, `display_bitrate` | `status_bar.show_remaining_time`, `status_bar.show_bitrate` |
| `progressbar_look`, `progressbar_color`, `progressbar_elapsed_color` | `progress_bar.chars`, `progress_bar.style`, `progress_bar.elapsed_style` |
| `color1`, `color2`, `window_border_color` | `styles.label`, `styles.value`, `styles.popup_border` |
| `visualizer_data_source`, `visualizer_in_stereo`, `visualizer_type`, `visualizer_fps`, `visualizer_color` | `visualizer.data_source`, `visualizer.in_stereo`, `visualizer.visualization`, `visualizer.fps`, `visualizer.colors` |
| `lyrics_directory` | `lyrics.directory`, `$XDG_DATA_HOME/reprise/lyrics` without it |
| `lyrics_fetchers` | `lyrics.fetchers`, of `lrclib` and `tekstowo` (see [Core](#core)) |
| `fetch_lyrics_for_current_song_in_background` | `lyrics.fetch_in_background` |
| `follow_now_playing_lyrics` | `lyrics.follow_playing` |
| `external_editor` | `editor.command`, `$VISUAL` or `$EDITOR` without it. The author sets `mcedit` in their own config. `use_console_editor` is gone: the editor always gets the terminal |

How the author's ncmpcpp settings were translated:
- **Prefixes and suffixes became styles.** `current_item_prefix`/`suffix`
  (`$(yellow)$r` ... `$/r$9`), `current_item_inactive_column_*` and
  `selected_item_*` existed only to wrap rows in color codes.
- **Color numbers are one lower:** 222 → 221, 78 → 77, 204 → 203, 237 → 236,
  29 → 28, 25 → 24, 238 → 237.
- **Columns use formats.** `(6f)[78]{NE}` (fixed width, the full track tag, no
  empty-tag marker) became `{width: 6, style: 77, format: '[%{track_raw}]'}`;
  the brackets print nothing when the tag is missing. `{t|f:Title}` became
  `'[%{title}|%{filename}]'` with `title: Title`.

#### Dropped options

Options for dropped features go with them: the clock and split screen
options, `allow_for_physical_item_deletion`,
`show_hidden_files_in_local_browser`.

**The visualizer's other options**
- `visualizer_spectrum_dft_size` and `visualizer_spectrum_gain`: the
  author's values are named constants.
- `visualizer_spectrum_hz_min` and `visualizer_spectrum_hz_max`: the
  spectrum shows the range of human hearing.
- `visualizer_spectrum_log_scale_x`, `visualizer_spectrum_log_scale_y`,
  `visualizer_spectrum_smooth_look` and
  `visualizer_spectrum_smooth_look_legacy_chars`: the spectrum is always on
  log scales, of eighth blocks, with the upper blocks of Symbols for Legacy
  Computing, as the author has it.
- `visualizer_look`: the dots of the ellipse are braille, and the bars of
  the spectrum are blocks.
- `visualizer_output_name` and `visualizer_sync_interval`: the worker drops
  the old samples itself, instead of resetting the output.
- `visualizer_autoscale`: the size of the picture shows how loud the music
  is.
- `visualizer_data_source` with a UDP address, for Mopidy. reprise is a
  client of MPD.

**Dead or obsolete**
- `visualizer_fifo_path` (not registered any more) and `lyrics_db` (unused).
- `system_encoding`: UTF-8 everywhere.
- `data_fetching_delay`: lazy loading is asynchronous.
- `playlist_show_mpd_host`.
- The `noop` sort mode.
- `connected_message_on_startup`: reprise has no such message.

**Replaced by action arguments or keymap entries**
- `volume_change_step` → `volume +2`, `seek_time` → `seek +1s`,
  `mpd_crossfade_time` → `toggle crossfade 5`.
- `screen_switcher_mode` → `tab: next_screen [browser, visualizer, media_library]`.
- `space_add_mode` → two actions, `add` and `add_or_remove`; space is bound to
  `add_or_remove`.

**One choice is clearly right**

| Option | Fixed behavior |
|---|---|
| `regular_expressions` | Always ICU regular expressions (see [Find and filter](#find-and-filter)) |
| `ignore_diacritics` | Always ignored in find and filter |
| `ask_before_clearing_playlists`, `ask_before_shuffling_playlists` | Destructive actions always ask (see [Destructive actions](#destructive-actions)) |
| `default_find_mode` and its toggle action | Find wraps around and says so in the status bar |
| `show_duplicate_tags` | Duplicate tag values are removed |
| `jump_to_now_playing_song_at_start` | Always jump |
| `display_volume_level` | Volume is always shown |
| `playlist_shorten_total_times` | Totals are always short (`1h 23m`) |
| `block_search_constraints_change_if_items_found` | Never blocks |
| `default_place_to_search_in`, `search_engine_default_search_mode` | The form starts at database and "contains", and keeps the user's choice for the session |
| `message_delay_time` | A fixed timeout, as a named constant |
| `discard_colors_if_item_is_selected` | Styles combine (see [Conventions](#conventions)) |
| `colors_enabled` | The `NO_COLOR` environment variable turns colors off |
| `ncmpcpp_directory` | The XDG directory `$XDG_CONFIG_HOME/reprise` |
| `store_lyrics_in_song_dir` | Needs the music directory, which is gone |
| `generate_win32_compatible_filenames` | The names of lyrics files always leave out the characters that Windows forbids, as ncmpcpp does by default |
| `active_window_border` | Only split screens and the tag editor used it |
| `header_visibility`, `statusbar_visibility` | The header and the status bar are always shown |
| `cyclic_scrolling` | Moving past the last item stops there |
| `header_text_scrolling` | Long text always scrolls |
| `playlist_disable_highlight_delay` | The queue hides the cursor after a fixed 5 seconds without input, as a named constant |
| `incremental_seeking` | Seeking always works as described in [Core](#core) |

#### Not carried over yet

- **The options of other later features:** `lines_scrolled`,
  `mouse_list_scroll_whole_page`, `mpd_music_dir` and the tag editor's
  options.

## Testing

reprise has tests from the first commit. Every milestone includes tests for
what it adds, and every bug fix starts with a test that reproduces the bug.

The layers, from cheapest to most expensive:

1. **Pure code: unit, property and golden tests.** These are most of the
   suite.
   - The format language:
     - parser errors and their positions;
     - rendering a song to spans, including the missing-tag rules;
     - a property that printing a parsed format and parsing it again gives the
       same syntax tree.
   - Key specs, keymap lookup with layering and prefixes, and the which-key
     entries.
   - Config decoding: defaults, unknown fields, error messages.
   - List logic: selection, find, filter, scrolling, "ignore leading the"
     sorting.
   - The mirror: applying `plchanges` results and truncating to
     `playlistlength`.
2. **Actions: no MPD mock needed.** Actions only *request* MPD commands
   through the `MpdRequest` effect, and the requests are data. A test runs an
   action with a pure handler and checks two things:
   - the new `AppState`;
   - the list of requested commands, compared by their request lines.

   For an action that continues after a reply, such as the browser's
   listing of a directory, the test runs the continuation with a reply. It
   writes the reply's lines, and the request's own command parses them, so
   the test reads like a conversation with MPD. A failure runs the request's
   failure event instead. A prompt or a confirmation is tested the same way:
   the test finds the pending prompt in the new state and runs its
   continuation with an answer.
3. **`mpd-protocol`: parsers against recorded replies, and a real `mpd`.**
   - Parsers get golden tests on raw replies recorded from a real server:
     songs, status, `ACK` errors, command lists with `list_OK`.
   - An integration suite starts a real `mpd` in a temporary directory, with
     a `null` audio output and a few silent, tagged FLAC files. The `flac`
     command line tool generates them, with tags from its `-T` option. It runs
     every command the library supports.
   - A real server rather than a fake one: a fake would encode the same
     misunderstandings of the protocol as the parser it is supposed to check.
4. **Queue sync: a property test against the real `mpd`.** Keeping the mirror
   of the queue in sync is the stateful part most likely to break. The test:
   - generates a random sequence of queue operations (add, delete, move,
     shuffle, clear, priority);
   - sends them to the test server;
   - after each `idle` notification, checks that the mirror equals the
     server's `playlistinfo`.
5. **Screens: a few golden snapshots.** Render a screen at a fixed terminal
   size to text, without the styles, and compare it with a stored file. Only
   a handful, for layout
   regressions (the which-key panel, popups, the column widths). The pure
   pieces underneath already have their own tests.

The visualizer's worker reads a real fifo, which the test writes as MPD's
fifo output does. The lyrics' worker reads a temporary directory of lyrics.

There is no separate mock MPD interpreter. Layer 2 replaces it for actions,
and the real server covers the protocol. A scripted `MpdRequest` handler is
added only if a multi-step flow ever needs one. It would hold a list of
(expected request, raw reply) pairs and parse each reply with the real parser.

Tools:
- `tasty` with `tasty-hunit` (`assertEqual` and friends),
- `tasty-golden`,
- `tasty-quickcheck` for properties.

CI installs `mpd` and `flac` for layers 3 and 4 (see [CI](#ci)).

### Benchmarks

Two `tasty-bench` suites measure what was slow once, on a queue of 4254
songs and a terminal of 160×45:
- **`mpd-protocol-bench`** parses the reply to `playlistinfo` of the whole
  queue, and to `plchanges` after a delete in the middle, which has every
  song after the deleted one.
- **`reprise-bench`** handles a key, the change of the queue after such a
  delete, and the keys of a find that matches nothing, which goes through the
  whole queue. It handles the listing of a directory as long as the queue,
  sorted by name. It also makes a frame of the queue, and vty writes a frame
  for xterm-256color, which covers the pin of vty-unix. It makes and writes
  a frame of each visualization after a second of noise, which it draws 60
  times a second, and computes the spectra of the channels for a frame.

CI builds the suites but doesn't run them, because timings on shared
machines are too noisy to fail a build on. To check a change, save the
timings of a suite before it and compare after, e.g. for `reprise-bench`:

```
cabal bench reprise-bench --benchmark-options='--csv reprise.csv'
cabal bench reprise-bench --benchmark-options='--baseline reprise.csv --fail-if-slower 20'
```

A benchmark that is more than 20% slower than in the saved file fails. Each
suite needs a file of its own, because the suites write the same file
otherwise.

## Project structure and conventions

The repository follows the
[effectful](https://github.com/haskell-effectful/effectful) repository, which
has the same author.

### Layout

```
reprise/                          repository root
├── cabal.project                 the package, the yamlet and vty-unix pins
├── reprise.cabal
├── fourmolu.yaml                 copied from effectful
├── .gitignore                    copied from effectful
├── .github/
│   ├── haskell-gha.conf.yml
│   └── workflows/haskell-gha.yml generated by haskell-gha
├── CHANGELOG.md, LICENSE, README.md
├── DESIGN.md                     this document
├── app/Main.hs
├── bench/
│   ├── mpd-protocol/Main.hs
│   └── reprise/Main.hs
├── cbits/                        width.c, clock.c, spectrum.c, pocketfft/ (vendored)
├── src/
│   ├── mpd-protocol/             README.md, Reprise/Mpd/Protocol/...
│   └── reprise/Reprise/...
└── tests/
    ├── mpd-protocol/             Main.hs, ...Tests.hs, recorded replies
    ├── mpd-test-server/Reprise/Mpd/TestServer.hs
    └── reprise/                  Main.hs, ...Tests.hs, golden files
```

The reprise repository is public on GitHub.

yamlet is not on Hackage yet. Until it is, `cabal.project` pulls it in as a
`source-repository-package` at a fixed commit. The remote repository has no
tags, so the pin is the newest commit of its `master` at the time of pinning.
CI can only fetch it if the yamlet repository is public too, which it is.

vty-unix is pinned the same way, to a fork that evaluates escape sequences
faster. vty-unix evaluates one for every color change and every row, and its
evaluator built the output a byte at a time, which made scrolling a list cost
about 40% more CPU. The fix is sent upstream; the pin goes once a release of
vty-unix has it. Caching the sequences of colors and row starts instead was
as fast for reprise's frames, but it only helps the sequences that it covers.

### Cabal files

These are copied from `effectful-core.cabal`:
- `cabal-version: 3.8`, `license: BSD-3-Clause`, `bug-reports` and
  `source-repository head`.
- A `common language` stanza that every component imports:
  - `ghc-options: -Wall -Wcompat -Werror=missing-deriving-strategies
    -Werror=prepositive-qualified-module`;
  - `default-language: GHC2021`;
  - `default-extensions`: `DataKinds`, `DeepSubsumption`,
    `DerivingStrategies`, `DuplicateRecordFields`, `LambdaCase`,
    `NoFieldSelectors`, `NoStarIsType`, `OverloadedRecordDot`,
    `RoleAnnotations`, `TypeFamilies`, `UndecidableInstances`.
- Lower bounds on the dependencies of the libraries. The test suites list
  their dependencies without bounds.
- A test suite for each library, `mpd-protocol-tests` and `reprise-tests`:
  `type: exitcode-stdio-1.0`, `main-is: Main.hs`, `hs-source-dirs` in
  `tests`, with every test module listed in `other-modules`.

Settings that effectful doesn't have:
- `tested-with: GHC ^>= { 9.6, 9.8, 9.10, 9.12, 9.14 }`. effectful supports
  the same range, and yamlet supports a wider one.
- `OverloadedStrings`, because reprise and mpd-protocol work with `Text`
  everywhere (yamlet, brick, the protocol).
- `OverloadedLabels`, for the optics labels (see [Code](#code)).
- `MultiWayIf`, for conditions that guards in a `case` would only make
  longer.
- `DeriveAnyClass`, for the `NFData` instances of `mpd-protocol`. Deriving
  strategies are always explicit, so it is unambiguous.
- `StrictData`. yamlet's `requiredField` throws when it is evaluated, so a
  config field whose `yamlDefault` uses it must be marked lazy with `~`. This
  is rare, because every config field has a real default.
- `ghc-options: -threaded` for the `reprise` executable and both test suites.
  The MPD worker threads and the real-server tests need it.
- `-with-rtsopts=-I0` for the `reprise` executable, which turns off the idle
  garbage collection of the threaded runtime. While a song plays, reprise
  wakes about once a second to show the time, and each wake was followed by a
  collection of the whole heap. In 30 seconds of playback with a queue of
  4254 songs, this took reprise from 101 ms of CPU to 39 ms. Memory freed in
  a burst of work is collected by the next regular collection instead.

### Code

- **Imports.** Import whole modules. Qualified imports use the postpositive
  form (`import Data.Map.Strict qualified as M`); the `prepositive` warning is
  an error.
- **Exports.** Every module has a header comment (`-- | ...`) and an explicit
  export list, split into Haddock sections.
- **Records.** Read fields with `OverloadedRecordDot` (`state.queue`). Update
  them with the generic optics from `optics-core`, e.g.
  `state & #queue % #display .~ Columns`, rather than record update syntax.
  With `DuplicateRecordFields`, record updates often trigger ambiguity
  warnings. The record types derive `Generic`, and `optics-core` 0.4 or newer
  turns that into `#field` labels without Template Haskell. The labels need
  `OverloadedLabels`.
- **Effects.** Each effect lives in its own module, laid out like
  `Effectful.Reader.Static`:
  - export sections `-- * Effect`, `-- ** Handlers`, `-- ** Operations`;
  - dynamic effects as `data E :: Effect where ...` with
    `type instance DispatchOf E = Dynamic`, and operations defined with
    `send`;
  - handlers follow the usual rules: only the new effect in the argument, a
    polymorphic tail, extra effects in the context.
- **Documentation.**
  - Argument docs with `-- ^` on the line under the argument.
  - No `@since` annotations: reprise is an application, and `mpd-protocol`
    is private. It gets them if it becomes a package of its own.
- **Internal modules.** In `mpd-protocol`, modules that are exposed only for
  the tests live under `Reprise.Mpd.Protocol.Internal.*` and carry
  `{-# OPTIONS_HADDOCK not-home #-}`.
- **Tests.** Laid out like `effectful/tests`:
  - `Main.hs` collects one `TestTree` from each `XTests` module
    (`module XTests (xTests) where`);
  - each test case is a top-level `test_name :: Assertion`, named in a
    `testGroup` list at the top of the module;
  - helpers go at the bottom, under a `-- Helpers` banner.
- **Changelog.** `CHANGELOG.md` has `# reprise-<version> (<date>)` headings
  and one bullet for each user-visible change.

### Formatting

- **fourmolu** formats every Haskell file. Its settings are effectful's
  `fourmolu.yaml`: 2-space indent, 90 columns, leading arrows, commas and
  import lists, single-line Haddock.
- **The version is pinned to 0.20.1.0,** the one the author uses. A newer
  version can format the same code differently.
- **The code is formatted from the first commit.** The effectful sources
  themselves don't all pass `fourmolu --mode check`; reprise starts clean and
  stays clean.

### CI

[haskell-gha](https://github.com/arybczak/haskell-gha) generates the workflow
from `.github/haskell-gha.conf.yml`:

```yaml
branches:
- master
apt: [mpd, flac, libicu-dev]
fourmolu:
  enabled: true
  version: 0.20.1.0
```

The workflow builds and tests every GHC version in `tested-with`, runs
`cabal check` and builds the Haddock documentation. It also runs a separate
fourmolu job. `mpd` and `flac` are for the protocol and queue sync tests, and
`libicu-dev` is for `text-icu`.

## Milestones

1. **Repository and `mpd-protocol` basics.**
   - The repository skeleton: cabal files, `fourmolu.yaml`, the haskell-gha
     config, the test layout.
   - Connection, response parser, `Command` with command lists.
   - Commands: `status`, `currentsong`, `plchanges`, `idle`, playback, volume.
   - The integration test harness with a real `mpd`.
2. **Something to listen with.**
   - Config loading, styles and the format language.
   - Header, status bar and progress bar.
   - Queue screen with classic and columns display (the author's queue uses
     columns).
   - The queue sync property test.
   - Keymaps with key sequences and the which-key panel, with bindings for
     playback.
   - From here on, reprise replaces ncmpcpp for the author's daily use.
   - Moved to milestone 3: album separators and the password prompt. The
     selection in the queue, moving the selected songs, priority, the line
     prompts and find in the queue came after milestone 2.
3. **Library navigation.**
   - Browser, including stored playlists.
   - Selection, find, filter, prompts, add and play.
4. **Feature parity for the core.**
   - Search engine, columns display in the other screens.
   - Outputs, the `:` prompt with completion. The help screen came with
     milestone 2.
5. **Later features**, in the order the author misses them.

## Postponed decisions

- **The media library's mtime sort** gets redesigned when the media library
  arrives.
- **Reloading the config while reprise runs** is not planned for the core. A
  restart is cheap, and the screens' state comes back from MPD.
- **Scripting** is not planned. The action registry is what a scripting layer
  would expose (see [Key bindings](#key-bindings)).
- **The memory of songs** waits until it matters. With a queue of 4254 songs,
  reprise kept 6.2 MB of live data, about 1.5 KB per song, and about 47–50 MB
  resident, which ncmpcpp uses too (55 MB). The live data was about 3 MB of
  strings and 1.9 MB of tag maps and lists; GHC keeps a heap of about twice
  the live data.
  - **Sharing equal tag values** comes with the media library, whose
    `listallinfo` loads the whole database. The parser keeps a table of the
    values of one reply, so that each artist, album, date or genre is decoded
    and stored once. Measure on a real database first.
  - **Reusing songs in the mirror** comes with the queue operations of
    milestone 3. A move sends every song whose position changed in
    `plchanges`, as new values. The mirror can keep its old song when only
    the position differs, which keeps the sharing and saves allocations.
  - **Tuning GHC's runtime** (nursery size, heap growth factor) is not
    planned. Each setting is a constant that needs a reason, for a few
    megabytes at most.
