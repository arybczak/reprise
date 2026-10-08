# Coming from ncmpcpp

reprise follows ncmpcpp's screens and most of its keys, so an ncmpcpp
user finds their way around quickly. This page lists what is different.

## Configuration

reprise reads one YAML file, `~/.config/reprise/config.yaml`
(`$XDG_CONFIG_HOME/reprise/config.yaml`). It has the options and the key
bindings, so ncmpcpp's separate bindings file is gone. Your file lists only
what you change; everything else keeps its default.
[config.yaml](config.yaml) lists the options with their defaults.

reprise doesn't read ncmpcpp's files. The tables below say what each
option became.

The defaults are close to a typical ncmpcpp setup, so an empty file is a
good start.

## Keys

The everyday keys are ncmpcpp's: `p`, `s`, `<`, `>`, `f`, `b`, `+`, `-`,
`enter`, `space`, `delete`, `/`, `?`, `.`, `,`, `1`, `2`, `7`, `8`, `l`,
`i`, `o`, `M`, `V`, `tab`, `f1`, `q`. The rest moved into groups of keys: press the
first key, and a panel at the bottom lists what can follow it. `f1` lists
every key.

| ncmpcpp | reprise | |
|---|---|---|
| `r`, `z`, `y`, `R` | `t r`, `t z`, `t s`, `t c` | toggle repeat, random, single, consume |
| `x`, `Y` | `t x`, `t g` | toggle crossfade, ReplayGain |
| `#`, `P` | `t b`, `t d` | toggle the bitrate, the display mode |
| `U` | `t f` | follow the playing song, in the queue and the lyrics |
| `c` | `e c` | clear the queue; it asks first |
| `Z` | `e s` | shuffle the queue, or the selected songs |
| `S` | `e w` | save the queue, or the selection, as a stored playlist |
| `ctrl-p` | `e p` | set the priority of songs |
| `v` (reverse the selection) | `v i` | |
| select the album, a range, the found items | `v a`, `v r`, `v f` | |
| `G` | `g b` | the song under the cursor in the browser |
| `g` | `g s` | seek to a position |
| `e` on the lyrics screen | `e e` | edit the lyrics |

Keys that are new in reprise:

- `ctrl-q` quits from anywhere, also in a prompt, and can't be rebound.
- `shift-up` and `shift-down` select songs while moving.
- `e m e`, `e m b` and `e m n` move songs to the end, to the beginning and
  after the playing song. `a e`, `a b` and `a n` add songs
  to the same places, and `a p` adds and plays them.
- `v A` selects the songs of the artist around the cursor.
- `:` runs an action by its name, e.g. `:volume 50` or `:add_path dir/x.flac`.
- `escape` goes back from the lyrics, the song info and the help.

Crop (`C`) and reversing the queue (`ctrl-r`) are gone.

## Options

| ncmpcpp | reprise |
|---|---|
| `mpd_host`, `mpd_port`, `mpd_password`, `mpd_connection_timeout` | `mpd.host`, `mpd.port`, `mpd.password`, `mpd.timeout` |
| `startup_screen` | `startup_screen` |
| `song_window_title_format`, `enable_window_title` | `window_title` |
| `song_list_format` | `songs.classic.left`, `songs.classic.right` |
| `song_columns_list_format` | `songs.columns.list` |
| `titles_visibility` | `songs.columns.show_titles` |
| `main_window_color` | `lists.style`, `styles.text` |
| `current_item_prefix`, `current_item_suffix` | `lists.cursor_style` |
| `selected_item_prefix`, `selected_item_suffix` | `lists.selected_style` |
| `now_playing_prefix`, `now_playing_suffix` | `lists.playing_style` |
| `centered_cursor` | `lists.keep_cursor_centered` |
| `ignore_leading_the` | `lists.ignore_leading_the` |
| `empty_tag_marker`, `empty_tag_color` | `lists.missing_tag`, `lists.missing_tag_style` |
| `tags_separator` | `lists.tag_separator` |
| `playlist_display_mode` | `queue.display` |
| `autocenter_mode` | `queue.follow_playing` |
| `playlist_show_remaining_time` | `queue.show_remaining_time` |
| `browser_display_mode` | `browser.display` |
| `browser_sort_mode`, `browser_sort_format` | `browser.sort.by`, `browser.sort.format` |
| `browser_playlist_prefix` | `browser.playlist_prefix` |
| `header_window_color`, `volume_color`, `state_flags_color`, `state_line_color` | `header.style`, `header.volume_style`, `header.flags_style`, `header.line_style` |
| `song_status_format` | `status_bar.song` |
| `statusbar_color`, `player_state_color`, `statusbar_time_color` | `status_bar.style`, `status_bar.state_style`, `status_bar.time_style` |
| `display_remaining_time`, `display_bitrate` | `status_bar.show_remaining_time`, `status_bar.show_bitrate` |
| `progressbar_look`, `progressbar_color`, `progressbar_elapsed_color` | `progress_bar.chars`, `progress_bar.style`, `progress_bar.elapsed_style` |
| `color1`, `color2` | `styles.label`, `styles.value` |
| `visualizer_data_source`, `visualizer_type`, `visualizer_fps`, `visualizer_color` | `visualizer.data_source`, `visualizer.visualization`, `visualizer.fps`, `visualizer.colors` |
| `lyrics_directory` | `lyrics.directory` |
| `lyrics_fetchers` | `lyrics.fetchers`: `lrclib` and `tekstowo` |
| `fetch_lyrics_for_current_song_in_background` | `lyrics.fetch_in_background` |
| `follow_now_playing_lyrics` | `lyrics.follow_playing` |
| `external_editor` | `editor.command`, else `$VISUAL` or `$EDITOR` |
| `volume_change_step`, `seek_time`, `mpd_crossfade_time` | the arguments of the keys: `volume +2`, `seek +1s`, `toggle crossfade 5` |
| `screen_switcher_mode` | `tab: next_screen [queue, browser]`, with the screens to go through |
| `space_add_mode` | `add` and `add_or_remove` are two actions; `space` adds or removes |

What changed in how they are written:

- **Styles replace prefixes and suffixes,** e.g. `cursor_style: yellow
  reverse` instead of `$(yellow)$r` and `$/r$9`. A style is a color,
  attributes and a background: `yellow on 24`, `bold`, `"#ff8700"`.
- **Colors are numbered from 0,** as terminals number them. ncmpcpp
  numbers them from 1, so ncmpcpp's 222 is reprise's 221.
- **Formats use names:** `%{artist}` instead of `%a`, `<red>...</>` for a
  style, `[...]` for a part that shows only if its tags have values, and
  `[a|b]` for the first that does. A column of `songs.columns.list` is
  e.g. `{width: 20%, style: 221, format: '%{artist}'}`.
- **The lyrics directory** is `~/.local/share/reprise/lyrics` by default.
  Set `lyrics.directory: ~/.lyrics` to keep ncmpcpp's.

Options whose choice is fixed in reprise:

- Find uses regular expressions and ignores diacritics and case.
- Clearing and shuffling the whole queue always ask first.
- Colors are on unless the `NO_COLOR` environment variable is set.
- The header, the status bar and the volume always show.
- The cursor jumps to the playing song when reprise starts.
- Duplicate values of a tag show once.

## Not available yet

The search engine, the media library, the playlist editor, the tag editors,
album separators in the queue, the server info, mouse support, and
commands that run on song change. The browser already opens stored
playlists and loads them, and `e w` saves them.

The clock screen and split screens are gone.

Filtering lists (`ctrl-f`) is gone too. To act on every match, find it
with `/` and select every match with `v f`; any action then works on all
of them, e.g. `delete` or `e m e`.
