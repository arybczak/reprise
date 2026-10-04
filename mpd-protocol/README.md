# mpd-protocol

A client library for the protocol of the
[Music Player Daemon](https://www.musicpd.org) (MPD), version 0.23 or newer.

- Commands are typed values that keep their request lines as plain data, so
  tests can compare them without a server.
- Commands combine with the `Applicative` interface into command lists, which
  run in a single round trip:

  ```haskell
  import MPD.Command
  import MPD.Connection

  example :: Settings -> IO ()
  example settings = do
    r <- withConnection settings $ \conn ->
      run conn $ (,) <$> status <*> currentSong
    print r
  ```

- `idle` and `noidle` are in `MPD.Idle`.
- The API is plain `IO`. The operations throw `MpdError` when they fail.
