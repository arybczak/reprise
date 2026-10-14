module Reprise.Test.LocalFiles (localFilesTests) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Posix.Files
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.LocalFiles
import Reprise.Mpd.Protocol.Types
import Reprise.Mpd.TestServer
import Reprise.TagLib

localFilesTests :: TestTree
localFilesTests =
  testGroup
    "local files"
    [ testCase "the suffixes of MPD's decoders" test_suffixes
    , testCase "which files are music" test_isMusic
    , testCase "TagLib's names as MPD's" test_mpdTags
    , testCase "the entries of a directory" test_entries
    , testCase "a directory that is gone" test_missing
    , testCase "walking directories" test_walk
    ]

-- | FFmpeg's plugin lists suffixes of formats that aren't audio too, so its
-- own are apart from those of the other plugins.
test_suffixes :: Assertion
test_suffixes =
  assertEqual
    "trusted and FFmpeg's only"
    Suffixes {trusted = S.fromList ["flac", "mp3"], ffmpegOnly = S.fromList ["m4a", "txt"]}
    ( suffixesOf
        [ Decoder {plugin = "flac", suffixes = ["flac"]}
        , Decoder {plugin = "mad", suffixes = ["MP3"]}
        , Decoder {plugin = "ffmpeg", suffixes = ["flac", "m4a", "txt"]}
        ]
    )

test_isMusic :: Assertion
test_isMusic = do
  let tags = Just FileTags {properties = [], seconds = Nothing}
  assertBool "a trusted suffix" (isMusic suffixes "a.FLAC" Nothing)
  assertBool "FFmpeg's suffix that TagLib reads" (isMusic suffixes "a.m4a" tags)
  assertBool
    "FFmpeg's suffix that TagLib doesn't read"
    (not (isMusic suffixes "a.txt" Nothing))
  assertBool "no suffix" (not (isMusic suffixes "flac" Nothing))
  assertBool "another suffix" (not (isMusic suffixes "a.jpg" tags))

-- | MPD maps some names of Vorbis comments, and leaves out the names that
-- it doesn't know.
test_mpdTags :: Assertion
test_mpdTags =
  assertEqual
    "the tags"
    ( M.fromList
        [(Artist, ["A", "B"]), (AlbumArtist, ["C"]), (Track, ["1/9"]), (MovementNumber, ["2"])]
    )
    ( mpdTags
        FileTags
          { properties =
              [ ("ARTIST", ["A", "B"])
              , ("ALBUMARTIST", ["C"])
              , ("TRACKNUMBER", ["1/9"])
              , ("MOVEMENTNUMBER", ["2"])
              , ("ENCODER", ["x"])
              ]
          , seconds = Nothing
          }
    )

-- | MPD's @listfiles@ leaves out symbolic links, which is why reprise lists
-- local directories itself. TagLib reads the tags of the files whose
-- suffixes MPD decodes. A FIFO is left out, as its open would wait for a
-- writer.
test_entries :: Assertion
test_entries = withSystemTempDirectory "local" $ \dir -> do
  createDirectory (dir </> "album")
  writeSong dir TestSong {path = "song.flac", tags = [("ARTIST", "A")], seconds = 2}
  BS.writeFile (dir </> "notes.txt") "not a song"
  BS.writeFile (dir </> ".hidden") ""
  createDirectoryLink "album" (dir </> "linked")
  createFileLink "song.flac" (dir </> "linked.flac")
  createFileLink "nowhere" (dir </> "broken")
  createNamedPipe (dir </> "pipe.flac") ownerModes
  listed <-
    maybe (assertFailure "the listing waits") pure
      =<< timeout 10000000 (listLocalDirectory suffixes dir)
  assertEqual
    "the names, whether they are directories, and the tags, by name"
    ( Right
        [ (".hidden", False, Nothing)
        , ("album", True, Nothing)
        , ("linked", True, Nothing)
        , ("linked.flac", False, Just song)
        , ("notes.txt", False, Nothing)
        , ("song.flac", False, Just song)
        ]
    )
    (map (\f -> (f.name, f.isDirectory, f.tags)) <$> listed)
  where
    song :: FileTags
    song = FileTags {properties = [("ARTIST", ["A"])], seconds = Just 2}

test_missing :: Assertion
test_missing = withSystemTempDirectory "local" $ \dir ->
  assertEqual "gone" (Left DirectoryMissing)
    =<< listLocalDirectory suffixes (dir </> "gone")

-- | A link back to a parent would walk forever. A directory that an earlier
-- one walked through has nothing left. A FIFO is left out, as TagLib's open
-- of it would wait for a writer.
test_walk :: Assertion
test_walk = withSystemTempDirectory "local" $ \dir -> do
  let album = dir </> "album"
  mapM_
    (writeSong album . testSong)
    ["b.flac", "a.flac", "cd2" </> "c.flac", ".hidden" </> "d.flac"]
  BS.writeFile (album </> "notes.txt") "not a song"
  BS.writeFile (album </> "cover.jpg") ""
  createDirectoryLink ".." (album </> "cd2" </> "up")
  createNamedPipe (album </> "pipe.m4a") ownerModes
  walked <-
    maybe (assertFailure "the walk waits") pure
      =<< timeout 10000000 (walkLocalDirectories suffixes [album, album </> "cd2", dir </> "gone"])
  assertEqual
    "the music files of each directory, depth first, by name"
    [ [album </> "a.flac", album </> "b.flac", album </> "cd2" </> "c.flac"]
    , []
    , []
    ]
    walked

-- | FLAC's suffix, and FFmpeg's, which include text files.
suffixes :: Suffixes
suffixes = Suffixes {trusted = S.fromList ["flac"], ffmpegOnly = S.fromList ["m4a", "txt"]}
