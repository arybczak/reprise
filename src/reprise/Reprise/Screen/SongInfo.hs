-- | The song info screen: the file, the audio, the ReplayGain and the tags
-- of a song, as text that scrolls.
module Reprise.Screen.SongInfo
  ( songInfoView
  , songInfoVerb
  , showSongInfo
  , songCommentsFetched
  ) where

import Control.Monad
import Data.Maybe
import Data.Text qualified as T
import Effectful
import Graphics.Vty qualified as V
import Optics.Core

import Reprise.Action
import Reprise.Config
import Reprise.Effect.MpdRequest
import Reprise.Event
import Reprise.Handler.Core
import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.Style

-- | The labels have 'styles.label', the values 'styles.value', and a tag
-- that the song is without is the missing tag, as in the lists.
songInfoView :: AppEnv -> AppState -> View -> V.Image
songInfoView env s v =
  V.vertCat . map row . take v.height . drop v.offset $ songInfoRows env s v.width
  where
    row :: (T.Text, Maybe T.Text) -> V.Image
    row (label, value) =
      V.horizCat
        [ V.text' (toAttr env.colorMode env.config.styles.label) label
        , case value of
            Just text -> V.text' (toAttr env.colorMode env.config.styles.value) text
            Nothing ->
              V.text'
                ( toAttr env.colorMode $
                    fromMaybe env.config.styles.value env.config.lists.missingTagStyle
                )
                env.config.lists.missingTag
        ]

-- | How the song info screen does a verb, if it does it.
songInfoVerb :: App es => Action -> Maybe (Eff es ())
songInfoVerb = \case
  Move t -> Just $ scrollLines t
  _ -> Nothing

-- | Show the info of the song under the cursor, from the top.
showSongInfo :: App es => Eff es ()
showSongInfo = showSongScreen SongInfoScreen $ \song -> do
  token <- newToken
  modifyS $ #songInfo .~ SongInfoState (Just song) token []
  -- A stream has no file to read, and its URL would be opened.
  unless (isStream song) $
    requestOr
      (readComments song.file)
      (const (SongCommentsFetched token []))
      (SongCommentsFetched token)
  pure True

-- | The comments of the file of the request with the token. Without them,
-- e.g. if MPD can't read the file, the screen shows the rest.
songCommentsFetched :: App es => Int -> [(T.Text, T.Text)] -> Eff es ()
songCommentsFetched token comments =
  whenCurrent (Just . (.songInfo)) (.token) token $ \_ ->
    modifyS $ #songInfo % #comments .~ comments
