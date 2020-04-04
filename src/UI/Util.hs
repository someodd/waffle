-- | Honestly, this is sloppily just a catchall for things many UI modules use. Should be sorted
-- soon/later.
module UI.Util where

import qualified Brick.BChan
import qualified Data.Vector                   as Vector

import           Brick.Main                     ( ViewportScroll
                                                , viewportScroll
                                                )
import qualified Brick.Widgets.List            as BrickList -- (List)? FIXME

import           GopherClient
import           UI.Representation

-- FIXME: more like makeState from menu lol. maybe can make do for any state
-- based on passing it the mode and other info! newStateForMenu?
--
-- probs needs to be IO
newStateForMenu
  :: Brick.BChan.BChan CustomEvent
  -> GopherMenu
  -> Location
  -> History
  -> GopherBrowserState
newStateForMenu chan gm@(GopherMenu ls) location history = GopherBrowserState
  { gbsBuffer     =
    MenuBuffer
      $ Menu (gm, BrickList.list MyViewport glsVector 1, mkFocusLinesIndex gm)
  , gbsLocation   = location
  , gbsHistory    = history
  , gbsRenderMode = MenuMode
  , gbsChan       = chan
  , gbsPopup      = Nothing
  }
 where
  glsVector = Vector.fromList $ map lineShow ls
  mkFocusLinesIndex (GopherMenu m) =
    map fst $ filter (not . isInfoMsg . snd) (zip [0 ..] m)

  -- | Used for filling up a list with display strings.
  lineShow :: Either GopherLine MalformedGopherLine -> String
  lineShow line = case line of
    -- It's a GopherLine
    (Left gl) -> case glType gl of
      -- Canonical type
      (Left _) -> clean $ glDisplayString gl
      -- Noncanonical type
      (Right nct) ->
        if nct == InformationalMessage && clean (glDisplayString gl) == ""
          then " "
          else clean $ glDisplayString gl
    -- It's a MalformedGopherLine
    (Right mgl) -> clean $ show mgl

-- FIXME: Is this appropriate for here? maybe another module?
-- | Replaces certain characters to ensure the Brick UI doesn't get "corrupted."
clean :: String -> String
clean = replaceTabs . replaceReturns
 where
  replaceTabs    = map (\x -> if x == '\t' then ' ' else x)
  replaceReturns = map (\x -> if x == '\r' then ' ' else x)

myNameScroll :: ViewportScroll MyName
myNameScroll = viewportScroll MyViewport
