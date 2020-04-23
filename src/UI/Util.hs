-- | Honestly, this is sloppily just a catchall for things many UI modules use. Should be sorted
-- soon/later.
module UI.Util where

import           Data.Maybe
import qualified Data.Vector                   as Vector
import qualified Data.Map                      as Map

import qualified Brick.Widgets.Edit            as B
import qualified Brick.BChan                   as B
import qualified Brick.Main                    as B
import qualified Brick.Types                   as B
import           Brick.Widgets.Center          as B
import           Brick.Widgets.Core            as B
import           Brick.Widgets.Border          as B
import           Brick.AttrMap                 as B
import qualified Graphics.Vty                  as V
import           Brick.Main                     ( ViewportScroll
                                                , viewportScroll
                                                )
import qualified Brick.Widgets.List            as BrickList -- (List)? FIXME

import           GopherClient
import           UI.Representation
import           UI.Popup
import           UI.Style

makePopupWidget :: GopherBrowserState -> B.Widget MyName
makePopupWidget gbs = 
  B.centerLayer $ head $ popup (pLabel .fromJust $ gbsPopup gbs) (pWidgets . fromJust $ gbsPopup gbs) (pHelp . fromJust $ gbsPopup gbs)

-- FIXME: okay so this is nice and all but how will we handle editor input once activated? how do we tell it's activated?
-- FIXME: poppys and statusWidget both relevant!
-- I need a better name for this, but basically it's the default view you see
-- for everything! There are only a few exceptions.
--defaultBrowserUI :: GopherBrowserState -> B.Viewport -> B.Widget MyName -> B.Widget MyName -> B.Widget MyName -> [B.Widget MyName]
defaultBrowserUI gbs mainViewport titleWidget mainWidget statusWidget = [makePopupWidget gbs | hasPopup gbs] ++ [hCenter $ vCenter view]
 where
  box =
    updateAttrMap (B.applyAttrMappings borderMappings)
      $ withBorderStyle customBorder
      $ B.borderWithLabel (withAttr titleAttr titleWidget)
      $ mainViewport
      $ hLimitPercent 100 mainWidget
  -- Maybe statusWidget should be Maybe so can override?
  status =
    if isStatusEditing gbs then
      let editWidget      = withAttr inputFieldAttr $ B.renderEditor (str . unlines) True (seEditorState $ fromJust $ gbsStatus gbs)
          editLabelWidget = str (seLabel $ fromJust $ gbsStatus gbs)
      in editLabelWidget <+> editWidget
    else
      statusWidget
  view = vBox
    [ box
    -- This needs to be better... it needs to detect the status mode and then construct the widget for either...?
    , vLimit 1 status
    -- TODO: status should work like popup. including edit Maybe and put this in represent
    -- Maybe have an Either status widget where it's either display status or edit field
    ]

-- WRONG TYPE, MAYBE IS NECESSARY
cacheLookup :: Location -> Cache -> Maybe FilePath
cacheLookup location = Map.lookup (locationAsString location)

isCached :: Location -> Cache -> Bool
isCached location cache = case Map.lookup (locationAsString location) cache of
  (Just _) -> True
  Nothing  -> False

cacheInsert :: Location -> FilePath -> Cache -> Cache
cacheInsert location = Map.insert (locationAsString location)

-- TODO: document and give a repl example
locationAsString :: Location -> String
locationAsString (host, port, resource, mode) =
  host ++ ":" ++ show port ++ resource ++ " (" ++ show mode ++ ")"

-- FIXME: more like makeState from menu lol. maybe can make do for any state
-- FIXME: update for cache
-- based on passing it the mode and other info! newStateForMenu?
--
-- probs needs to be IO
--
-- | Make a new GopherBrowserState for a GopherMenu based on a few
-- necessary parts that must be carried over, like History and
-- Cache.
newStateForMenu
  :: B.BChan CustomEvent
  -> GopherMenu
  -> Location
  -> History
  -> Cache
  -> GopherBrowserState
newStateForMenu chan gm@(GopherMenu ls) location history cache = GopherBrowserState
  { gbsBuffer     =
    MenuBuffer
      $ Menu (gm, BrickList.list MenuViewport glsVector 1, mkFocusLinesIndex gm)
  , gbsLocation   = location
  , gbsHistory    = history
  , gbsRenderMode = MenuMode
  , gbsChan       = chan
  , gbsPopup      = Nothing
  , gbsCache      = cache-- FIXME: should I be updating this?
  , gbsStatus     = Nothing
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

mainViewportScroll :: ViewportScroll MyName
mainViewportScroll = viewportScroll MainViewport

menuViewportScroll :: ViewportScroll MyName
menuViewportScroll = viewportScroll MenuViewport

textViewportScroll :: ViewportScroll MyName
textViewportScroll = viewportScroll TextViewport
