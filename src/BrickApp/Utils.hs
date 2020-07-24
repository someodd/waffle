{-# LANGUAGE OverloadedStrings #-}

-- | Honestly, this is sloppily just a catchall for things many UI modules use. Should be sorted
-- soon/later.
module BrickApp.Utils
  ( makePopupWidget
  , defaultBrowserUI
  , defaultOptimizedUI
  , menuToMenuBuffer
  , cacheLookup
  , isCached
  , newStateForMenu
  , myNameScroll
  , mainViewportScroll
  , cacheInsert
  , menuViewportScroll
  , textViewportScroll
  , locationAsString
  , selectorToRenderMode
  , renderModeToItemChar
  ) where

import qualified Data.Text                     as T
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
import qualified Brick.Widgets.List            as BrickList -- (List)? FIXME

import           Gopher
import           BrickApp.Types                       ( GopherBrowserState(..)
                                                , Cache
                                                , History
                                                , Location
                                                , RenderMode(..)
                                                , CustomEvent
                                                , Menu(..)
                                                , Buffer(..)
                                                , StatusEditor(..)
                                                , Popup(..)
                                                )
import           BrickApp.Types.Names                 ( AnyName(..), MyName(..) )
import           BrickApp.Types.Helpers               ( isStatusEditing, hasPopup )
import           BrickApp.Utils.Popup
import           BrickApp.Utils.Style

makePopupWidget :: GopherBrowserState -> B.Widget AnyName
makePopupWidget gbs = 
  B.centerLayer $ head $ popup (pLabel . fromJust $ gbsPopup gbs) (pWidgets . fromJust $ gbsPopup gbs) (pHelp . fromJust $ gbsPopup gbs)

-- | Pick out the appropriate `RenderMode` for the supplied `Selector`.
selectorToRenderMode :: Selector -> RenderMode
selectorToRenderMode selector =
  case selectorItemType selector of
    Just someItemType ->
      case someItemType of
        Canonical Directory -> MenuMode
        Canonical File      -> TextFileMode
        _                   -> FileBrowserMode
    Nothing -> FileBrowserMode

-- FIXME: okay so this is nice and all but how will we handle editor input once activated? how do we tell it's activated?
-- FIXME: poppys and statusWidget both relevant!
-- I need a better name for this, but basically it's the default view you see
-- for everything! There are only a few exceptions.
--defaultBrowserUI :: GopherBrowserState -> B.Viewport -> B.Widget MyName -> B.Widget MyName -> B.Widget MyName -> [B.Widget MyName]
-- FIXME: can make this all name-agnostic if you fix status issue?
defaultBrowserUI ::
  GopherBrowserState
  -> (B.Widget AnyName -> B.Widget AnyName)
  -> B.Widget AnyName
  -> B.Widget AnyName
  -> B.Widget AnyName
  -> [B.Widget AnyName]
defaultBrowserUI gbs mainViewport titleWidget mainWidget statusWidget = defaultOptimizedUI gbs titleWidget mainWidget' statusWidget
 where
  mainWidget' = mainViewport $ hLimitPercent 100 mainWidget

-- TODO: replace `defaultBrowserUI` with this!
-- mainWidget should be like: (mainViewport $ hLimitPercent 100 mainWidget)
defaultOptimizedUI ::
  GopherBrowserState
  -> B.Widget AnyName
  -> B.Widget AnyName
  -> B.Widget AnyName
  -> [B.Widget AnyName]
defaultOptimizedUI gbs titleWidget mainWidget statusWidget = [makePopupWidget gbs | hasPopup gbs] ++ [hCenter $ vCenter view]
 where
  box :: B.Widget AnyName
  box =
    updateAttrMap (B.applyAttrMappings borderMappings)
      $ withBorderStyle customBorder
      $ B.borderWithLabel (withAttr titleAttr titleWidget)
      $ mainWidget

  -- Maybe statusWidget should be Maybe so can override?
  -- FIXME: this is source of enforcing MyName because seEditorState is always MyName type... what if constructed it here instead based
  -- on type of name given
  status :: B.Widget AnyName
  status =
    if isStatusEditing gbs then
      let editWidget      = withAttr inputFieldAttr $ B.renderEditor (txt . T.unlines) True (seEditorState $ fromJust $ gbsStatus gbs)
          editLabelWidget = txt (seLabel $ fromJust $ gbsStatus gbs)
      in editLabelWidget <+> editWidget
    else
      statusWidget

  -- FIXME: could I just use <+> or something here? Try to combinge name N with named other type? impossible?
  view :: B.Widget AnyName
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
locationAsString :: Location -> T.Text
locationAsString (host, port, resource, mode) =
  host <> ":" <> (T.pack $ show port) <> resource <> " (" <> (T.pack $ show mode) <> ")"

-- | From a generic `GopherMenu` to a TUI-specific `Buffer` type.
menuToMenuBuffer :: GopherMenu -> Buffer
menuToMenuBuffer gopherMenu@(GopherMenu ls) =
  MenuBuffer $ Menu (gopherMenu, BrickList.list (MyName MyWidget) glsVector 1, mkFocusLinesIndex gopherMenu)
 where
  glsVector = Vector.fromList $ map lineShow ls

  mkFocusLinesIndex (GopherMenu m) =
    map fst $ filter (not . isInfoMsg . snd) (zip [0 ..] m)

  -- | Used for filling up a list with display strings.
  lineShow :: MenuLine -> T.Text
  lineShow line = case line of
    -- It's a GopherLine
    (Parsed gl) -> case glType gl of
      -- Canonical type
      (Canonical _) -> glDisplayString gl
      -- Noncanonical type
      (NonCanonical nct) ->
        if nct == InformationalMessage && glDisplayString gl == ""
          then " "
          else glDisplayString gl
    -- It's a MalformedGopherLine
    (Unparseable _) -> menuLineAsText line

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
newStateForMenu chan gm location history cache = GopherBrowserState
  { gbsBuffer     = menuToMenuBuffer gm
  , gbsLocation   = location
  , gbsHistory    = history
  , gbsRenderMode = MenuMode
  , gbsChan       = chan
  , gbsPopup      = Nothing
  , gbsCache      = cache-- FIXME: should I be updating this?
  , gbsStatus     = Nothing
  }

myNameScroll :: B.ViewportScroll AnyName
myNameScroll = B.viewportScroll $ MyName MyViewport

mainViewportScroll :: B.ViewportScroll AnyName
mainViewportScroll = B.viewportScroll $ MyName MainViewport

menuViewportScroll :: B.ViewportScroll AnyName
menuViewportScroll = B.viewportScroll $ MyName MenuViewport

textViewportScroll :: B.ViewportScroll AnyName
textViewportScroll = B.viewportScroll $ MyName TextViewport

renderModeToItemChar :: RenderMode -> Char
renderModeToItemChar renderMode =
  case renderMode of
    TextFileMode -> '0'
    MenuMode -> '1'
    MenuJumpMode -> '1'
    FileBrowserMode -> '9'
    SearchMode -> '1'
    -- FIXME: this shouldn't be an error!
    _ -> error $ "Can't bookmark mode " ++ show renderMode
