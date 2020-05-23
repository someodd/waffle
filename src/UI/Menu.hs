{-# LANGUAGE OverloadedStrings #-}

-- FIXME: optimizations! cleanup!
module UI.Menu
  ( menuEventHandler
  , menuModeUI
  )
where

import qualified Data.Text                     as T
import           Data.List                     as List
import qualified Data.Vector                   as Vector
import           Control.Monad.IO.Class
import           Data.Maybe

import qualified Graphics.Vty                  as V
import qualified Brick.Main                    as M
import qualified Brick.Widgets.List            as L
import           Lens.Micro                     ( (^.) )
import qualified Brick.Widgets.List            as BrickList
import qualified Brick.Types                   as T
import           Brick.Widgets.Edit            as E
import           Brick.Widgets.Core             ( viewport
                                                , txt
                                                , withAttr
                                                , (<+>)
                                                )
import           Web.Browser

import           UI.Style
import           Gopher
import           UI.Progress
import           UI.Util
import           UI.Representation

-- | Get the `MenuLine` which is currently selected, or Nothing.
selectedMenuLine :: Menu -> Maybe MenuLine
selectedMenuLine menu =
  case l ^. BrickList.listSelectedL of
    Just lineNumber -> Just $ menuLine gopherMenu lineNumber
    Nothing         -> Nothing
  where
    (Menu (gopherMenu, l, _)) = menu

-- | Used by `jumpNextLink` and `jumpPrevLink` for creating a new
-- menu that uses the updated list position.
updateMenuPosition :: Menu -> Int -> Menu
updateMenuPosition menu next =
  let (Menu (gm, l, fl)) = menu
  in  Menu (gm, BrickList.listMoveTo next l, fl)

-- FIXME: move away form getMenu gbs and using gbs
-- | Jump to the next link (wraps around). Basically, skips info items.
jumpNextLink :: Menu -> Menu
jumpNextLink menu = updateMenuPosition menu next
 where
  (Menu (_, l, focusLines)) = menu

  headOr a []    = a
  headOr _ (x:_) = x

  next = case (BrickList.listSelected l) of
    -- NOTE: using "find" for this feels inefficient... oh well!
    Just currentIndex ->
      -- Try to find a line # bigger than the currently selected line in
      -- the focusLines to give us the new/next line to jump to.
      --
      -- If we cannot find a line # bigger than the currently selected line
      -- we wrap to the first link. However, if there is no "first link,"
      -- something that would happen if there's no elements in focusLines,
      -- we just return the active line.
      fromMaybe (headOr currentIndex focusLines) (find (> currentIndex) focusLines)
    -- If there's no currently selected line let's select line 0!
    Nothing           -> headOr 0 focusLines

-- | Jump to previous link (wraps around). Basically, skips info items.
-- Be sure to see `jumpNextLink` (most of my code comments are in there).
jumpPrevLink :: Menu -> Menu
jumpPrevLink menu = updateMenuPosition menu next
 where
  (Menu (_, l, focusLines)) = menu

  lastOr a []     = a
  lastOr _ xs     = last xs

  next = case (BrickList.listSelected l) of
    Just currentIndex ->
      fromMaybe (lastOr currentIndex focusLines) (find (< currentIndex) $ reverse focusLines)
    Nothing           -> lastOr 0 focusLines

-- | Make a request based on the currently selected Gopher menu item and change
-- the application state (GopherBrowserState) to reflect the change.
newStateFromSelectedMenuItem :: GopherBrowserState -> IO GopherBrowserState
newStateFromSelectedMenuItem gbs = case lineType of
  (Canonical ct) -> case ct of
    Directory -> initProgressMode gbs Nothing (host, port, resource, MenuMode)
    File -> initProgressMode gbs Nothing (host, port, resource, TextFileMode)
    IndexSearchServer -> pure gbs
      { gbsRenderMode = SearchMode
      , gbsBuffer     = SearchBuffer $ Search
                          { sbQuery             = ""
                          , sbFormerBufferState = gbsBuffer gbs
                          , sbSelector          = resource
                          , sbPort              = port
                          , sbHost              = host
                          , sbEditorState       = E.editor MyViewport Nothing ""
                          }
      }
    ImageFile -> initProgressMode gbs Nothing (host, port, resource, FileBrowserMode)
    -- FIXME: it's possible this could be an incorrect exception if everything isn't covered, like telnet
    -- so I need to implement those modes above and then of course this can be the catchall...
    _         -> initProgressMode gbs Nothing (host, port, resource, FileBrowserMode)
  (NonCanonical nct) -> case nct of
    HtmlFile -> openBrowser (T.unpack $ T.drop 4 resource) >> pure gbs
    InformationalMessage -> pure gbs
    -- FIXME: same as previous comment...
    _        -> initProgressMode gbs Nothing (host, port, resource, FileBrowserMode)
 where
  menu = getMenu gbs
  (host, port, resource, lineType) = case selectedMenuLine menu of
    -- ParsedLine
    Just (Parsed  gl)     -> (glHost gl, glPort gl, glSelector gl, glType gl)
    -- FIXME: why even error here?
    -- Unrecognized/unparseable line
    Just (Unparseable _ ) -> error "Can't do anything with unrecognized line."
    Nothing               -> error "Nothing is selected!"

menuModeUI :: GopherBrowserState -> [T.Widget MyName]
menuModeUI gbs = defaultBrowserUI gbs (viewport MenuViewport T.Horizontal) titleWidget mainWidget statusWidget
  where
   (Menu (_, l, _)) = getMenu gbs
   titleWidget =
     let (host, port, resource, _) = gbsLocation gbs
     in txt $ " " <> host <> ":" <> (T.pack $ show port) <> if not $ T.null resource then " (" <> resource <> ") " else " "
   statusWidget =
     let cur              = case l ^. BrickList.listSelectedL of
                              Nothing -> txt "-"
                              Just i  -> txt (T.pack $ show (i + 1))
         total            = txt $ T.pack . show $ Vector.length $ l ^. BrickList.listElementsL
     in  txt "? for help. Menu mode. " <+> txt "Item " <+> cur <+> txt " of " <+> total
   mainWidget = BrickList.renderListWithIndex (listDrawElement gbs) True l

-- FIXME: this is messy! unoptimized!
listDrawElement
  :: GopherBrowserState -> Int -> Bool -> T.Text -> T.Widget MyName
listDrawElement gbs indx sel a = cursorRegion <+> possibleNumber <+> withAttr
  lineColor
  (selStr a <+> lineDescriptorWidget (menuLine gmenu indx) )
 where
  menu = getMenu gbs
  (Menu (gmenu, mlist, focusLines)) = menu
  maybeSelectedLine = selectedMenuLine menu

  -- FIXME: I should document what this does...
  selStr s
    | sel && isJust maybeSelectedLine && isInfoMsg (fromJust $ selectedMenuLine menu) =
      withAttr custom2Attr (txt s)
    | sel       = withAttr customAttr $ txt s
    | otherwise = txt s

  cursorRegion = if sel then withAttr asteriskAttr $ txt " ➤ " else txt "   "
  isLink                            = indx `elem` focusLines
  lineColor                         = if isLink then linkAttr else textAttr
  biggestIndexDigits =
    length $ show (Vector.length $ mlist ^. BrickList.listElementsL)
  curIndexDigits = length $ show $ fromJust $ indx `elemIndex` focusLines

  possibleNumber = if isLink
    then
      withAttr numberPrefixAttr
      $  txt
      $  numberPad
      $  (T.pack $ show (fromJust $ indx `elemIndex` focusLines))
      <> ". "
    else txt $ (T.replicate (biggestIndexDigits + 2) " ")
    where
      numberPad :: T.Text -> T.Text
      numberPad = (T.replicate (biggestIndexDigits - curIndexDigits) " " <>)

  lineDescriptorWidget :: MenuLine -> T.Widget n
  lineDescriptorWidget line = case line of
    -- it's a parsed line
    (Parsed gl) -> case glType gl of
      -- Cannonical type
      (Canonical ct) -> case ct of
        Directory -> withAttr directoryAttr $ txt " 📂 [Directory]"
        File      -> withAttr fileAttr $ txt " 📄 [File]"
        IndexSearchServer ->
          withAttr indexSearchServerAttr $ txt " 🔎 [IndexSearchServer]"
        _ -> withAttr genericTypeAttr $ txt $ " [" <> (T.pack $ show ct) <> "]"
      -- Noncannonical type
      (NonCanonical nct) -> case nct of
        InformationalMessage -> txt $ T.replicate (biggestIndexDigits + 2) " "
        HtmlFile -> withAttr directoryAttr $ txt " 🌐 [HTMLFile] "
        _ -> withAttr genericTypeAttr $ txt $ " [" <> (T.pack $ show nct) <> "]"
    -- it's a malformed/unrecognized line
    (Unparseable _) -> txt ""

-- | Describe the currently selected line in the menu/map.
lineInfoPopup :: GopherBrowserState -> GopherBrowserState
lineInfoPopup gbs =
  let menu = getMenu gbs
      currentLineInfo = case (selectedMenuLine menu) of
        Just gopherLine -> explainLine gopherLine
        Nothing         -> "Nothing is selected!"
  in  gbs { gbsPopup = Just $ Popup { pLabel   = "Line Info"
                                    , pWidgets = [txt currentLineInfo]
                                    , pHelp    = "Currently selected line is of this type. ESC to close."
                                    }
          }

menuEventHandler
  :: GopherBrowserState
  -> V.Event
  -> T.EventM MyName (T.Next GopherBrowserState)
menuEventHandler gbs e
  -- Handle a popup (esc key to dismiss) while there is a popup present...
  | hasPopup gbs = case e of
      V.EvKey V.KEsc [] -> M.continue $ closePopup gbs
      _ -> M.continue gbs
  --- Handle controlling the menu.
  | otherwise    = case e of
      V.EvKey (V.KChar 'i') [] -> M.continue $ lineInfoPopup gbs
      V.EvKey V.KEnter [] ->
        liftIO (newStateFromSelectedMenuItem gbs) >>= M.continue
      V.EvKey (V.KChar 'l') [] -> M.hScrollBy menuViewportScroll 1 >> M.continue gbs
      V.EvKey (V.KChar 'h') [] -> M.hScrollBy menuViewportScroll (-1) >> M.continue gbs
      V.EvKey (V.KChar 'n') [] -> M.continue $ newMenuBuffer gbs $ jumpNextLink (getMenu gbs)
      V.EvKey (V.KChar 'p') [] -> M.continue $ newMenuBuffer gbs $ jumpPrevLink (getMenu gbs)
      V.EvKey (V.KChar 'u') [] -> liftIO (goParentDirectory gbs) >>= M.continue
      V.EvKey (V.KChar 'f') [] -> liftIO (goHistory gbs 1) >>= M.continue
      V.EvKey (V.KChar 'b') [] -> liftIO (goHistory gbs (-1)) >>= M.continue
      -- The following catch-all is to hand off the event to Brick's list handler (the special one with vi controls).
      ev -> M.continue =<< updateMenuList <$> L.handleListEventVi
        L.handleListEvent
        ev
        (getMenuList gbs)
 where
  getMenuList x = let (Menu (_, gl, _)) = getMenu x in gl
  updateMenuList x =
    let (Menu (gm, _, fl)) = getMenu gbs
    in  gbs { gbsBuffer = MenuBuffer $ Menu (gm, x, fl) }
