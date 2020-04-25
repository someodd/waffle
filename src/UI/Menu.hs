-- FIXME: optimizations! cleanup!
module UI.Menu
  ( menuEventHandler
  , menuModeUI
  )
where

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
                                                , str
                                                , withAttr
                                                , (<+>)
                                                )
import           Web.Browser

import           UI.Style
import           Gopher
import           UI.Progress
import           UI.Util
import           UI.Representation

selectedMenuLine :: GopherBrowserState -> Either RecognizedGopherLine UnrecognizedGopherLine
selectedMenuLine gbs =
  -- given the scope of this function, i believe this error message is not horribly accurate in all cases where it might be used
  let
    lineNumber = fromMaybe
      (error
        "Hit enter, but nothing was selected to follow! I'm not sure how that's possible!"
      )
      (l ^. BrickList.listSelectedL)
  in  menuLine menu lineNumber
  where (Menu (menu, l, _)) = getMenu gbs

-- Inefficient
jumpNextLink :: GopherBrowserState -> GopherBrowserState
jumpNextLink gbs = updateMenuList (BrickList.listMoveTo next l)
 where
  (Menu (_, l, focusLines)) = getMenu gbs
  currentIndex = fromJust $ BrickList.listSelected l
  next = fromMaybe (head focusLines) (find (> currentIndex) focusLines)
  -- FIXME: repeated code
  updateMenuList ls =
    let (Menu (gm, _, fl)) = getMenu gbs
    in  gbs { gbsBuffer = MenuBuffer $ Menu (gm, ls, fl) }

-- Inefficient
jumpPrevLink :: GopherBrowserState -> GopherBrowserState
jumpPrevLink gbs = updateMenuList (BrickList.listMoveTo next l)
 where
  (Menu (_, l, focusLines)) = getMenu gbs
  currentIndex = fromJust $ BrickList.listSelected l
  next = fromMaybe (last focusLines)
                   (find (< currentIndex) $ reverse focusLines)
  -- FIXME: repeated code
  updateMenuList ls =
    let (Menu (gm, _, fl)) = getMenu gbs
    in  gbs { gbsBuffer = MenuBuffer $ Menu (gm, ls, fl) }

-- | Make a request based on the currently selected Gopher menu item and change
-- the application state (GopherBrowserState) to reflect the change.
newStateFromSelectedMenuItem :: GopherBrowserState -> IO GopherBrowserState
newStateFromSelectedMenuItem gbs = case lineType of
  (Left ct) -> case ct of
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
  (Right nct) -> case nct of
    HtmlFile -> openBrowser (drop 4 resource) >> pure gbs
    InformationalMessage -> pure gbs
    -- FIXME: same as previous comment...
    _        -> initProgressMode gbs Nothing (host, port, resource, FileBrowserMode)
 where
  (host, port, resource, lineType) = case selectedMenuLine gbs of
    -- RecognizedGopherLine
    (Left  gl) -> (glHost gl, glPort gl, glSelector gl, glType gl)
    -- Unrecognized line
    (Right _ ) -> error "Can't do anything with unrecognized line."

menuModeUI :: GopherBrowserState -> [T.Widget MyName]
menuModeUI gbs = defaultBrowserUI gbs (viewport MenuViewport T.Horizontal) titleWidget mainWidget statusWidget
  where
   (Menu (_, l, _)) = getMenu gbs
   titleWidget =
     let (host, port, resource, _) = gbsLocation gbs
     in str $ " " ++ host ++ ":" ++ show port ++ if not $ List.null resource then " (" ++ resource ++ ") " else " "
   statusWidget =
     let cur              = case l ^. BrickList.listSelectedL of
                              Nothing -> str "-"
                              Just i  -> str (show (i + 1))
         total            = str $ show $ Vector.length $ l ^. BrickList.listElementsL
     in  str "? for help. Menu mode. " <+> str "Item " <+> cur <+> str " of " <+> total
   mainWidget = BrickList.renderListWithIndex (listDrawElement gbs) True l

-- FIXME: this is messy! unoptimized!
listDrawElement
  :: GopherBrowserState -> Int -> Bool -> String -> T.Widget MyName
listDrawElement gbs indx sel a = cursorRegion <+> possibleNumber <+> withAttr
  lineColor
  (lineDescriptorWidget (menuLine gmenu indx) <+> selStr a)
 where
  selStr s
    | sel && isInfoMsg (selectedMenuLine gbs) = withAttr custom2Attr (str s)
    | sel       = withAttr customAttr $ str s
    | otherwise = str s

  (Menu (gmenu, mlist, focusLines)) = getMenu gbs

  cursorRegion = if sel then withAttr asteriskAttr $ str " ➤ " else str "   "
  isLink                            = indx `elem` focusLines
  lineColor                         = if isLink then linkAttr else textAttr
  biggestIndexDigits =
    length $ show (Vector.length $ mlist ^. BrickList.listElementsL)
  curIndexDigits = length $ show $ fromJust $ indx `elemIndex` focusLines

  possibleNumber = if isLink
    then
      withAttr numberPrefixAttr
      $  str
      $  numberPad
      $  show (fromJust $ indx `elemIndex` focusLines)
      ++ ". "
    else str ""
    where numberPad = (replicate (biggestIndexDigits - curIndexDigits) ' ' ++)

  lineDescriptorWidget :: Either RecognizedGopherLine UnrecognizedGopherLine -> T.Widget n
  lineDescriptorWidget line = case line of
    -- it's a gopherline
    (Left gl) -> case glType gl of
      -- Cannonical type
      (Left ct) -> case ct of
        Directory -> withAttr directoryAttr $ str "📂 [Directory] "
        File      -> withAttr fileAttr $ str "📄 [File] "
        IndexSearchServer ->
          withAttr indexSearchServerAttr $ str "🔎 [IndexSearchServer] "
        _ -> withAttr genericTypeAttr $ str $ "[" ++ show ct ++ "] "
      -- Noncannonical type
      (Right nct) -> case nct of
        InformationalMessage -> str $ replicate (biggestIndexDigits + 2) ' '
        HtmlFile -> withAttr directoryAttr $ str "🌐 [HTMLFile] "
        _ -> withAttr genericTypeAttr $ str $ "[" ++ show nct ++ "] "
    -- it's a malformed/unrecognized line
    (Right _) -> str ""

-- | Describe the currently selected line in the menu/map.
lineInfoPopup :: GopherBrowserState -> GopherBrowserState
lineInfoPopup gbs =
  let currentLineInfo = explainLine $ selectedMenuLine gbs
  in gbs { gbsPopup = Just $ Popup { pLabel   = "Line Info"
                                   , pWidgets = [str currentLineInfo]
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
      V.EvKey (V.KChar 'n') [] -> M.continue $ jumpNextLink gbs
      V.EvKey (V.KChar 'p') [] -> M.continue $ jumpPrevLink gbs
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
