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
import           Brick.Widgets.Border           ( borderWithLabel )
import           Brick.AttrMap                  ( applyAttrMappings )
import qualified Brick.Widgets.List            as BrickList
import qualified Brick.Types                   as T
import           Brick.Widgets.Center           ( vCenter
                                                , hCenter
                                                )
import           Brick.Widgets.Edit            as E
import           Brick.Widgets.Core             ( viewport
                                                , str
                                                , withAttr
                                                , withBorderStyle
                                                , vBox
                                                , vLimit
                                                , hLimitPercent
                                                , (<+>)
                                                , updateAttrMap
                                                )
import           Web.Browser

import           UI.Style
import           UI.History
import           GopherClient
import           UI.Progress
import           UI.Representation

selectedMenuLine :: GopherBrowserState -> Either GopherLine MalformedGopherLine
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
  next = fromMaybe (reverse focusLines !! 0)
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
    Directory -> initProgressMode gbs (host, port, resource, MenuMode)
    File -> initProgressMode gbs (host, port, resource, TextFileMode)
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
    ImageFile -> initProgressMode gbs (host, port, resource, FileBrowserMode)
    -- FIXME: it's possible this could be an incorrect exception if everything isn't covered, like telnet
    -- so I need to implement those modes above and then of course this can be the catchall...
    _         -> initProgressMode gbs (host, port, resource, FileBrowserMode)
  (Right nct) -> case nct of
    HtmlFile -> openBrowser (drop 4 resource) >> pure gbs
    -- FIXME: same as previous comment...
    _        -> initProgressMode gbs (host, port, resource, FileBrowserMode)
 where
  (host, port, resource, lineType) = case selectedMenuLine gbs of
    -- GopherLine
    (Left  gl) -> (glHost gl, glPort gl, glSelector gl, glType gl)
    -- Unrecognized line
    (Right _ ) -> error "Can't do anything with unrecognized line."

-- FIXME: what if rendering with a popup?
-- | The UI for rendering and viewing a menu.
menuModeUI :: GopherBrowserState -> [T.Widget MyName]
menuModeUI gbs = [hCenter $ vCenter view]
 where
  (Menu (_, l, _))          = getMenu gbs
  label                     = str " Item " <+> cur <+> str " of " <+> total -- TODO: should be renamed
  (host, port, resource, _) = gbsLocation gbs
  title = " " ++ host ++ ":" ++ show port ++ if not $ List.null resource
    then " (" ++ resource ++ ") "
    else " "
  cur = case l ^. BrickList.listSelectedL of
    Nothing -> str "-"
    Just i  -> str (show (i + 1))
  total = str $ show $ Vector.length $ l ^. BrickList.listElementsL
  box =
    updateAttrMap (applyAttrMappings borderMappings)
      $ withBorderStyle customBorder
      $ borderWithLabel (withAttr titleAttr $ str title)
      $ viewport MyWidget T.Horizontal
      $ hLimitPercent 100
      $ BrickList.renderListWithIndex (listDrawElement gbs) True l
  view = vBox
    [ box
    , vLimit 1 $ str "Esc to exit. Vi keys to browse. Enter to open." <+> label
    ]

-- FIXME: this is messy! unoptimized!
listDrawElement
  :: GopherBrowserState -> Int -> Bool -> String -> T.Widget MyName
listDrawElement gbs indx sel a = cursorRegion <+> possibleNumber <+> withAttr
  lineColor
  (lineDescriptorWidget (menuLine (gmenu) indx) <+> selStr a)
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

  lineDescriptorWidget :: Either GopherLine MalformedGopherLine -> T.Widget n
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
    -- it's a malformed line
    (Right _) -> str ""

myWidgetScroll :: M.ViewportScroll MyName
myWidgetScroll = M.viewportScroll MyWidget

menuEventHandler
  :: GopherBrowserState
  -> V.Event
  -> T.EventM MyName (T.Next GopherBrowserState)
menuEventHandler gbs e = case e of
  -- TODO, FIXME: V.EvKey (V.KChar 'i') [] -> M.continue (lineInfoPopup gbs)
  V.EvKey V.KEnter [] ->
    liftIO (newStateFromSelectedMenuItem gbs) >>= M.continue
  V.EvKey (V.KChar 'l') [] -> M.hScrollBy myWidgetScroll 1 >> M.continue gbs
  V.EvKey (V.KChar 'h') [] -> M.hScrollBy myWidgetScroll (-1) >> M.continue gbs
  V.EvKey (V.KChar 'n') [] -> M.continue $ jumpNextLink gbs
  V.EvKey (V.KChar 'p') [] -> M.continue $ jumpPrevLink gbs
  V.EvKey (V.KChar 'u') [] -> liftIO (goParentDirectory gbs) >>= M.continue
  V.EvKey (V.KChar 'f') [] -> liftIO (goHistory gbs 1) >>= M.continue
  V.EvKey (V.KChar 'b') [] -> liftIO (goHistory gbs (-1)) >>= M.continue
-- check gbs if the state says we're handling a menu (list) or a text file (viewport)
  ev -> M.continue =<< updateMenuList <$> L.handleListEventVi
    L.handleListEvent
    ev
    (getMenuList gbs)
 where
  getMenuList x = let (Menu (_, gl, _)) = getMenu x in gl
  updateMenuList x =
    let (Menu (gm, _, fl)) = getMenu gbs
    in  gbs { gbsBuffer = MenuBuffer $ Menu (gm, x, fl) }
