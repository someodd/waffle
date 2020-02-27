module Ui where

import Brick
import qualified Graphics.Vty as Vty
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

import Control.Monad
import Data.List
import Data.Maybe

import GopherClient

data MyState = MyState
  { msMenu :: GopherMenu
  , msLinkIndexMarker :: Int
  , msLinkIndices :: [Int]
  } deriving (Show)

data MyName = MyViewport
  deriving (Show, Eq, Ord)

type MyApp = App MyState () MyName

-- | Replace the \t (tabs) and \r (returns) with spaces, because otherwise
-- they'll break Brick!
clean :: String -> String
clean = replaceTabs . replaceReturns
  where
    replaceTabs = map (\x -> if x == '\t' then ' ' else x)
    replaceReturns = map (\x -> if x == '\r' then ' ' else x)

menuStr :: GopherMenu -> Widget n
menuStr m = str . clean $ show m

gopherLineWidget :: Either GopherLine MalformedGopherLine -> Widget n
gopherLineWidget l = case l of
  -- GopherLine
  (Left gl) ->
    if glActive gl then
      (visible . str $ " --> ") <+> (str $ show gl)
    else
      str "     " <+> (str $ show gl)
  -- MalformedLine
  (Right ml) -> str "" <+> (str $ show ml)

gopherMenuWidgets (GopherMenu m) = vBox $ map gopherLineWidget m

-- I get the feeling that there's a far more efficient way to do this in brick lmao
markFirstActive :: GopherMenu -> (GopherMenu, [Int])
markFirstActive gopherMenu@(GopherMenu x) =
  let relevantLineNumber = firstNonInfoLineNumber gopherMenu--FIXME: fromMaybe is bad
      relevantGopherLine = x !! relevantLineNumber
      relevantGopherLine' = case relevantGopherLine of
        Left c -> c {glActive=True}
        Right _ -> error "This should be impossible..."--FIXME
  in (GopherMenu $ take relevantLineNumber x ++ [Left relevantGopherLine'] ++ drop (relevantLineNumber+1) x, allLinkLineNumbers gopherMenu)
  where
  -- FIXME: should me a Maybe...
  allLinkLineNumbers (GopherMenu m) = findIndices isNotInfo m
  firstNonInfoLineNumber (GopherMenu m) = fromJust $ findIndex isNotInfo m
  isNotInfo r = case r of
    -- Is it a proper Gopherline?
    (Left l) -> case glType l of
      -- Is a canonical gopher item type which is never info...
      Left _ -> True
      -- .. is a gopher noncanon type which may be info
      Right w -> w /= InformationalMessage
    -- .. or is it malformed?
    (Right _) -> False

changeActiveLine :: GopherMenu -> Int -> Int -> GopherMenu
changeActiveLine (GopherMenu x) n prev =
  let relevantGopherLine = x !! n
      relevantGopherLine' = case relevantGopherLine of
        Left c -> c {glActive=True}
        Right _ -> error "This should be impossible..."--FIXME
      prevLine = x !! prev
      prevLine' = case prevLine of
        Left c -> c {glActive=False}
        Right _ -> error "This should be impossible..."--FIXME
      prevFixed = take prev x ++ [Left prevLine'] ++ drop (prev+1) x
  in GopherMenu $ take n prevFixed ++ [Left relevantGopherLine'] ++ drop (n+1) prevFixed

myNameScroll :: M.ViewportScroll MyName
myNameScroll = M.viewportScroll MyViewport

drawUi :: MyState -> [Widget MyName]
drawUi s = [C.center $ B.border $ hLimitPercent 100 $ vLimitPercent 100 $ ui]
  where
  --ui = viewport MyViewport Both $ vBox [menuStr $ msMenu s]
  ui = viewport MyViewport Both $ gopherMenuWidgets (msMenu s)

updateMenuNewLinkPos :: MyState -> Int -> (Int, Int, GopherMenu)
updateMenuNewLinkPos s n =
  let currentIndexPos = msLinkIndexMarker s
      currentLineNumber = (msLinkIndices s) !! currentIndexPos
      potentialIndexPos = currentIndexPos + n
      newIndexPos =
        if potentialIndexPos > (length $ msLinkIndices s) - 1 then 0
        else if potentialIndexPos < 0 then (length $ msLinkIndices s) - 1
        else potentialIndexPos
      newLineNumber = (msLinkIndices s) !! newIndexPos
      newMenu = changeActiveLine (msMenu s) newLineNumber currentLineNumber
      lineDifference = newLineNumber - currentLineNumber
  in (newLineNumber, newIndexPos, newMenu)

-- extremely similar to changeActiveLine
unactive (GopherMenu x) n =
  let relevantGopherLine = x !! n
      relevantGopherLine' = case relevantGopherLine of
        Left c -> c {glActive=False}
        Right _ -> error "This should be impossible..."--FIXME crashes because malformed gets... unactivated why?!
  in GopherMenu $ take n x ++ [Left relevantGopherLine'] ++ drop (n+1) x

-- NOTE: This is using setTop but should also use a percentage of the total height
-- to center... ctx <- getContext; h = availHeight ctx
--
-- Or it shouldn't scroll unless off of viewport
scrollLinkPos :: MyState -> Int -> EventM MyName (Next MyState)
scrollLinkPos s n =
  let (newLineNumber, newIndexPos, newMenu) = updateMenuNewLinkPos s n
  in M.setTop myNameScroll newLineNumber >> M.continue (s {msMenu=newMenu, msLinkIndexMarker=newIndexPos})

{-
getViewHeight :: Int
getViewHeight = getContext >>= \ctx -> availHeight ctx
-}

-- | Return what to scroll by in order to vertically center on a specific line
-- in a viewport.
{-
scrollCenterOn :: Int -> Int -> Int
scrollCenterOn toLine fromLine = do
  ctx <- getContext
  let h = availHeight ctx in (abs $ toLine - fromLine) + (h `div` 2)
-}

-- NOTE: when scrolling text we disable visible/active so we can actually move beyond the actively selected link
handleEvent :: MyState -> BrickEvent MyName () -> EventM MyName (Next MyState)
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
-- ^ quit
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'j')  [])) =
  let (_, newIndexPos, newMenu) = updateMenuNewLinkPos s 1
  in M.continue $ s {msMenu=newMenu, msLinkIndexMarker=newIndexPos}-- no need for index marker anymore?
-- ^ move link cursor down
handleEvent s (VtyEvent (Vty.EvKey (Vty.KDown)  [])) =
  M.vScrollBy myNameScroll 1 >> M.continue (s {msMenu=unactive (msMenu s) (msLinkIndices s !! msLinkIndexMarker s)})
-- ^ scroll down, disable active
--handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'k')  [])) = updateMenuNewLinkPos s (-1)
handleEvent s (VtyEvent (Vty.EvKey (Vty.KUp)  [])) =
  M.vScrollBy myNameScroll (-1) >> M.continue (s {msMenu=unactive (msMenu s) (msLinkIndices s !! msLinkIndexMarker s)})
-- ^ scroll text up
handleEvent s (VtyEvent (Vty.EvKey (Vty.KHome)  [])) =
  M.vScrollToBeginning myNameScroll >> M.continue (s {msMenu=unactive (msMenu s) (msLinkIndices s !! msLinkIndexMarker s)})
-- ^ return text home
handleEvent s (VtyEvent (Vty.EvKey (Vty.KEnd)  [])) =
  M.vScrollToEnd myNameScroll >> M.continue (s {msMenu=unactive (msMenu s) (msLinkIndices s !! msLinkIndexMarker s)})
-- ^ end of text
handleEvent s (VtyEvent (Vty.EvKey (Vty.KPageDown)  [])) =
  M.vScrollPage myNameScroll Down >> M.continue (s {msMenu=unactive (msMenu s) (msLinkIndices s !! msLinkIndexMarker s)})
-- ^ page down text
handleEvent s (VtyEvent (Vty.EvKey (Vty.KPageUp)  [])) =
  M.vScrollPage myNameScroll Up >> M.continue (s {msMenu=unactive (msMenu s) (msLinkIndices s !! msLinkIndexMarker s)})
-- ^ page up text
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'l') [])) = M.hScrollBy myNameScroll 1 >> M.continue s
-- ^ scroll text right
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'h')  [])) = M.hScrollBy myNameScroll (-1) >> M.continue s
-- ^ scroll text left
handleEvent s _ = continue s

myApp :: MyApp
myApp = App
  { appDraw = drawUi
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = pure
  , appAttrMap = const $ attrMap Vty.defAttr []
  }

uiMain :: GopherMenu -> IO ()
uiMain initialState =
  let (initialMenu, linkIndices) = markFirstActive initialState
  in void $ defaultMain myApp $ MyState {msMenu = initialMenu, msLinkIndexMarker = 0, msLinkIndices=linkIndices}
