module Ui where

import Brick
import qualified Graphics.Vty as Vty
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

import Control.Monad

data MyState = MyState
  { msText :: String
  } deriving (Show)

data MyName = MyViewport
    deriving (Show, Eq, Ord)

type MyApp = App MyState () MyName

-- | Replace the \t (tabs) and \r (returns) with spaces, because otherwise
-- they'll break Brick!
clean = replaceTabs . replaceReturns
    where
        replaceTabs = map (\x -> if x == '\t' then ' ' else x)
        replaceReturns = map (\x -> if x == '\r' then ' ' else x)

myNameScroll :: M.ViewportScroll MyName
myNameScroll = M.viewportScroll MyViewport

drawUi :: MyState -> [Widget MyName]
drawUi s = [C.center $ B.border $ hLimitPercent 100 $ vLimitPercent 100 $ ui]
    where
    ui = viewport MyViewport Both $ vBox [str $ msText s]

handleEvent :: MyState -> BrickEvent MyName () -> EventM MyName (Next MyState)
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s

handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'j')  [])) = M.vScrollBy myNameScroll 1 >> M.continue s
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'k')  [])) = M.vScrollBy myNameScroll (-1) >> M.continue s
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'l') [])) = M.hScrollBy myNameScroll 1 >> M.continue s
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'h')  [])) = M.hScrollBy myNameScroll (-1) >> M.continue s

handleEvent s _ = continue s

myApp :: MyApp
myApp = App
    { appDraw = drawUi
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap Vty.defAttr []
    }

uiMain initialState = void $ defaultMain myApp $ MyState {msText = initialState}
