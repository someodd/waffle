-- TODO: hlint
-- TODO: horizontal scroll for lines that need it.
-- TODO: just disable enter/color for certain lines like information but let you select duh
--- TODO: use INI theme function so people can edit INI file to set
--- various attribute styles/colors!
-- can even change borders
-- TODO: color/invert keys in status bar
-- TODO: some of this stuff deserves to go in gopherclient
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Ui where

import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Control.Monad.IO.Class

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimitPercent, str, vBox, vLimit, withAttr, (<+>))
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec

import GopherClient

drawUI :: GopherBrowserState -> [Widget ()]
drawUI gbs = [ui]
    where
        l = gbsList gbs
        label = str "Item " <+> cur <+> str " of " <+> total
        cur = case l^.(L.listSelectedL) of
                Nothing -> str "-"
                Just i  -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.(L.listElementsL)
        box = B.borderWithLabel label $
              hLimitPercent 100 $
              L.renderListWithIndex (listDrawElement gbs) True l
        ui = C.vCenter $ vBox [ box
                              , vLimit 1 $ C.hCenter $ str "Esc to exit. Vi keys to browse. Enter to open."
                              ]

-- FIXME: should check line type... display different for selection
-- NOTE: should parts of this go in gopherclient? also what about content negotiation?
-- can't always give back a gophermenu! what about text pages? what about downloads?
-- data Content = Viewable (Either GopherMenu Text) | Download?
-- if we detect text we should instead use viewport
-- | Make a Gopher Protocol request based on the information in state and update
--- the state based on said request.
requestNewState :: GopherBrowserState -> IO GopherBrowserState
requestNewState gbs = do
  -- should be looking at type!
  let (host, port, resource) = decipherLine (selectedMenuLine gbs)
  o <- gopherGet host (show port) resource
  let newMenu = makeGopherMenu o
  pure $ makeState newMenu
  where 
  decipherLine l = case l of
    -- GopherLine
    (Left gl) -> (glHost gl, glPort gl, glSelector gl)
    -- Unrecognized line
    (Right _) -> error "Can't do anything with unrecognized line."

selectedMenuLine :: GopherBrowserState -> Either GopherLine MalformedGopherLine
selectedMenuLine gbs = lineFromMenu (gbsMenu gbs)
  where 
  -- FIXME: use menuLine
  lineFromMenu (GopherMenu ls) = ls !! lineNumber
  lineNumber = case (gbsList gbs)^.(L.listSelectedL) of
          Nothing -> error "Hit enter but nothing selected"
          Just i  -> i

menuLine :: GopherMenu -> Int -> Either GopherLine MalformedGopherLine
menuLine (GopherMenu ls) indx = ls !! indx

appEvent :: GopherBrowserState -> T.BrickEvent () e -> T.EventM () (T.Next GopherBrowserState)
appEvent gbs (T.VtyEvent e) =
    case e of
        V.EvKey (V.KEnter) [] -> liftIO (requestNewState gbs) >>= M.continue
        V.EvKey V.KEsc [] -> M.halt gbs
        ev -> M.continue =<< (\x -> gbs {gbsList=x}) <$> (L.handleListEventVi L.handleListEvent) ev (gbsList gbs)
appEvent gbs _ = M.continue gbs

-- TODO: prefix most lines with a [type descriptor] that has a color associated with it
-- FIXME: what if not followable? disable colors...
listDrawElement :: GopherBrowserState -> Int -> Bool -> String -> Widget ()
listDrawElement gbs indx sel a =
    let selStr s = if sel && isInfoMsg (selectedMenuLine gbs) then withAttr custom2Attr (str $ s)
                   else if sel then withAttr customAttr (str $ s)
                   else str $ s
    in lineDescriptorWidget (menuLine (gbsMenu gbs) indx) <+> selStr a
    where
    lineDescriptorWidget :: Either GopherLine MalformedGopherLine -> Widget n
    lineDescriptorWidget line = case line of
      -- it's a gopherline
      (Left gl) -> case glType gl of
        -- Cannonical type
        (Left ct) -> case ct of
          Directory -> withAttr directoryAttr $ str "[Directory] "
          _ -> withAttr genericTypeAttr $ str $ "[" ++ show ct ++ "] "
        -- Noncannonical type
        (Right nct) -> case nct of
          InformationalMessage -> str ""
          _ -> withAttr genericTypeAttr $ str $ "[" ++ show nct ++ "] "
      -- it's a malformed line
      (Right _) -> str ""

-- FIXME
-- Maybe belongs in GopherClient... show instances?
-- maybe should produce widgets?
lineShow :: Either GopherLine MalformedGopherLine -> String
lineShow line = case line of
  -- It's a GopherLine
  (Left gl) -> case glType gl of
    -- Canonical type
    (Left _) -> clean $ glDisplayString gl
    -- Noncanonical type
    (Right nct) -> if nct == InformationalMessage then (if (clean $ glDisplayString gl) == "" then " " else clean $ glDisplayString gl) else clean $ glDisplayString gl
  -- It's a MalformedGopherLine
  (Right mgl) -> clean $ show mgl
  where
  clean :: String -> String
  clean = replaceTabs . replaceReturns
    where
      replaceTabs = map (\x -> if x == '\t' then ' ' else x)
      replaceReturns = map (\x -> if x == '\r' then ' ' else x)

makeState :: GopherMenu -> GopherBrowserState
makeState gm@(GopherMenu ls) = GopherBrowserState
  { gbsList = L.list () glsVector 1
  , gbsMenu = gm
  }
  where
  glsVector = Vec.fromList $ map lineShow ls

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

custom2Attr :: A.AttrName
custom2Attr = "custom2"

directoryAttr :: A.AttrName
directoryAttr = "directoryAttr"

genericTypeAttr :: A.AttrName
genericTypeAttr = "genericTypeAttr"

-- should implement style too
theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            fg V.white)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (directoryAttr,         fg V.red)
    , (genericTypeAttr,         fg V.blue)
    , (customAttr,            (V.defAttr `V.withStyle` V.bold) `V.withForeColor` V.black `V.withBackColor` (V.rgbColor (201 :: Int) (255 :: Int) (229 :: Int)))
    , (custom2Attr,           V.white `on` V.brightBlack)
    ]

theApp :: M.App GopherBrowserState e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

-- TODO: rename to GopherBrowseState?
data GopherBrowserState = GopherBrowserState
  { gbsMenu :: GopherMenu
  , gbsList :: L.List () String
  }

uiMain :: GopherMenu -> IO ()
uiMain gm = void $ M.defaultMain theApp (makeState gm)
