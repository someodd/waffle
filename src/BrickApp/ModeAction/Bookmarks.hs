{-# LANGUAGE OverloadedStrings #-}

-- TODO: this doesn't even make sense here atm because there's no mode its just menu mode etc
-- | UI for editing the open config for setting commands associated with
-- opening a menu item of specific types.
module BrickApp.ModeAction.Bookmarks where

import qualified Data.Map as Map
import           Data.Maybe                     ( fromJust )
import qualified Data.Text                     as T
import           Data.List                      ( intercalate )

import qualified Brick.Widgets.Dialog as D
import           Brick.Widgets.Edit            as E
import           Brick.Widgets.Core             ( str )
import qualified Data.ConfigFile               as CF

import           Bookmarks
import           BrickApp.Types.Names
import           BrickApp.Utils                       ( menuToMenuBuffer, renderModeToItemChar )
import           Gopher
import           Config.Bookmarks
import           BrickApp.Types
import           BrickApp.Types.Helpers
import           BrickApp.ModeAction.Menu.State       ( selectedMenuLine )

initAddBookmarkMode :: GopherBrowserState -> GopherBrowserState
initAddBookmarkMode gbs = gbs
  { gbsRenderMode = AddBookmarkMode
  , gbsStatus     = Just $ StatusEditor { seLabel = "Bookmark Name: "
                                        , seEditorState = E.editor (MyName EditorViewport) Nothing ""
                                        , seFormerMode = gbsRenderMode gbs
                                        }
  }

-- | Remove the selected bookmark, writing to file, and then give back
-- a new version of the `GopherBrowserState` that has the bookmark
-- removed from the menu.
removeSelectedBookmark :: GopherBrowserState -> IO GopherBrowserState
removeSelectedBookmark gbs = do
  let selectedLine = case selectedMenuLine (getMenu gbs) of
                       Just l -> l
                       Nothing -> error "nothing selected"
  let sectionSpec = case selectedLine of
                      Parsed parsedLine -> T.unpack $ glDisplayString parsedLine
                      Unparseable _     -> error "Unparseable line in bookmarks!"
  removeBookmark sectionSpec
  initBookmarksMode gbs

-- FIXME: a hack right now
initBookmarksMode :: GopherBrowserState -> IO GopherBrowserState
initBookmarksMode gbs = do
  bookmarks <- bookmarksMenuText
  pure $ gbs { gbsBuffer = menuToMenuBuffer $ makeGopherMenu $ bookmarks
             , gbsRenderMode = BookmarksMode
             , gbsStatus = Nothing
             , gbsPopup = Nothing
             , gbsLocation = ("", 0, "", MenuMode, Just "Bookmarks")
             }

-- | Create the display string/SectionSpec using the URI of
-- the spec (no schema included [gopher://]) and the input
-- from when the user was prompted to enter a label.
createDisplayString :: String -> String -> CF.SectionSpec
createDisplayString uri inputString =
  let strippedStart = dropWhile (== ' ') inputString
      stripped      = reverse $ dropWhile (== ' ') $ reverse strippedStart
  in  if stripped == ""
        then uri
        else stripped ++ " (" ++ uri ++ ")"

-- | Create a URI string, without gopher:// scheme, from the
-- hostname (`Text`), port (`Int`), item type character (`Char`),
-- and the resource (`Text`).
createUriString :: T.Text -> Int -> Char -> T.Text -> String
createUriString hostName port itemTypeChar resource =
  intercalate "/" [ T.unpack hostName ++ ":" ++ (show port)
                  , [itemTypeChar]
                  , removeLeadSlash $ T.unpack resource
                  ]
 where
  removeLeadSlash s = if take 1 s == "/" then drop 1 s else s

-- FIXME: should popup with a prompt asking for a label? with inputPopupUI
-- | Bookmark the current `Location` and create a popup saying "bookmark added!"
bookmarkCurrentLocation :: GopherBrowserState -> IO GopherBrowserState
bookmarkCurrentLocation gbs =
  -- FIXME: this get contents should be in utils or something
  -- FIXME: fromJust is bad!. Also this could have displayString as default
  let (hostName, port, resource, renderMode, _) = gbsLocation gbs
      uri   = createUriString hostName port itemTypeChar resource
      inputValue = T.unpack $ T.filter (/= '\n') $ T.unlines (E.getEditContents $ seEditorState $ fromJust $ gbsStatus gbs)
      label = createDisplayString uri inputValue
      itemTypeChar = renderModeToItemChar renderMode
      bookmark = (label, T.unpack hostName, port, T.unpack resource, itemTypeChar)
      popup             = Popup
                            { pDialogWidget = D.dialog (Just "Added Bookmark") (Just (0, [ ("Ok", Ok) ])) 50--wtf what about max width for bug
                            , pDialogMap = Map.fromList [("Ok", pure . closePopup)]
                            , pDialogBody = str $ "Added: " ++ inputValue
                            }
  -- FIXME: fromJust!
  in addBookmark bookmark >> pure (gbs { gbsPopup = Just popup, gbsStatus = Nothing, gbsRenderMode = seFormerMode (fromJust . gbsStatus $ gbs) } )

-- should have ability to accept defaults for input fields (in the case of bookmarking a page,
-- vs. raw input on the bookmarks page)
-- addBookmarkPopup :: GopherBrowserState -> ...

-- addBookmark :: URI -> ...
