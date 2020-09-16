{-# LANGUAGE OverloadedStrings #-}

-- | UI for editing the open config for setting commands associated with
-- opening a menu item of specific types.
module BrickApp.ModeAction.Open where

import           Data.List                      ( intersperse )

import qualified Brick.Focus as F
import Brick.Widgets.Core
  ( (<+>)
  , hLimit
  , vLimit
  , str
  , txt
  , visible
  )
import qualified Brick.Main                    as M
import qualified Brick.Types                   as T
import           Brick.Widgets.Edit            as E
import qualified Data.ConfigFile               as CF

import           BrickApp.Utils                       ( myNameScroll )
import           Config.ConfigOpen
import           Config
import           BrickApp.Types
import           BrickApp.Types.Names

-- All this field name stuff should probably go in UI.Types! TODO FIXME
allFieldNames :: [AnyName]
allFieldNames = map FieldName [FileField ..]

firstFieldName :: AnyName
firstFieldName = head allFieldNames

lastFieldName :: AnyName
lastFieldName = last allFieldNames

beginningScroll :: T.EventM AnyName ()
beginningScroll = M.vScrollToBeginning myNameScroll >> M.vScrollBy myNameScroll 3

--initConfigOpenMode :: GopherBrowserState -> GopherBrowserState
initConfigOpenMode ::
  GopherBrowserState
  -> IO GopherBrowserState
initConfigOpenMode gbs = gbsToGiveBack
 where
  gbsToGiveBack :: IO GopherBrowserState
  gbsToGiveBack = do
    initState <- initialState gbs
    pure $ gbs
      { gbsRenderMode = OpenConfigMode
      , gbsBuffer     = OpenConfigBuffer initState
      }

  initialState :: GopherBrowserState -> IO OpenConfigState
  initialState gbs' = do
    configParser <- getUserOpenConfig
    fileFieldValue <- (readConfigParserValue configParser "open-assocs" "File")
    directoryFieldValue <- (readConfigParserValue configParser "open-assocs" "Directory")
    csoPhoneBookServerFieldValue <- (readConfigParserValue configParser "open-assocs" "CsoPhoneBookServer")
    errorFieldValue <- (readConfigParserValue configParser "open-assocs" "Error")
    binHexedMacintoshFileValue <- (readConfigParserValue configParser "open-assocs" "BinHexedMacintoshFile")
    dosBinaryArchiveValue <- (readConfigParserValue configParser "open-assocs" "DosBinaryArchive")
    unixUuencodedFileValue <- (readConfigParserValue configParser "open-assocs" "UnixUuencodedFile")
    indexSearchServerValue <- (readConfigParserValue configParser "open-assocs" "IndexSearchServer")
    textBasedTelnetSessionValue <- (readConfigParserValue configParser "open-assocs" "TextBasedTelnetSession")
    binaryFileValue <- (readConfigParserValue configParser "open-assocs" "BinaryFile")
    redundantServerValue <- (readConfigParserValue configParser "open-assocs" "RedundantServer")
    gifFileValue <- (readConfigParserValue configParser "open-assocs" "GifFile")
    imageFileValue <- (readConfigParserValue configParser "open-assocs" "ImageFile")
    tn3270SessionValue <- (readConfigParserValue configParser "open-assocs" "Tn3270Session")
    docValue <- (readConfigParserValue configParser "open-assocs" "Doc")
    htmlFileValue <- (readConfigParserValue configParser "open-assocs" "HtmlFile")
    informationalMessageValue <- (readConfigParserValue configParser "open-assocs" "InformationalMessage")
    soundFileValue <- (readConfigParserValue configParser "open-assocs" "SoundFile")
    pure $ OpenConfigState gbs'
                    (F.focusRing allFieldNames)
                    (E.editor (FieldName FileField) Nothing fileFieldValue)
                    (E.editor (FieldName DirectoryField) Nothing directoryFieldValue)
                    (E.editor (FieldName CsoPhoneBookServerField) Nothing csoPhoneBookServerFieldValue)
                    (E.editor (FieldName ErrorField) Nothing errorFieldValue)
                    (E.editor (FieldName BinHexedMacintoshFileField) Nothing binHexedMacintoshFileValue)
                    (E.editor (FieldName DosBinaryArchiveField) Nothing dosBinaryArchiveValue)
                    (E.editor (FieldName UnixUuencodedFileField) Nothing unixUuencodedFileValue)
                    (E.editor (FieldName IndexSearchServerField) Nothing indexSearchServerValue)
                    (E.editor (FieldName TextBasedTelnetSessionField) Nothing textBasedTelnetSessionValue)
                    (E.editor (FieldName BinaryFileField) Nothing binaryFileValue)
                    (E.editor (FieldName RedundantServerField) Nothing redundantServerValue)
                    (E.editor (FieldName GifFileField) Nothing gifFileValue)
                    (E.editor (FieldName ImageFileField) Nothing imageFileValue)
                    (E.editor (FieldName Tn3270SessionField) Nothing tn3270SessionValue)
                    (E.editor (FieldName DocField) Nothing docValue)
                    (E.editor (FieldName HtmlFileField) Nothing htmlFileValue)
                    (E.editor (FieldName InformationalMessageField) Nothing informationalMessageValue)
                    (E.editor (FieldName SoundFileField) Nothing soundFileValue)

-- TODO: this could go in Types
-- | FIXME: isn't this redundant because of the representation
editFields :: [([Char], OpenConfigState -> Editor String AnyName)]
editFields =
  [ ("File (plaintext)", editFile)
  , ("Directory", editDirectory)
  , ("CSO phonebook server", editCsoPhoneBookServer)
  , ("Gopher error", editError)
  , ("Bin-hexed Macintosh file", editBinHexedMacintoshFile)
  , ("DOS binary archive", editDosBinaryArchive)
  , ("Unix Uuencoded File", editUnixUuencodedFile)
  , ("Index search server", editIndexSearchServer)
  , ("Text-based Telnet session", editTextBasedTelnetSession)
  , ("Binary file", editBinaryFile)
  , ("Redundant server", editRedundantServer)
  , ("GIF file", editGifFile)
  , ("Image file", editImageFile)
  , ("TN3270 Session", editTn3270Session)
  , ("Document", editDoc)
  , ("HTML/web", editHtmlFile)
  , ("Info message", editInformationalMessage)
  , ("Sound file", editSoundFile)
  ]

editWidgets :: OpenConfigState -> [T.Widget AnyName]
editWidgets openConfigState =
  intersperse (str " ") $ map makeField editFields
 where
  -- if it is the current focus add "visible"
  customRenderEditor :: Bool -> E.Editor String AnyName -> T.Widget AnyName
  customRenderEditor hasFocus theEditor =
    if hasFocus
      then visible $ E.renderEditor (str . unlines) hasFocus theEditor
      else E.renderEditor (str . unlines) hasFocus theEditor

  fieldWidth = 30

  --makeField :: (String, * -> *) -> [Widget n]
  makeField (label, someEdit) =
    let editField = F.withFocusRing (focusRing openConfigState) customRenderEditor (someEdit openConfigState)
    in  str (label ++ ": ") <+> (vLimit 1 $ hLimit fieldWidth editField)

-- | Used to return the `GopherBrowserState` from before `OpenConfigMode` was activated.
returnFormerGbs :: GopherBrowserState -> GopherBrowserState
returnFormerGbs gbs =
  let (OpenConfigBuffer openConfigState) = gbsBuffer gbs
  in  formerState openConfigState

-- FIXME: could just be a refocus by taking F.focusNext or F.focusPrev as arg
focusNext :: OpenConfigState -> OpenConfigState
focusNext openConfigState =
  let modified = F.focusNext $ focusRing openConfigState
  in  openConfigState { focusRing = modified }

focusPrev :: OpenConfigState -> OpenConfigState
focusPrev openConfigState =
  let modified = F.focusPrev $ focusRing openConfigState
  in  openConfigState { focusRing = modified }

-- | Update the `GopherBrowserState` with the new `OpenConfigState`
updateGbs :: GopherBrowserState -> (OpenConfigState -> OpenConfigState) -> GopherBrowserState
updateGbs gbs performOnOpenConfigState =
  let (OpenConfigBuffer openConfigState) = gbsBuffer gbs
      mutatedOpenConfigState             = performOnOpenConfigState openConfigState
  in gbs { gbsBuffer = OpenConfigBuffer mutatedOpenConfigState }

-- FIXME: scrollBy should just be an enum of scroll up or scroll down
--scrollFocus :: FocusRing -> ViewportScroll n -> Int -> ()
{-
scrollFocus ring viewport' scrollBy
  | scrollBy > numberOfFocusEntries = M.vScrollToBeginning viewport'
  | 
 where
  numberOfFocusEntries = focusRingLength ring
-}

{-
scrollFocus ...
  focusRingCursor getFocusRing gbs  
 where
  getFocusRing :: GopherBrowserState -> FocusRing n
  getFocusRing gbs = do
    let (OpenConfigBuffer openConfigState) = gbsBuffer gbs
    in  focusRing openConfigState
-}

-- FIXME: Hacky and hard-coded... minor improvemnt would be to
-- LENGTH the head and footer...
scrollFocusNext :: F.FocusRing AnyName -> T.EventM AnyName ()
scrollFocusNext ring =
  case F.focusGetCurrent ring of
    Just someFieldName ->
      if someFieldName == lastFieldName
        -- FIXME
        then beginningScroll
        else M.vScrollBy myNameScroll 2
    Nothing            -> beginningScroll

-- FIXME: SHOULD REPLACE WITH SHOW MAP ENUM THING OF ITEMTYPES
-- in gopher.hs, like an all types enum bounds
allTypesAsStrings :: [[Char]]
allTypesAsStrings =
  [ "File"
  , "Directory"
  , "CsoPhonebookServer"
  , "Error"
  , "BinHexedMacintoshFile"
  , "DosBinaryArchive"
  , "UnixUuencodedFile"
  , "IndexSearchServer"
  , "TextBasedTelnetSession"
  , "BinaryFile"
  , "RedundantServer"
  , "GifFile"
  , "ImageFile"
  , "TN3270Session"
  , "Document"
  , "HtmlFile"
  , "InformationalMessage"
  , "SoundFile"
  ]

scrollFocusPrev :: F.FocusRing AnyName -> T.EventM AnyName ()
scrollFocusPrev ring =
  case F.focusGetCurrent ring of
    Just someFieldName ->
      if someFieldName == firstFieldName
        -- FIXME
        then endScroll
        else M.vScrollBy myNameScroll (-2)
    Nothing            -> endScroll
 where
  endScroll = M.vScrollToEnd myNameScroll >> M.vScrollBy myNameScroll (-2)

-- Should send notification about being saved TODO in status
saveConfig :: OpenConfigState -> IO ()
saveConfig openConfig = do
  -- First create the new config from the editor values (don't save yet!)...
  -- I feel like this could be easily accomplished with fold monad...
  let emptyConf = case (CF.add_section customEmptyCP "open-assocs") of
                    Left readError -> error (show readError)
                    Right cp       -> cp
      pendingConf = foldl (\b a -> pendingSet b a) emptyConf allTypesAsStrings

  -- Now read the existing config, do a merge and write!
  userConfig <- getUserOpenConfig
  let finalConfig = CF.merge userConfig pendingConf
  outPath <- getUserOpenConfigPath
  writeFile outPath (CF.to_string finalConfig)
 where
  -- There will always be at least [""] due to the way edit fields work in `Brick`.
  getEditValue recordFunc = head $ E.getEditContents (recordFunc openConfig)

  -- TODO: need to handle with Either
  --pendingSet :: CF.ConfigParser -> String -> CF.ConfigParser
  pendingSet conf itemType =
    -- This logic could be a higher order function for config
    let val = CF.set conf "open-assocs" itemType (getEditValue (itemTypeStrToEdit itemType))
    in  case val of
      Left readError -> error (show readError)
      Right cp       -> cp

  itemTypeStrToEdit itemType =
    -- TODO: this should really be a general map somewhere to do a lookup
    case itemType of
      "File" -> editFile
      "Directory" -> editDirectory
      "CsoPhonebookServer" -> editCsoPhoneBookServer
      "Error" -> editError
      "BinHexedMacintoshFile" -> editBinHexedMacintoshFile
      "DosBinaryArchive" -> editDosBinaryArchive
      "UnixUuencodedFile" -> editUnixUuencodedFile
      "IndexSearchServer" -> editIndexSearchServer
      "TextBasedTelnetSession" -> editTextBasedTelnetSession
      "BinaryFile" -> editBinaryFile
      "RedundantServer" -> editRedundantServer
      "GifFile" -> editGifFile
      "ImageFile" -> editImageFile
      "TN3270Session" -> editTn3270Session
      "Document" -> editDoc
      "HtmlFile" -> editHtmlFile
      "InformationalMessage" -> editInformationalMessage
      "SoundFile" -> editSoundFile
      _           -> error "impossible. placeholder here."

addSavedPopup :: GopherBrowserState -> GopherBrowserState
addSavedPopup gbs =
  let pop = Popup
              { pLabel = "Saved!"
              , pWidgets = [txt "Your changes were saved successfully!"]
              , pHelp = "Saved to ~/.config/waffle/open.ini"
              }
  in gbs { gbsPopup = Just pop }
