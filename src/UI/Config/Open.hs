{-# LANGUAGE OverloadedStrings #-}

-- | UI for editing the open config for setting commands associated with
-- opening a menu item of specific types.
module UI.Config.Open where

import           Control.Monad.IO.Class
import           Data.List                      ( intersperse )

import qualified Brick.Focus as F
import Brick.Widgets.Core
  ( (<+>)
  , hLimit
  , vLimit
  , vBox
  , str
  , txt
  , viewport
  )
import qualified Brick.Main                    as M
import qualified Brick.Types                   as T
import           Brick.Widgets.Edit            as E
import qualified Graphics.Vty                  as V
import           Graphics.Vty.Input.Events      ( Event )
import qualified Data.ConfigFile               as CF

import           UI.Util                        ( defaultBrowserUI, myNameScroll )
import           UI.Representation
import           Config.ConfigOpen
import           Config

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
  -> T.EventM AnyName (T.Next GopherBrowserState)
initConfigOpenMode gbs = M.vScrollToBeginning myNameScroll >> (liftIO gbsToGiveBack >>= M.continue)
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

openConfigModeUI :: GopherBrowserState -> [T.Widget AnyName]
openConfigModeUI gbs =
  defaultBrowserUI gbs (viewport (MyName MyViewport) T.Vertical) titleWidget mainWidget statusWidget
 where
  {-
  fields =
    let renderEditor = E.renderEditor (str . unlines)
        fieldMaker = \x -> F.withFocusRing (focusRing openConfigState) renderEditor (x openConfigState)
    in  map fieldMaker ...
  -}
  openConfigState =
    let (OpenConfigBuffer ocs) = gbsBuffer gbs
    in  ocs
  titleWidget = txt "Config: Open Associations"
  statusWidget = txt "Set commands for item types. Use tab to cycle fields."

  vBoxHeader =
    [ str "Set the commands associated with opening specific menu item types."
    , str "Leave blank to use xdg-open."
    , str " "
    ]
  vBoxFooter =
    [ str " "
    , str "Press TAB to switch between editors, ESC to quit."
    ]

  mainWidget :: T.Widget AnyName
  mainWidget = vBox (vBoxHeader ++ editWidgets openConfigState ++ vBoxFooter)

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
  customRenderEditor :: Bool -> E.Editor String AnyName -> T.Widget AnyName
  customRenderEditor = E.renderEditor (str . unlines)

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

-- TODO/FIXME: i'm told i must fix this with lenses
-- | Handle typing in a field and produce an event for the open associations
-- configuration editor's `focusRing`.
handleFieldTypingEvent
  :: Event
  -> GopherBrowserState
  -> F.FocusRing AnyName
  -> T.EventM AnyName (T.Next GopherBrowserState)
handleFieldTypingEvent e gbs ring =
  M.continue =<< case F.focusGetCurrent ring of
           -- This replicates a lot of the data structure of the lookup map implemented for item type to field info FIXME TODO
           -- Should also have a lookup for fieldname to item type and the reverse.
           Just (FieldName FileField) -> updateFieldEvent editFile (\oc x -> oc { editFile = x })
           Just (FieldName DirectoryField) -> updateFieldEvent editDirectory (\oc x -> oc { editDirectory = x })
           Just (FieldName CsoPhoneBookServerField) -> updateFieldEvent editCsoPhoneBookServer (\oc x -> oc { editCsoPhoneBookServer = x })
           Just (FieldName ErrorField) -> updateFieldEvent editError (\oc x -> oc { editError = x })
           Just (FieldName BinHexedMacintoshFileField) -> updateFieldEvent editBinHexedMacintoshFile (\oc x -> oc { editBinHexedMacintoshFile = x })
           Just (FieldName DosBinaryArchiveField) -> updateFieldEvent editDosBinaryArchive (\oc x -> oc { editDosBinaryArchive = x })
           Just (FieldName UnixUuencodedFileField) -> updateFieldEvent editUnixUuencodedFile (\oc x -> oc { editUnixUuencodedFile = x })
           Just (FieldName IndexSearchServerField) -> updateFieldEvent editIndexSearchServer (\oc x -> oc { editIndexSearchServer = x })
           Just (FieldName TextBasedTelnetSessionField) -> updateFieldEvent editTextBasedTelnetSession (\oc x -> oc { editTextBasedTelnetSession = x })
           Just (FieldName BinaryFileField) -> updateFieldEvent editBinaryFile (\oc x -> oc { editBinaryFile = x })
           Just (FieldName RedundantServerField) -> updateFieldEvent editRedundantServer (\oc x -> oc { editRedundantServer = x })
           Just (FieldName GifFileField) -> updateFieldEvent editGifFile (\oc x -> oc { editGifFile = x })
           Just (FieldName ImageFileField) -> updateFieldEvent editImageFile (\oc x -> oc { editImageFile = x })
           Just (FieldName Tn3270SessionField) -> updateFieldEvent editTn3270Session (\oc x -> oc { editTn3270Session = x })
           Just (FieldName DocField) -> updateFieldEvent editDoc (\oc x -> oc { editDoc = x })
           Just (FieldName HtmlFileField) -> updateFieldEvent editHtmlFile (\oc x -> oc { editHtmlFile = x })
           Just (FieldName InformationalMessageField) -> updateFieldEvent editInformationalMessage (\oc x -> oc { editInformationalMessage = x })
           Just (FieldName SoundFileField) -> updateFieldEvent editSoundFile (\oc x -> oc { editSoundFile = x })
           Just _ -> error "this should be impossible..."
           Nothing -> pure gbs
 where
  updateFieldEvent recordFunc updater =
          let relevantEditor          = recordFunc (getOpenConfig gbs)
              updateFileEditor x      = gbs { gbsBuffer = OpenConfigBuffer $ updater (getOpenConfig gbs) x  }
          in  updateFileEditor <$> E.handleEditorEvent e relevantEditor

-- | The Brick application event handler for open config mode. See: UI.appEvent and
--- Brick.Main.appHandleEvent.
openConfigEventHandler
  :: GopherBrowserState -> Event -> T.EventM AnyName (T.Next GopherBrowserState)
openConfigEventHandler gbs e = case e of
  V.EvKey V.KBackTab [] -> scrollFocusPrev (getFocusRing gbs) >> M.continue (updateGbs gbs focusPrev)
  V.EvKey (V.KChar '\t') [] -> scrollFocusNext (getFocusRing gbs) >> M.continue (updateGbs gbs focusNext)
  V.EvKey (V.KChar 's') [V.MCtrl] -> liftIO (saveConfig (getOpenConfig gbs)) >> M.continue gbs
    -- FIXME: esc quits! Change key...
  V.EvKey V.KEsc   [] -> M.continue $ returnFormerGbs gbs
  _ -> handleFieldTypingEvent e gbs (getFocusRing gbs)
