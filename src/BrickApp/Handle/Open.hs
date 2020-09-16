module BrickApp.Handle.Open where

import           Control.Monad.IO.Class

import qualified Brick.Focus as F
import qualified Brick.Main                    as M
import qualified Brick.Types                   as T
import           Brick.Widgets.Edit            as E
import qualified Graphics.Vty                  as V
import           Graphics.Vty.Input.Events      ( Event )

import BrickApp.Types
import BrickApp.Types.Names
import BrickApp.Types.Helpers
import BrickApp.ModeAction.Open

-- | The Brick application event handler for open config mode. See: UI.appEvent and
--- Brick.Main.appHandleEvent.
openConfigEventHandler
  :: GopherBrowserState -> Event -> T.EventM AnyName (T.Next GopherBrowserState)
openConfigEventHandler gbs e = case e of
  V.EvKey V.KBackTab [] -> M.continue (updateGbs gbs focusPrev)
  V.EvKey (V.KChar '\t') [] -> M.continue (updateGbs gbs focusNext)
  V.EvKey (V.KChar 's') [V.MCtrl] -> liftIO (saveConfig (getOpenConfig gbs)) >> M.continue (addSavedPopup gbs)
    -- FIXME: ctrl+c quits!
  V.EvKey V.KEsc [] -> M.continue $ returnFormerGbs gbs
  _ -> handleFieldTypingEvent e gbs (getFocusRing gbs)

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


