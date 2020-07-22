module BrickApp.Types.Names where

-- FIXME: this is not specific! Should be OpenConfigFieldNames!
data FieldName = FileField
              | DirectoryField
              | CsoPhoneBookServerField
              | ErrorField
              | BinHexedMacintoshFileField
              | DosBinaryArchiveField
              | UnixUuencodedFileField
              | IndexSearchServerField
              | TextBasedTelnetSessionField
              | BinaryFileField
              | RedundantServerField
              | GifFileField
              | ImageFileField
              | Tn3270SessionField
              | DocField
              | HtmlFileField
              | InformationalMessageField
              | SoundFileField
              deriving (Bounded, Enum, Show, Ord, Eq)

data AnyName = FieldName FieldName | MyName MyName
  deriving (Show, Eq, Ord)

data MyName = MyViewport | MainViewport | EditorViewport | MyWidget | TextViewport | MenuViewport
  deriving (Show, Eq, Ord)

-- FIXME: This name is very bad!
data EditName = Edit1 deriving (Ord, Show, Eq)

