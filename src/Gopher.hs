{-# LANGUAGE OverloadedStrings #-}

-- TODO: use FileSig for file detection taking in a file path or bytes?
-- TODO/FIXME: implement data ItemType = Canonical CanonicalItemType | NonCanonical NonCanonicalItemType
-- TODO: more stuff from UI needs to go in here for clearer separation?
-- TODO: resource is misnamed. it's "selector string."
-- TODO: menu and directory seem to be synonymous
-- TODO: GopherItemType = GopherNonCanonicalItemType | GopherCanonicalItemType
-- and then explain:
--
-- RFC 1436 states "The client software decides what items are available by looking at
-- the first character of each line in a directory listing."

-- | Models the Gopher protocol; protocol representation, as per RFC 1436, with
-- some leniency for noncanonical features.
--
-- Mostly handles parsing Gopher menus from text to representations/types.
--
-- Currently no support for Gopher+.
--
-- I found these resources very useful in implementing the models herein:
--
--   * RFC 1436 - The Internet Gopher Protocol (a distributed document search and retrieval protocol)
--     https://tools.ietf.org/html/rfc1436
--   * Gopher (protocol) - Wikipedia
--     https://en.wikipedia.org/wiki/Gopher_(protocol)
--   * Gopher+ - Wikipedia
--     https://en.wikipedia.org/wiki/Gopher%2B
--   * RFC 4266 - The gopher URI Scheme
--     https://datatracker.ietf.org/doc/rfc4266/?include_text=1
module Gopher
  (
  -- * Models for Gopher menus and other representations
  -- $gopherMenus
    GopherMenu(GopherMenu)
  , MenuLine(..)
  , ParsedLine(..)
  , UnparseableLine
  , Selector
  -- ** Models for item types
  , ItemType(..)
  , GopherNonCanonicalItemType(..)
  , GopherCanonicalItemType(..)
  -- ** Utilities for Gopher menu models
  , selectorItemType
  , isInfoMsg
  , makeGopherMenu
  , parentDirectory
  , searchSelector
  , menuLine
  , explainLine
  , menuLineAsText
  , cleanAll
  ) where

import qualified Data.Text        as T
import           Text.Read
import           System.FilePath   (takeExtension, splitPath, joinPath)

-- | The types of lines in a Gopher menu which are types specified by the original
-- Gopher protocol specification (RFC 1435).
data GopherCanonicalItemType =
  File |
  -- ^ Item is a (plaintext) file. The item is a TextFile Entity. Client
  -- Should use a TextFile Transaction.
  Directory |
  -- ^ Gopher submenu. The item is a Menu Entity. Client should use a Menu
  -- Transaction.
  CsoPhoneBookServer |
  -- ^ CcsoNameServer. The information applies to a CSO phone book entity.
  -- Clients should talk CSO protocol.
  Error |
  -- ^ Signals an error condition.
  BinHexedMacintoshFile |
  -- ^ Item is a Macintosh file encoded in BINHEX format.
  DosBinaryArchive |
  -- ^ Item is PC-DOS binary file of some sort.  Client gets to decide.
  -- DOS binary archive of some sort. Client must read until TCP connection closes. beware
  UnixUuencodedFile |
  -- ^ UNIX uuencoded file; item is a uuencoded file.
  IndexSearchServer |
  -- ^ Gopher full-text search. The information applies to a Index Server.
  -- Client should use a FullText Search transaction.
  TextBasedTelnetSession |
  -- ^ Text-based telnet session. The information applies to a Telnet session.
  -- Connect to given host at given port. The name to login as at this host is
  -- in the selector string.
  BinaryFile |
  -- ^ Client must read until tcp connection closes, beware. Item is a binary file. Client
  -- must decide what to do with it.
  RedundantServer |
  -- ^ The information applies to a duplicated server. The information contained within is
  -- a duplicate of the primary server. THe primary server is defined as the last DirEntity
  -- that has a non-plus "type" field. The client should use the transaction as defined by
  -- the primary server type field.
  GifFile |
  -- ^ Item is a GIF graphic file.
  ImageFile |
  -- ^ Item is some kind of image file. Client gets to decide.
  Tn3270Session
  -- ^ Telnet 3270. The information applies to a tn3270 based telnet session.
  -- Connect to given host at given port. The name to login as at this
  -- host is in the selector string.
  deriving (Eq, Show)

-- | Item types improvised by Gopher client authors and others after the original
-- Gopher protocol specification (RFC 1436).
data GopherNonCanonicalItemType =
  Doc |
  -- ^ Doc. Seen used alongside PDFs and .DOCs.
  HtmlFile |
  -- ^ HTML file.
  InformationalMessage |
  -- ^ Informational message.
  SoundFile
  -- ^ Sound file (especially the WAV format).
  deriving (Eq, Show)

-- | ItemType of a Gopher document, as described in Gopher URIs (hopefully) or
-- in Gophermap/`GopherMenu` items.
data ItemType = Canonical GopherCanonicalItemType | NonCanonical GopherNonCanonicalItemType
  deriving (Eq, Show)

-- TODO, FIXME: change the gl prefix
-- | Representation of a recognized (something I can interpret; formatted as expected)
-- line in a 'GopherMenu'.
--
-- A Gopher protocol item/line is defined with tab-delimitated fields. This
-- abstraction makes it easier to handle said lines. The line itself will look
-- something like this (where 'F' is a tab):
--
-- > 1Display stringFselector stringFhostFportFextrastuff\CRLF
data ParsedLine = ParsedLine
  { glType :: ItemType
  -- ^ The first character on each line tells you whether the line/item
  -- describes a document, directory, or search device. AKA "definition."
  , glDisplayString :: T.Text
  -- ^ To be shown to the user for use in selecting this document
  -- (or directory) for retrieval.
  , glSelector :: Selector
  -- ^ A selector string that the client software mus tsend to the server
  -- to retrieve the documen t(or directory listing).  The selector string
  -- should mean nothing to the client software; it should never be modified by
  -- the client. In practice, the selector string is often a pathname or other
  -- file selector used by the server to locate the item desired. Also referred
  -- to as the "magic string."
  , glHost :: T.Text
  -- ^ the domain-name of the host that has this document (or directory) and...
  , glPort :: Int
  -- ^ ..continuing from glHost, the port at which to connect.
  , glGopherPlus :: [T.Text]
  -- ^ Any extra fields are Gopher+ fields and not a part of the original
  -- Gopher Protocol specification.
  }

-- | Determine a selector's `ItemType` based off file extension.
--
-- To determine a selector's item type based on RFC 4266 (like /1/somemap, or
-- /s/file.ogg) see `selectorPrefixItemType`.
--
-- >>> selectorExtToItemType $ T.pack "/foo/bar.ogg"
-- Just (NonCanonical SoundFile)
--
-- >>> selectorExtToItemType $ T.pack "/some/gophermap"
-- Just (Canonical Directory)
selectorExtToItemType :: Selector -> Maybe ItemType
selectorExtToItemType selector =
  let extension = takeExtension (T.unpack selector)
  in  case extension of
        -- Canonical types
        ""      -> Just $ Canonical Directory  -- TODO: should we rename Directory to GopherMap or *Menu* or something?
        ".gif"  -> Just $ Canonical GifFile
        -- Canonical types: (text) File
        ".txt"  -> Just $ Canonical File
        ".md"  -> Just $ Canonical File
        -- Canonical types: ImageFile
        ".jpg"  -> Just $ Canonical ImageFile
        ".ico"  -> Just $ Canonical ImageFile
        ".svg"  -> Just $ Canonical ImageFile
        ".tif"  -> Just $ Canonical ImageFile
        ".tiff" -> Just $ Canonical ImageFile
        ".jpeg" -> Just $ Canonical ImageFile
        ".png"  -> Just $ Canonical ImageFile
        -- Noncanonical types: SoundFile
        ".wav"  -> Just $ NonCanonical SoundFile
        ".mid"  -> Just $ NonCanonical SoundFile
        ".midi" -> Just $ NonCanonical SoundFile
        ".mp3"  -> Just $ NonCanonical SoundFile
        ".ogg"  -> Just $ NonCanonical SoundFile
        ".flac" -> Just $ NonCanonical SoundFile
        -- Noncanonical types: Documents
        ".doc"  -> Just $ NonCanonical Doc
        ".docx" -> Just $ NonCanonical Doc
        ".rtf"  -> Just $ NonCanonical Doc
        ".tex"  -> Just $ NonCanonical Doc
        ".odt"  -> Just $ NonCanonical Doc
        ".pdf"  -> Just $ NonCanonical Doc
        _       -> Nothing

-- | According to RFC 4266 we should be able to determine the resource's type
-- from the selector portion (the path), although this is often not the case
-- in reality, so when that's not the case we return Nothing.
--
-- To determine a file's `ItemType` based on file extension, please use
-- `selectorExtToItemType`.
--
-- >>> selectorPrefixItemType $ T.pack "/1/some/gopher/map"
-- Just (Canonical Directory)
--
-- >>> selectorPrefixItemType $ T.pack "/s/some/file.ogg"
-- Just (NonCanonical SoundFile)
--
-- >>> selectorPrefixItemType $ T.pack "/some/path"
-- Nothing
selectorPrefixItemType :: Selector -> Maybe ItemType
selectorPrefixItemType selector =
  let potentialItemChar = selectorToItemChar selector
  in  case potentialItemChar of
        (Just itemChar) -> charToItemType itemChar
        Nothing         -> Nothing
  where
    selectorToItemChar :: Selector -> Maybe Char
    selectorToItemChar selector' =
      let splitDirectories = filter (/= "") $ T.splitOn "/" selector'
      in  if not (null splitDirectories) && T.length (head splitDirectories) == 1
            then Just $ T.head . head $ splitDirectories
            else Nothing

-- TODO: clean up... lots of casses unneeded?
-- | Efficiently determine the `ItemType` from selector alone.
selectorItemType :: Selector -> Maybe ItemType
selectorItemType selector = do
  -- First we see if the selector already informs us of the file type.
  let maybeSomeType = selectorPrefixItemType selector
  case maybeSomeType of
    (Just someType) -> Just someType
    -- If we cannot determine the `ItemType` from the selector prefix we use the file extension.
    Nothing         ->
      let maybeTypeFromExt = selectorExtToItemType selector
      in  case maybeTypeFromExt of
            (Just typeFromExt) -> Just typeFromExt
            Nothing            -> Nothing

-- NOTE: is "Unrecognized" better than "Unparseable."
-- | Representation of a line in a GopherMenu which is either of an unrecognized
-- type or is simply not formatted correctly/malformed.
--
-- If it's not a ParsedLine then it's this.
newtype UnparseableLine = UnparseableLine [T.Text]

-- | A line in a 'GopherMenu'.
data MenuLine = Parsed ParsedLine | Unparseable UnparseableLine

menuLineAsText :: MenuLine -> T.Text
menuLineAsText (Parsed parsedLine)        =
  let indent = if glType parsedLine == NonCanonical InformationalMessage then "          " else ""
  in  indent <> glDisplayString parsedLine
menuLineAsText (Unparseable unparsedLine) =
  let (UnparseableLine fields) = unparsedLine
  in  "    " <> (cleanAll .T.pack $ show fields) <> " (UNPARSED LINE)"

-- | Take the character from a menu line delivered from a Gopher server and give
-- back the type of the item that line describes.
--
-- "One-character code indicates what kind of content the client should expect.
-- This code may be either a digit or a letter of the alphabet; letters are
-- case-sensitive."
charToItemType
  :: Char -> Maybe ItemType
-- Left (canonical)
charToItemType '0' = Just $ Canonical File
charToItemType '1' = Just $ Canonical Directory
charToItemType '2' = Just $ Canonical CsoPhoneBookServer
charToItemType '3' = Just $ Canonical Error
charToItemType '4' = Just $ Canonical BinHexedMacintoshFile
charToItemType '5' = Just $ Canonical DosBinaryArchive
charToItemType '6' = Just $ Canonical UnixUuencodedFile
charToItemType '7' = Just $ Canonical IndexSearchServer
charToItemType '8' = Just $ Canonical TextBasedTelnetSession
charToItemType '9' = Just $ Canonical BinaryFile
charToItemType '+' = Just $ Canonical RedundantServer
charToItemType 'T' = Just $ Canonical Tn3270Session
charToItemType 'g' = Just $ Canonical GifFile
charToItemType 'I' = Just $ Canonical ImageFile
-- Right (noncanonical)
charToItemType 'd' = Just $ NonCanonical Doc
charToItemType 'h' = Just $ NonCanonical HtmlFile
charToItemType 'i' = Just $ NonCanonical InformationalMessage
charToItemType 's' = Just $ NonCanonical SoundFile
charToItemType _   = Nothing

showAddressPlus :: ParsedLine -> T.Text
showAddressPlus gl =
  glHost gl <> ":" <> T.pack (show $ glPort gl) <> " " <> glSelector gl <> " " <> T.pack (show $ glGopherPlus gl)

-- | A "selector" is the magic string used to locate documents in a Gopherhole.
-- This is is sent to a connected host on Gopherspace with `Network.Simple.TCP.send`,
-- which is why the datatype is `ByteString`.
-- datatype.
type Selector = T.Text

-- Fixme: for these three... rename to explain*ItemType?
-- FIXME: update with the description from one of those docstrings...
explainCanonicalType :: GopherCanonicalItemType -> T.Text
explainCanonicalType File =
  "Item is a file. Plaintext file. "
explainCanonicalType Directory = "Item is a directory. Gopher submenu."
explainCanonicalType CsoPhoneBookServer =
  "Item is a CSO phone-book server. CCSO Nameserver."
explainCanonicalType Error =
  "Error. Error code returned by a Gopher server to indicate failure."
explainCanonicalType BinHexedMacintoshFile =
  "Item is a BinHexed Macintosh file. BinHex-encoded file (primarily for Macintosh computers)."
explainCanonicalType DosBinaryArchive
  = "Item is DOS binary archive of some sort. Client must read until the TCP connection closes. Beware. DOS file."
explainCanonicalType UnixUuencodedFile =
  "Item is a UNIX uuencoded file. uuencoded file."
explainCanonicalType IndexSearchServer =
  "Item is an Index-Search server. Gopher full-text search."
explainCanonicalType TextBasedTelnetSession =
  "Item points to a text-based telnet session. Telnet."
explainCanonicalType BinaryFile =
  "Item is a binary file! Client must read until the TCP connection closes. Beware. Binary file."
explainCanonicalType RedundantServer
  = "Item is a redundant server. Mirror or alternate server (for load balancing or in case of primary server downtime)."
explainCanonicalType Tn3270Session =
  "Item points to a text-based tn3270 session. Telnet 3270."
explainCanonicalType GifFile = "Item is a GIF format graphics file. GIF file."
explainCanonicalType ImageFile =
  "Item is some kind of image file.  Client decides how to display. Image file."

explainNonCanonicalType :: GopherNonCanonicalItemType -> T.Text
explainNonCanonicalType Doc = "Doc. Seen used alongside PDF's and .DOC's."
explainNonCanonicalType HtmlFile = "HTML file."
explainNonCanonicalType InformationalMessage = "Informational message."
explainNonCanonicalType SoundFile = "Sound file (especially the WAV format)."

-- FIXME: delete
-- TODO: should actually use gopherline information to describe the type further, like info about it?
-- FIXME: what about malformed item type?
-- this could just all be a part of an elaborate show instance?

-- TODO: explainanytype?
-- | Explain a Gopher line/menu item type, either canonical (RFC 1436) or non canonical.
explainType
  :: ParsedLine -> T.Text
explainType gopherLine = case glType gopherLine of
  Canonical  canonType      -> explainCanonicalType canonType <> " " <> showAddressPlus gopherLine
  NonCanonical nonCanonType -> explainNonCanonicalType nonCanonType <> " " <> showAddressPlus gopherLine

-- TODO/FIXME: better haddock string, use REPL example too?
-- | Describe any line from a Gopher menu.
explainLine :: MenuLine -> T.Text
explainLine (Parsed parsedLine)        =
  explainType parsedLine
explainLine (Unparseable unparsedLine) =
  let (UnparseableLine fields) = unparsedLine
  in  "Malformed, unrecognized, or incorrectly parsed. " <> T.pack (show fields)

-- | Parse a Gopher-menu-formatted Text into a 'GopherMenu' representation.
makeGopherMenu :: T.Text -> GopherMenu
makeGopherMenu rawString = GopherMenu $ map makeMenuLine rowsOfFields
 where
  -- A period on a line by itself denotes the end.
  rmTerminatorPeriod l = if last l == ".\r" then init l else l
  -- TODO: this could use some explaining... basically prep things for being parsed
  -- into type...
  rowsOfFields :: [[T.Text]]
  rowsOfFields =
    let linesWithoutTerminator = rmTerminatorPeriod . T.lines $ rawString
    in  map (T.splitOn "\t") linesWithoutTerminator

  -- | Create a MenuLine from a series of fields.
  makeMenuLine :: [T.Text] -> MenuLine
  makeMenuLine fields =
    let potentiallyParsedLine = parseMenuLine fields
    in case potentiallyParsedLine of
      Nothing           -> Unparseable $ UnparseableLine fields
      (Just parsedLine) -> Parsed parsedLine

  -- NOTE: this pattern match works well because everything after port gets lumped
  -- into a list of Gopher+ fields. Otherwise, it'll just be an empty list!
  -- | Attempt to parse a menu line.
  parseMenuLine :: [T.Text] -> Maybe ParsedLine
  -- We have all of the necessary fields to potentially create a ParsedLine
  parseMenuLine (typeCharAndDisplayString : selector : host : port : gopherPlus)
    = let typeChar       = getFirstChar typeCharAndDisplayString-- NOTE: Seems a hacky way to string to char...
          displayString  = dropFirstChar typeCharAndDisplayString
          maybePortAsInt = maybeInt port
      in case maybePortAsInt of
        Nothing          -> Nothing
        (Just portAsInt) -> case charToItemType typeChar of
          -- The item type indicator is recognized...
          Just x  -> Just $ ParsedLine
            { glType          = x
            , glDisplayString = displayString-- FIXME: do we need to clean this?
            , glSelector      = selector
            , glHost          = host
            , glPort          = portAsInt
            , glGopherPlus    = gopherPlus
            }
          --- The item type indicator is not recognized...
          Nothing -> Nothing
      where
        maybeInt someText = readMaybe . T.unpack $ someText :: Maybe Int
        -- These two functions work assuming the display string is preceded by the item type character
        dropFirstChar     = T.drop 1
        getFirstChar      = T.head
  -- We do not have all of the necessary fields to create a ParsedLine...
  parseMenuLine _ = Nothing

-- | Representation of Gopher menus: a list of 'MenuLine's.
newtype GopherMenu = GopherMenu [MenuLine]

-- FIXME: is this used anywhere? replaces show instance for gophermenu
-- | Easily represent a GopherMenu as a 'Text', formatted as it might be rendered.
-- gopherMenuAsText (GopherMenu menuLines) = T.unlines $ map menuLineAsText menuLines

-- | Detect if a Gopher menu line is of the noncanonical 'InformationalMessage' type.
isInfoMsg :: MenuLine -> Bool
isInfoMsg line = case line of
  -- It's a ParsedLine
  (Parsed gl) -> case glType gl of
    -- Canonical type
    (Canonical  _  ) -> False
    -- Noncanonical type
    (NonCanonical nct) -> nct == InformationalMessage
  -- It's an UnparseableLine
  (Unparseable _) -> False

-- TODO: maybe should take/return FilePath...
-- | Get the selector (also referred to as the "path" or "magic string"
-- for the parent directory/menu of the supplied selector, if possible
-- (may already be at root).
--
-- >>> parentDirectory $ T.pack "/"
-- Nothing
--
-- >>> parentDirectory $ T.pack ""
-- Nothing
--
-- >>> parentDirectory $ T.pack "/foo/bar/hello/world"
-- Just "/foo/bar/hello/"
parentDirectory :: T.Text -> Maybe T.Text
parentDirectory ""       = Nothing
parentDirectory "/"      = Nothing
parentDirectory selector =
  Just (T.pack $ joinPath . init . splitPath . T.unpack $ selector)

-- | Given the search "endpoint" (the selector for the search), create a
-- new selector for querying the supplied endpoint for the supplied query.
--
-- This is very similar to the URLs you'd expect from GET requests and
-- query strings in HTTP.
--
-- >>> searchSelector (T.pack "/somesearch") (T.pack "foo bar")
-- "/somesearch\tfoo bar"

-- >>> searchSelector (T.pack "") (T.pack "foo bar")
-- "foo bar"
searchSelector :: Selector -> T.Text -> Selector
searchSelector resource query =
  -- FIXME: I don't think this if null resource then query bit would actually work
  -- as seen in the REPL example, would it?
  if T.null resource then query else resource <> "\t" <> query

-- | Get the nth line from a 'GopherMenu'.
menuLine :: GopherMenu -> Int -> MenuLine
menuLine (GopherMenu ls) indx = ls !! indx

-- | Replace tabs with spaces. Used mostly for text files, because
-- `MenuLine`s don't have this issue. Attempting to display tabs to
-- the terminal will have unintended consequences.
--
-- >>> cleanTabs $ T.pack "\tfoo\tbar\t"
-- " foo bar "
cleanTabs :: T.Text -> T.Text
cleanTabs    = T.map (\x -> if x == '\t' then ' ' else x)

-- | Replace carriage returns with spaces. Used for text files and
-- `MenuLine`s.
--
-- >>> cleanReturns $ T.pack "foo\rbar"
-- "foo bar"
cleanReturns :: T.Text -> T.Text
cleanReturns = T.map (\x -> if x == '\r' then ' ' else x)

-- TODO: fix the lines right off the bat with replace returns!
-- FIXME: should be cleanMenuLine and cleanTextFile because menuLines don't
-- need tabs replaced due to it being tab-delimited already! I believe...
-- NOTE: and what about the returns?
-- FIXME: Is this appropriate for here? maybe another module?
-- | Replaces certain characters to ensure the Brick UI doesn't get "corrupted."
cleanAll :: T.Text -> T.Text
cleanAll = cleanTabs . cleanReturns

{- $gopherMenus

A Gopher
menu in its raw form is essentially just tab-delimited text, where each row is a
"menu item." These menu lines are represented by 'MenuLine' in two forms: 'Parsed'
and 'Unparseable'. Sometimes you come across rows that are malformed for one reason
or another, or they have some custom implementation, so it's better just to display
the 'Unparseable' lines as-is and render the 'Parsed' (recognized) lines as
expected.

-}

