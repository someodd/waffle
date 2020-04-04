-- TODO: more stuff from UI needs to go in here for clearer separation?
-- TODO: resource is misnamed. it's "selector string."
-- TODO: menu and directory seem to be synonymous
-- TODO: handle network problems
-- TODO: implement metadata that is in between ========================== that. also null.host?
-- TODO: left is preferred for errors. There is a reason for this. Read about that in LYAH
-- TODO: content detection. if not diretory then use text mode to display or another mode to download...
module GopherClient where

import           Data.List
import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString               as BS
import qualified Data.ByteString.UTF8          as U8
import           Data.List.Split

import           Network.Simple.TCP

-- FIXME: these comment descriptions should aso go to the explain function...
-- | Gopher Protocol/RFC 1435 canonical item type characters.
-- "The client software decides what items are available by looking at
-- the first character of each line in a directory listing."
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

-- | Item types improvised by Gopher client authors and others after RFC 1436,
-- see the Gopher+ specification...
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

-- FIXME: bring back glNumber for line #
-- | A Gopher protocol item/line is defined with tab-delimitated fields. This
-- abstraction makes it easier to handle said lines. The line itself will look
-- something like this (where 'F' is a tab):
--
--    1Display stringFselector stringFhostFportFextrastuff\CRLF
data GopherLine = GopherLine
  { glType :: Either GopherCanonicalItemType GopherNonCanonicalItemType
  -- ^ The first character on each line tells you whether the line/item
  -- describes a document, directory, or search device. AKA "definition."
  , glDisplayString :: String
  -- ^ To be shown to the user for use in selecting this document
  -- (or directory) for retrieval.
  , glSelector :: String
  -- ^ A selector string that the client software mus tsend to the server
  -- to retrieve the documen t(or directory listing).  The selector string
  -- should mean nothing to the client software; it should never be modified by
  -- the client. In practice, the selector string is often a pathname or other
  -- file selector used by the server to locate the item desired. Also referred
  -- to as the "magic string."
  , glHost :: String
  -- ^ the domain-name of the host that has this document (or directory) and...
  , glPort :: Int
  -- ^ ..continuing from glHost, the port at which to connect.
  , glGopherPlus :: [String]
  -- ^ Any extra fields are Gopher+ fields and not a part of the original
  -- Gopher Protocol specification.
  }

-- NOTE: for name, Malformed or Unrecognized?
-- | For Gopher lines which are not formatted correctly
newtype MalformedGopherLine = MalformedGopherLine
  { mglFields :: [String]
  }

-- | The way a GopherLine is displayed (string) after being parsed, namely used by the UI
instance Show GopherLine where
  show x = indent x ++ glDisplayString x
   where
    indent l =
      if glType l == Right InformationalMessage then "          " else ""

-- | Displaying a malformed Gopher line (string) after being parsed, namely used by the UI
instance Show MalformedGopherLine where
  show x = "    " ++ show (mglFields x)--FIXME: might this not error? add "ERROR" to end?

-- | Take the character from a menu line delivered from a Gopher server and give
-- back the type of the item that line describes.
--
-- "One-character code indicates what kind of content the client should expect.
-- This code may be either a digit or a letter of the alphabet; letters are
-- case-sensitive."
charToItemType
  :: Char -> Maybe (Either GopherCanonicalItemType GopherNonCanonicalItemType)
-- Left (canonical)
charToItemType '0' = Just $ Left File
charToItemType '1' = Just $ Left Directory
charToItemType '2' = Just $ Left CsoPhoneBookServer
charToItemType '3' = Just $ Left Error
charToItemType '4' = Just $ Left BinHexedMacintoshFile
charToItemType '5' = Just $ Left DosBinaryArchive
charToItemType '6' = Just $ Left UnixUuencodedFile
charToItemType '7' = Just $ Left IndexSearchServer
charToItemType '8' = Just $ Left TextBasedTelnetSession
charToItemType '9' = Just $ Left BinaryFile
charToItemType '+' = Just $ Left RedundantServer
charToItemType 'T' = Just $ Left Tn3270Session
charToItemType 'g' = Just $ Left GifFile
charToItemType 'I' = Just $ Left ImageFile
-- Right (noncanonical)
charToItemType 'd' = Just $ Right Doc
charToItemType 'h' = Just $ Right HtmlFile
charToItemType 'i' = Just $ Right InformationalMessage
charToItemType 's' = Just $ Right SoundFile
charToItemType _   = Nothing

showAddressPlus :: GopherLine -> String
showAddressPlus gl =
  glHost gl ++ ":" ++ show (glPort gl) ++ " " ++ glSelector gl ++ " " ++ show (glGopherPlus gl)

-- Fixme: for these three... rename to explain*ItemType?
-- FIXME: update with the description from one of those docstrings...
explainCanonicalType :: GopherCanonicalItemType -> String
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

explainNonCanonicalType :: GopherNonCanonicalItemType -> String
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
  :: GopherLine -> String
explainType gopherLine = case glType gopherLine of
  Left  canonType    -> explainCanonicalType canonType ++ " " ++ showAddressPlus gopherLine
  Right nonCanonType -> explainNonCanonicalType nonCanonType ++ " " ++ showAddressPlus gopherLine

-- TODO: needs to be more elaborate! use the actual data to construct
-- | Explain a line from a Gopher map/menu. Can be a GopherLine or a MalformedGopherLine.
explainLine
  :: Either GopherLine MalformedGopherLine -> String
explainLine (Left gopherLine) = explainType gopherLine
explainLine (Right malformedGopherLine) = "Malformed, unrecognized, or incorrectly parsed. " ++ show (mglFields malformedGopherLine)

-- | Take a big string (series of lines) returned from a Gopher request and
-- parse it into a Gopher menu (a series of GopherLines)!
makeGopherMenu :: String -> GopherMenu
makeGopherMenu rawString = GopherMenu $ map makeGopherLine rowsOfFields
 where
  -- A period on a line by itself denotes the end.
  rmTerminatorPeriod l = if last l == ".\r" then init l else l
  -- TODO: this could use some explaining... basically prep things for being parsed
  -- into type...
  rowsOfFields =
    let linesWithoutTerminator = rmTerminatorPeriod . lines $ rawString
    in  map (splitOn "\t") linesWithoutTerminator

  -- NOTE: this pattern match works well because everything after port gets lumped
  -- into a list of Gopher+ fields. Otherwise, it'll just be an empty list!
  makeGopherLine :: [String] -> Either GopherLine MalformedGopherLine
  makeGopherLine allFields@(typeCharAndDisplayString : selector : host : port : gopherPlus)
    = do
      let typeChar      = head $ take 1 typeCharAndDisplayString-- NOTE: Seems a hacky way to string to char...
      let displayString = drop 1 typeCharAndDisplayString
      case charToItemType typeChar of
        Just x -> Left $ GopherLine { glType          = x
                                    , glDisplayString = displayString
                                    , glSelector      = selector
                                    , glHost          = host
                                    , glPort          = read port--FIXME: what if this fails to int?
                                    , glGopherPlus    = gopherPlus
                                    }
        Nothing -> Right $ MalformedGopherLine { mglFields = allFields }
  makeGopherLine malformed =
    Right $ MalformedGopherLine { mglFields = malformed }

-- | As you can see, a GopherMenu is simply an ordered sequence of
-- GopherLines.
newtype GopherMenu = GopherMenu [Either GopherLine MalformedGopherLine]

fromMenu :: GopherMenu -> [Either GopherLine MalformedGopherLine]
fromMenu (GopherMenu m) = m

-- | Easily represent a GopherMenu as a string, formatted as it might be rendered.
instance Show GopherMenu where
  show (GopherMenu ls) = unlines $ map gopherLineShow ls
   where
    -- Seems a little hacky
    -- is normal line
    gopherLineShow (Left  x) = show x
    -- is malformed line
    gopherLineShow (Right x) = show x ++ "(MALFORMED LINE)"-- Does this have potential to break?

-- Nice benefit: AFAIK info msgs are the only kind that aren't followable
-- NOTE: should only be used in cases where there's no point in nested cases otherwise
isInfoMsg :: Either GopherLine MalformedGopherLine -> Bool
isInfoMsg line = case line of
  -- It's a GopherLine
  (Left gl) -> case glType gl of
    -- Canonical type
    (Left  _  ) -> False
    -- Noncanonical type
    (Right nct) -> nct == InformationalMessage
  -- It's a MalformedGopherLine
  (Right _) -> False

-- | Get the magic string for the parent directory/menu, if possible (may already be at root).
--
-- >> parentDirectory "/"
-- Nothing
--
-- >> parentDirectory ""
-- Nothing
--
-- >> parentDirectory "/foo/bar/hello/world"
-- Just "foo/bar/hello"
parentDirectory :: String -> Maybe String
parentDirectory magicString
  | magicString == "/" || null magicString = Nothing
  | otherwise = Just $ intercalate "/" (init $ wordsBy (== '/') magicString)

-- TODO: delete after implementing caching because it won't be used anymore due
-- to UI.Progress!
-- Lots of redundancy
downloadGet :: String -> String -> String -> IO BS.ByteString
downloadGet host port resource = connect host port $ \(connectionSocket, _) ->
  do
    send connectionSocket (B8.pack $ resource ++ "\r\n")
    -- need to only fetch as many bytes as it takes to get period on a line by itself to
    -- close the connection.
    getAllBytes (pure Nothing) 1024 (pure BS.empty) connectionSocket

newtype GetAllBytesCallback st = GetAllBytesCallback (st -> BS.ByteString -> IO st, st)

-- TODO: What about modifying this with a callback for progress...
getAllBytes
  :: IO (Maybe (GetAllBytesCallback a))
  -> Int
  -> IO BS.ByteString
  -> Socket
  -> IO BS.ByteString
getAllBytes callback recvChunks acc connectionSocket = do
  gosh <- recv connectionSocket recvChunks
  wacc <- acc
  case gosh of
    Nothing   -> acc
    Just chnk -> do
      cb <- callback
      let newCallbackArg = case cb of
            Just (GetAllBytesCallback (callback', gbs)) -> do
              newGbs <- callback' gbs chnk
              pure $ Just $ GetAllBytesCallback (callback', newGbs)
            Nothing -> pure Nothing
      getAllBytes newCallbackArg
                  recvChunks
                  (pure $ BS.append wacc chnk)
                  connectionSocket

-- TODO/FIXME: implement progressmode! it'd be almost exactly like menu
-- | Gopher protocol TCP/IP request. Leave "resource" as an empty/blank string
-- if you don't wish to specify.
searchGet :: String -> String -> String -> String -> IO (String, String)
searchGet host port resource query = do
  let selector = if null resource then query else resource ++ "\t" ++ query
  o <- connect host port $ \(connectionSocket, _) -> do
    send connectionSocket (U8.fromString $ selector ++ "\r\n")
    -- need to only fetch as many bytes as it takes to get period on a line by itself to
    -- close the connection.
    wow <- getAllBytes (pure Nothing) 1024 (pure BS.empty) connectionSocket
    pure $ B8.unpack wow--FIXME: u8.toString
  pure (o, selector)

-- TODO: delete after implementing caching because it won't be used anymore due
-- to UI.Progress!
-- | Gopher protocol TCP/IP request. Leave "resource" as an empty/blank string
-- if you don't wish to specify.
gopherGet :: String -> String -> String -> IO String
gopherGet host port resource = connect host port $ \(connectionSocket, _) -> do
  send connectionSocket (B8.pack $ resource ++ "\r\n")
  -- need to only fetch as many bytes as it takes to get period on a line by itself to
  -- close the connection.
  wow <- getAllBytes (pure Nothing) 1024 (pure BS.empty) connectionSocket
  pure $ U8.toString wow

menuLine :: GopherMenu -> Int -> Either GopherLine MalformedGopherLine
menuLine (GopherMenu ls) indx = ls !! indx
