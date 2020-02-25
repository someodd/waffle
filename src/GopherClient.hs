module GopherClient where

import Data.Maybe
import qualified Data.ByteString.Char8 as B8
import Data.List.Utils (replace)
import Data.List.Split

import Network.Simple.TCP

{-|
Gopher Protocol/RFC 1435 canonical item type characters.

"The client software decides what items are available by looking at
the first character of each line in a directory listing."
-}
-- FIXME: these comment descriptions should aso go to the explain function...
data GopherCanonicalItemType =
    -- | Item is a (plaintext) file. The item is a TextFile Entity. Client
    -- Should use a TextFile Transaction.
    File |
    -- | Gopher submenu. The item is a Menu Entity. Client should use a Menu
    -- Transaction.
    Directory |
    -- | CcsoNameServer. The information applies to a CSO phone book entity.
    -- Clients should talk CSO protocol.
    CsoPhoneBookServer |
    -- | Signals an error condition.
    Error |
    -- | Item is a Macintosh file encoded in BINHEX format.
    BinHexedMacintoshFile |
    -- | Item is PC-DOS binary file of some sort.  Client gets to decide.
    -- DOS binary archive of some sort. Client must read until TCP connection closes. beware
    DosBinaryArchive |
    -- | UNIX uuencoded file; item is a uuencoded file.
    UnixUuencodedFile |
    -- | Gopher full-text search. The information applies to a Index Server.
    -- Client should use a FullText Search transaction.
    IndexSearchServer |
    -- | Text-based telnet session. The information applies to a Telnet session.
    -- Connect to given host at given port. The name to login as at this host is
    -- in the selector string.
    TextBasedTelnetSession |
    -- | Client must read until tcp connection closes, beware. Item is a binary file. Client
    -- must decide what to do with it.
    BinaryFile |
    -- | The information applies to a duplicated server. The information contained within is
    -- a duplicate of the primary server. THe primary server is defined as the last DirEntity
    -- that has a non-plus "type" field. The client should use the transaction as defined by
    -- the primary server type field.
    RedundantServer |
    -- | Item is a GIF graphic file.
    GifFile |
    -- | Item is some kind of image file. Client gets to decide.
    ImageFile |
    -- | Telnet 3270. The information applies to a tn3270 based telnet session.
    -- Connect to given host at given port. The name to login as at this
    -- host is in the selector string.
    Tn3270Session
    deriving (Eq)

-- | Item types improvised by Gopher client authors and others after RFC 1436,
-- see the Gopher+ specification...
data GopherNonCanonicalItemType =
    -- | Doc. Seen used alongside PDFs and .DOCs.
    Doc |
    -- | HTML file.
    HtmlFile |
    -- | Informational message.
    InformationalMessage |
    -- | Sound file (especially the WAV format).
    SoundFile
    deriving (Eq)

{-|
A Gopher protocol item/line is defined with tab-delimitated fields. This
abstraction makes it easier to handle said lines. The line itself will look
something like this (where 'F' is a tab):

    1Display stringFselector stringFhostFportFextrastuff<CRLF>

-}
-- FIXME: perhaps rename to DirectoryEntity?
data GopherLine = GopherLine {
    -- | The first character on each line tells you whether the line/item
    -- describes a document, directory, or search device. AKA "definition."
     glType :: Either GopherCanonicalItemType GopherNonCanonicalItemType
    -- | This is the nth line the server sent for this directory.
    ,glLineNumber :: Int
    -- | To be shown to the user for use in selecting this document
    -- (or directory) for retrieval.
    ,glDisplayString :: String
    -- | A selector string that the client software mus tsend to the server
    -- to retrieve the documen t(or directory listing).  The selector string
    -- should mean nothing to the client software; it should never be modified by
    -- the client. In practice, the selector string is often a pathname or other
    -- file selector used by the server to locate the item desired. Also referred
    -- to as the "magic string."
    ,glSelector :: String
    -- | the domain-name of the host that has this document (or directory) and...
    ,glHost :: String
    -- | ..continuing from glHost, the port at which to connect.
    ,glPort :: Int
    -- | Any extra fields are Gopher+ fields and not a part of the original
    -- Gopher Protocol specification.
    ,glGopherPlus :: String
    }

-- | The way a GopherLine is displayed (string) after being parsed, namely used by the UI
instance Show GopherLine where
    show x = (indent x) ++ (glDisplayString x)
        where
        indent x = if (glType x) == (Right InformationalMessage) then "           " else "      "

{-|
Take the character from a menu line delivered from a Gopher server and give
back the type of the item that line describes.

"One-character code indicates what kind of content the client should expect.
This code may be either a digit or a letter of the alphabet; letters are
case-sensitive."
-}
charToItemType :: Char -> Either GopherCanonicalItemType GopherNonCanonicalItemType
-- Left (canonical)
charToItemType '0' = Left File
charToItemType '1' = Left Directory
charToItemType '2' = Left CsoPhoneBookServer
charToItemType '3' = Left Error
charToItemType '4' = Left BinHexedMacintoshFile
charToItemType '5' = Left DosBinaryArchive
charToItemType '6' = Left UnixUuencodedFile
charToItemType '7' = Left IndexSearchServer
charToItemType '8' = Left TextBasedTelnetSession
charToItemType '9' = Left BinaryFile
charToItemType '+' = Left RedundantServer
charToItemType 'T' = Left Tn3270Session
charToItemType 'g' = Left GifFile
charToItemType 'I' = Left ImageFile
-- Right (noncanonical)
charToItemType 'd' = Right Doc
charToItemType 'h' = Right HtmlFile
charToItemType 'i' = Right InformationalMessage
charToItemType 's' = Right SoundFile

-- Fixme: for these three... rename to explain*ItemType?
-- FIXME: update with the description from one of those docstrings...
explainCanonicalType :: GopherCanonicalItemType -> String
explainCanonicalType item = case item of
     File -> "Item is a file. Plaintext file."
     Directory -> "Item is a directory. Gopher submenu."
     CsoPhoneBookServer -> "Item is a CSO phone-book server. CCSO Nameserver."
     Error -> "Error. Error code returned by a Gopher server to indicate failure."
     BinHexedMacintoshFile -> "Item is a BinHexed Macintosh file. BinHex-encoded file (primarily for Macintosh computers)."
     DosBinaryArchive -> "Item is DOS binary archive of some sort. Client must read until the TCP connection closes. Beware. DOS file."
     UnixUuencodedFile -> "Item is a UNIX uuencoded file. uuencoded file."
     IndexSearchServer -> "Item is an Index-Search server. Gopher full-text search."
     TextBasedTelnetSession -> "Item points to a text-based telnet session. Telnet."
     BinaryFile -> "Item is a binary file! Client must read until the TCP connection closes. Beware. Binary file."
     RedundantServer -> "Item is a redundant server. Mirror or alternate server (for load balancing or in case of primary server downtime)."
     Tn3270Session -> "Item points to a text-based tn3270 session. Telnet 3270."
     GifFile -> "Item is a GIF format graphics file. GIF file."
     ImageFile -> "Item is some kind of image file.  Client decides how to display. Image file."

explainNonCanonicalType :: GopherNonCanonicalItemType -> String
explainNonCanonicalType item = case item of
     Doc -> "Doc. Seen used alongside PDF's and .DOC's."
     HtmlFile -> "HTML file."
     InformationalMessage -> "Informational message."
     SoundFile -> "Sound file (especially the WAV format)."

-- | Explain a Gopher line/menu item type, either canonical (RFC 1436) or non canonical.
explainType :: Either GopherCanonicalItemType GopherNonCanonicalItemType -> String
explainType itemType = case itemType of
    Left item -> explainCanonicalType item
    Right item -> explainNonCanonicalType item

-- | Canonical item types are represented by their explaination in explainCanonicalType.
instance Show GopherCanonicalItemType where
    show = explainCanonicalType

-- | Non-canonical item types are represented by their explaination in
-- explainNonCanonicalType.
instance Show GopherNonCanonicalItemType where
    show = explainNonCanonicalType

-- | Take a big string (series of lines) returned from a Gopher request and
-- parse it into a Gopher menu (a series of GopherLines)!
makeGopherLines :: String -> GopherMenu
makeGopherLines rawString = GopherMenu $ map makeGopherLine numberedLines
    where
    numberedLines = zip [0..] (rmTerminatorPeriod . lines $ rawString)
    -- A period on a line by itself denotes the end.
    rmTerminatorPeriod l = if last l == ".\r" then init l else l
    splitFields = splitOn "\t"
    parseLink = error "must implement" -- "link" is wrong
    makeGopherLine l = GopherLine {glType=itemType
                                  ,glLineNumber=lineNumber
                                  ,glDisplayString=fields !! 0
                                  ,glSelector=fields !! 1
                                  ,glHost=fields !! 2
                                  ,glPort=read $ fields !! 3
                                  ,glGopherPlus=""--fields !! 4
                                  }
        where
        lineNumber = fst l
        line = snd l
        itemType = charToItemType $ head line
        fields = splitFields . tail $ line

-- | As you can see, a GopherMenu is simply an ordered sequence of
-- GopherLines.
data GopherMenu = GopherMenu [GopherLine]

-- | Easily represent a GopherMenu as a string, formatted as it might be rendered.
instance Show GopherMenu where
    show (GopherMenu lines) = unlines $ map show lines

-- | Gopher protocol TCP/IP request. Leave "resource" as an empty/blank string
-- if you don't wish to specify.
gopherGet :: String -> String -> String -> IO String
gopherGet host port resource =
    connect host port $ \(connectionSocket, remoteAddr) -> do
      send connectionSocket (B8.pack $ resource ++ "\r\n")
      -- need to only fetch as many bytes as it takes to get period on a line by itself to
      -- close the connection.
      wow <- getAllBytes (pure B8.empty) connectionSocket
      pure $ B8.unpack wow
    where
    recvChunks = 1024
    getAllBytes :: IO B8.ByteString -> Socket -> IO B8.ByteString
    getAllBytes acc connectionSocket = do
        gosh <- recv connectionSocket recvChunks
        wacc <- acc
        pure $ fromJust gosh
        if gosh == Nothing then
            acc
        else if B8.null wacc then
            getAllBytes (pure $ fromJust gosh) connectionSocket
        else
            getAllBytes (pure $ B8.append wacc (fromJust gosh)) connectionSocket
