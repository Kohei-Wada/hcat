{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HCat where

import qualified Control.Exception as Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.Functor ((<&>))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as PoxixClock
import Data.Time.Format as TimeFormat
import qualified System.Directory as Directory
import qualified System.Environment as Env
import System.IO
import qualified System.IO.Error as IOError
import qualified System.Info as SystemInfo
import System.Process (readProcess)
import Terminal
import Text.Printf (printf)

data FileInfo = FileInfo
  { filePath :: FilePath,
    fileSize :: Int,
    fileMTime :: Clock.UTCTime,
    fileReadable :: Bool,
    fileWritable :: Bool,
    fileExecutable :: Bool
  }
  deriving (Show)

data ScrollCancel = Up | Down | CancelScroll deriving (Show)

runHCat :: IO ()
runHCat = withErrorHandling $ do
  -- Get the list of files from the command line arguments
  fs <- do
    args <- Env.getArgs
    eitherToError $ handleArgs args

  -- Read the contents of the files
  cs <- forM fs $ \f -> do
    h <- openFile f ReadMode
    TextIO.hGetContents h

  -- Get the file information
  is <- forM fs fileInfo
  -- Get the terminal size
  terminalSize <- getTerminalSize

  -- Paginate the contents of the files
  let pss = zipWith (paginate terminalSize) is cs

  -- Concatenate the paginated contents and display them
  showPages (concat pss)
  where
    handleError :: IOError.IOError -> IO ()
    handleError e = putStrLn $ "Error: " ++ show e

    withErrorHandling :: IO () -> IO ()
    withErrorHandling action = action `Exception.catch` handleError

    eitherToError :: (Show a) => Either a b -> IO b
    eitherToError (Left err) = Exception.throwIO $ IOError.userError $ show err
    eitherToError (Right x) = return x

handleArgs :: [String] -> Either String [FilePath]
handleArgs args = case args of
  [] -> Left "No files specified"
  xs -> Right xs

wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap lineLength lineText
  | Text.length lineText <= lineLength = [lineText]
  | otherwise =
      let (candidate, nextLines) = Text.splitAt lineLength lineText
          (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
       in firstLine : wordWrap lineLength (overflow <> nextLines)
  where
    softWrap hardWrappedText textIndex
      | textIndex <= 0 = (hardWrappedText, Text.empty)
      | Text.index hardWrappedText textIndex == ' ' =
          let (wrappedLine, rest) = Text.splitAt textIndex hardWrappedText
           in (wrappedLine, Text.tail rest)
      | otherwise = softWrap hardWrappedText (textIndex - 1)

groupsOf :: Int -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n xs =
  let (hd, tl) = splitAt n xs
   in hd : groupsOf n tl

paginate :: ScreenDimensions -> FileInfo -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) finfo text =
  let rows' = rows - 1
      wrappedLines = concatMap (wordWrap cols) (Text.lines text)
      pages = map (Text.unlines . padTo rows') (groupsOf rows' wrappedLines)
      pageCount = length pages
      statusLines = map (formatFileInfo finfo cols pageCount) [1 .. pageCount]
   in zipWith (<>) pages statusLines
  where
    padTo :: Int -> [Text.Text] -> [Text.Text]
    padTo lineCount rowsToPad =
      take lineCount $ rowsToPad <> repeat ""

getTerminalSize :: IO ScreenDimensions
getTerminalSize = case SystemInfo.os of
  "linux" -> getWindowSize
  "darwin" -> getWindowSize
  _other -> return $ ScreenDimensions 24 80

getContinue :: IO ScrollCancel
getContinue = do
  putStrLn "Press Enter to continue, or q to quit"
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  input <- hGetChar stdin
  case input of
    'k' -> return Up
    'j' -> return Down
    'q' -> return CancelScroll
    _ -> getContinue

showPages :: [Text.Text] -> IO ()
showPages [] = return ()
showPages (page : pages) = showPagesHelper (page : pages) []

showPagesHelper :: [Text.Text] -> [Text.Text] -> IO ()
showPagesHelper [] _ = return () -- all pages shown
showPagesHelper (page : pages) prevPages = do
  clearScreen
  TextIO.putStrLn page
  continue <- getContinue
  case continue of
    Down -> showPagesHelper pages (page : prevPages)
    Up -> case prevPages of
      [] -> showPagesHelper (page : pages) []
      (p : ps) -> showPagesHelper (p : page : pages) ps
    CancelScroll -> return ()

clearScreen :: IO ()
clearScreen = BS.putStr "\^[[1J\^[[1;1H"

fileInfo :: FilePath -> IO FileInfo
fileInfo path = do
  permissions <- Directory.getPermissions path
  size <- BS.readFile path <&> BS.length
  mtime <- Directory.getModificationTime path
  return $
    FileInfo
      { filePath = path,
        fileSize = size,
        fileMTime = mtime,
        fileReadable = Directory.readable permissions,
        fileWritable = Directory.writable permissions,
        fileExecutable = Directory.executable permissions
      }

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text.Text
formatFileInfo FileInfo {..} maxWidth totalPages currentPage =
  let fileName = filePath
      timestamp = TimeFormat.formatTime TimeFormat.defaultTimeLocale "%Y-%m-%d %H:%M:%S" fileMTime
      permissionString =
        [ if fileReadable then 'r' else '-',
          if fileWritable then 'w' else '-',
          if fileExecutable then 'x' else '-'
        ]
      statusLine =
        Text.pack $
          printf
            "%s | permissions: %s | %d bytes | modified %s | page %d of %d"
            fileName
            permissionString
            fileSize
            timestamp
            currentPage
            totalPages
   in invertText $ truncateStatus statusLine
  where
    invertText input =
      let reverseVideo = "\^[[7m"
          resetVideo = "\^[[0m"
       in reverseVideo <> input <> resetVideo

    truncateStatus statusLine
      | maxWidth <= 3 = ""
      | Text.length statusLine > maxWidth = Text.take (maxWidth - 3) statusLine <> "..."
      | otherwise = statusLine
