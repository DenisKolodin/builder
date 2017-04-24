module File.IO
  ( writeBinary, readBinary
  , writeUtf8, readUtf8
  , writeBuilder
  , Writer
  , put
  , putByteString
  , putBuilder
  , putFile
  , exists
  , remove, removeDir
  , find
  , andM
  )
  where

import qualified Control.Monad.Reader as Reader
import Control.Monad.Trans (liftIO)
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS (defaultChunkSize)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import GHC.IO.Exception ( IOErrorType(InvalidArgument) )
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import System.FilePath ((</>))
import qualified System.IO as IO
import System.IO.Error (ioeGetErrorType, annotateIOError, modifyIOError)

import qualified Reporting.Error as Error
import qualified Reporting.Error.Assets as AError
import qualified Reporting.Task as Task



-- BINARY


writeBinary :: (Binary.Binary a) => FilePath -> a -> Task.Task ()
writeBinary path value =
  liftIO $
    do  let dir = FP.dropFileName path
        Dir.createDirectoryIfMissing True dir
        Binary.encodeFile path value


readBinary :: (Binary.Binary a) => FilePath -> Task.Task a
readBinary path =
  do  exists <- liftIO (Dir.doesFileExist path)
      if not exists
        then throwCorruptBinary path
        else
          do  result <- liftIO (Binary.decodeFileOrFail path)
              case result of
                Left _ ->
                  throwCorruptBinary path

                Right value ->
                  return value


throwCorruptBinary :: FilePath -> Task.Task a
throwCorruptBinary filePath =
  Task.throw (Error.Assets (AError.CorruptBinary filePath))



-- WRITE UTF-8


writeUtf8 :: FilePath -> Text.Text -> IO ()
writeUtf8 filePath text =
  withUtf8 filePath IO.WriteMode $ \handle ->
    TextIO.hPutStr handle text


withUtf8 :: FilePath -> IO.IOMode -> (IO.Handle -> IO a) -> IO a
withUtf8 filePath mode callback =
  IO.withFile filePath mode $ \handle ->
    do  IO.hSetEncoding handle IO.utf8
        callback handle



-- READ UTF-8


readUtf8 :: FilePath -> IO Text.Text
readUtf8 filePath =
  withUtf8 filePath IO.ReadMode $ \handle ->
    modifyIOError
      (encodingError filePath)
      (TextIO.hGetContents handle)


encodingError :: FilePath -> IOError -> IOError
encodingError filePath ioError =
  case ioeGetErrorType ioError of
    InvalidArgument ->
      annotateIOError
        (userError "Bad encoding; the file must be valid UTF-8")
        ""
        Nothing
        (Just filePath)

    _ ->
      ioError



-- WRITE BUILDER


writeBuilder :: FilePath -> BS.Builder -> IO ()
writeBuilder path builder =
  IO.withBinaryFile path IO.WriteMode $ \handle ->
    do  IO.hSetBuffering handle (IO.BlockBuffering Nothing)
        BS.hPutBuilder handle builder



-- WRITER


type Writer = Reader.ReaderT IO.Handle IO ()


put :: FilePath -> Writer -> IO ()
put path writer =
  IO.withBinaryFile path IO.WriteMode (Reader.runReaderT writer)


putByteString :: BS.ByteString -> Writer
putByteString chunk =
  do  handle <- Reader.ask
      liftIO $ BS.hPut handle chunk


putBuilder :: BS.Builder -> Writer
putBuilder builder =
  do  handle <- Reader.ask
      liftIO $ mapM_ (BS.hPut handle) $
        LBS.toChunks (BS.toLazyByteString builder)


putFile :: FilePath -> Writer
putFile path =
  do  sink <- Reader.ask
      liftIO $ IO.withBinaryFile path IO.ReadMode $ \source ->
        putHelp source sink


putHelp :: IO.Handle -> IO.Handle -> IO ()
putHelp source sink =
  do  chunk <- BS.hGet source BS.defaultChunkSize
      if BS.null chunk then return () else
        do  BS.hPut sink chunk
            putHelp source sink



-- EXISTS


exists :: FilePath -> Task.Task Bool
exists filePath =
  liftIO $ Dir.doesFileExist filePath



-- REMOVE FILES


remove :: FilePath -> Task.Task ()
remove filePath =
  liftIO $
    do  exists <- Dir.doesFileExist filePath
        if exists
          then Dir.removeFile filePath
          else return ()


removeDir :: FilePath -> IO ()
removeDir path =
  do  exists <- Dir.doesDirectoryExist path
      if exists
        then Dir.removeDirectoryRecursive path
        else return ()



-- FIND FILES


find :: FilePath -> IO (Maybe FilePath)
find name =
  do  subDir <- Dir.getCurrentDirectory
      findHelp name (FP.splitDirectories subDir)


findHelp :: FilePath -> [String] -> IO (Maybe FilePath)
findHelp name dirs =
  if null dirs then
    return Nothing

  else
    do  exists <- Dir.doesFileExist (FP.joinPath dirs </> name)
        if exists
          then return (Just (FP.joinPath dirs))
          else findHelp name (init dirs)



-- HELPER


andM :: (Monad m) => [m Bool] -> m Bool
andM checks =
  case checks of
    [] ->
      return True

    check : otherChecks ->
      do  bool <- check
          if bool then andM otherChecks else return False
