module File.IO
  ( writeBinary, readBinary
  , writeUtf8, readUtf8
  , remove, exists
  , removeDir
  , andM
  )
  where

import Control.Monad.Except (liftIO)
import qualified Data.Binary as Binary
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import GHC.IO.Exception ( IOErrorType(InvalidArgument) )
import qualified System.Directory as Dir
import System.FilePath (dropFileName)
import System.IO (utf8, hSetEncoding, withFile, Handle, IOMode(ReadMode, WriteMode))
import System.IO.Error (ioeGetErrorType, annotateIOError, modifyIOError)

import qualified Reporting.Error as Error
import qualified Reporting.Error.Assets as AError
import qualified Reporting.Task as Task



-- BINARY


writeBinary :: (Binary.Binary a) => FilePath -> a -> Task.Task ()
writeBinary path value =
  liftIO $
    do  let dir = dropFileName path
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
  withUtf8 filePath WriteMode $ \handle ->
    TextIO.hPutStr handle text


withUtf8 :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withUtf8 filePath mode callback =
  withFile filePath mode $ \handle ->
    do  hSetEncoding handle utf8
        callback handle



-- READ UTF-8


readUtf8 :: FilePath -> IO Text.Text
readUtf8 filePath =
  withUtf8 filePath ReadMode $ \handle ->
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



-- REMOVE FILES


remove :: FilePath -> Task.Task ()
remove filePath =
  liftIO $
    do  exists <- Dir.doesFileExist filePath
        if exists
          then Dir.removeFile filePath
          else return ()


exists :: FilePath -> Task.Task Bool
exists filePath =
  liftIO $ Dir.doesFileExist filePath


removeDir :: FilePath -> IO ()
removeDir path =
  do  exists <- Dir.doesDirectoryExist path
      if exists
        then Dir.removeDirectoryRecursive path
        else return ()



-- HELPER


andM :: (Monad m) => [m Bool] -> m Bool
andM checks =
  case checks of
    [] ->
      return True

    check : otherChecks ->
      do  bool <- check
          if bool then andM otherChecks else return False
