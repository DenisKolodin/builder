{-# OPTIONS_GHC -Wall #-}
module File.Hash
  ( State
  , starter
  , toString
  , putByteString
  , putBuilder
  , append
  )
  where


import Control.Monad (foldM)
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS (defaultChunkSize)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Digest.Pure.SHA as SHA
import qualified System.IO as IO



-- HASH STATE


data State =
  State
    { _length :: !Int
    , _decoder :: !(Binary.Decoder SHA.SHA1State)
    }


starter :: State
starter =
  State 0 SHA.sha1Incremental


toString :: State -> String
toString (State len decoder) =
  SHA.showDigest $ SHA.completeSha1Incremental decoder len



-- PUT


putByteString :: IO.Handle -> State -> BS.ByteString -> IO State
putByteString handle (State len decoder) chunk =
  do  BS.hPut handle chunk
      return $ State (len + BS.length chunk) (Binary.pushChunk decoder chunk)


putBuilder :: IO.Handle -> State -> BS.Builder -> IO State
putBuilder handle state builder =
  foldM (putByteString handle) state $
    LBS.toChunks (BS.toLazyByteString builder)


-- APPEND


append :: IO.Handle -> FilePath -> State -> IO State
append sink path state =
  IO.withBinaryFile path IO.ReadMode $ \src ->
    appendHelp sink src state


appendHelp :: IO.Handle -> IO.Handle -> State -> IO State
appendHelp sink src state =
  do  chunk <- BS.hGet src BS.defaultChunkSize
      if BS.null chunk
        then return state
        else appendHelp sink src =<< putByteString sink state chunk
