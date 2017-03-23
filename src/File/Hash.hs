{-# OPTIONS_GHC -Wall #-}
module File.Hash
  ( State
  , starter
  , toString
  , put
  , append
  )
  where


import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder.Extra as BS (defaultChunkSize)
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


put :: IO.Handle -> State -> BS.ByteString -> IO State
put handle (State len decoder) chunk =
  do  BS.hPut handle chunk
      return $ State (len + BS.length chunk) (Binary.pushChunk decoder chunk)



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
        else appendHelp sink src =<< put sink state chunk
