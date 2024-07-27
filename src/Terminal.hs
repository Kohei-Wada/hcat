{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module Terminal (getWindowSize, ScreenDimensions (..)) where

import Data.Word (Word16)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt

data WindowSize = WindowSize
  { ws_row :: Word16,
    ws_col :: Word16
  }
  deriving (Show)

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int,
    screenColumns :: Int
  }
  deriving (Show)

tIOCGWINSZ :: CInt
tIOCGWINSZ = 0x5413

getWindowSize' :: CInt -> IO WindowSize
getWindowSize' fd = allocaBytes 8 $ \p_ws -> do
  throwErrnoIfMinus1_ "getWindowSize" $ c_ioctl fd tIOCGWINSZ p_ws
  row <- peekByteOff p_ws 0
  col <- peekByteOff p_ws 2
  return $ WindowSize row col

getWindowSize :: IO ScreenDimensions
getWindowSize = do
  WindowSize {..} <- getWindowSize' 1
  return $ ScreenDimensions (fromIntegral ws_row) (fromIntegral ws_col)
