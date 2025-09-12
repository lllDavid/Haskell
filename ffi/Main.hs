{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types
import Foreign.Marshal.Array (withArray)

foreign import ccall "array_sum" c_array_sum :: Ptr CLLong -> CInt -> IO CLLong

arraySum :: [Int] -> IO Integer
arraySum xs =
  withArray (map (fromIntegral :: Int -> CLLong) xs) $ \ptr -> do
    s <- c_array_sum ptr (fromIntegral $ length xs)
    return (fromIntegral s) 

main :: IO ()
main = do
  let numbers = [1..1000000]
  sumResult <- arraySum numbers
  print sumResult