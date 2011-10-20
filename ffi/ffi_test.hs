{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign.C


foreign import ccall "hilbert_value.c xy2d" c_hilbert_value :: CInt -> CInt -> CInt -> CInt 
findHilbertValue :: Int -> Int -> Int -> Int
findHilbertValue n x y = fromIntegral $ c_hilbert_value (fromIntegral n) (fromIntegral x) (fromIntegral y)

testHilbertValue :: String -> String
testHilbertValue _ = show $ findHilbertValue 4 2 1

main = interact testHilbertValue
