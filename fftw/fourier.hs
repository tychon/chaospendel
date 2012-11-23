
-- cabal install vector
-- cabal install fft

import System.IO
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Array as A
import qualified Data.Array.CArray as CA
import Math.FFT
import Data.Complex

main = do
  let xs = [0,0.0001..(8*pi)] :: [Double]
      n = length xs
      --xs' = map (\x -> (sin (x))+(cos (x))+4*(cos(3*x))+(sin (5*x))) xs
      xs' = map (\x -> 4*(sin (x))+(sin (8*x))+(sin (20*x))) xs
  putStrLn $ unlines $ map (show) xs'
  let (xsptr, dataoffset, datalength) = VS.unsafeToForeignPtr $ VS.fromList xs'
  xsarray <- CA.unsafeForeignPtrToCArray xsptr ((1::Int), n)
  let ysarray = dftRC xsarray
  hPutStrLn stderr $ formatCSV $ map (/ (fromIntegral n)) $ CA.elems ysarray

formatCSV xs = unlines $ map (\x@(xreal :+ ximag) -> (show xreal)++","++(show ximag)) xs
