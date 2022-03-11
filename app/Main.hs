{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use guards" #-}

import Control.Arrow (Arrow (second))
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (State, StateT, evalStateT, execState, get, modify, put, runState)
import Control.Monad.Trans.Writer (WriterT, execWriterT, tell)
import Debug.Trace
import GHC.Base (Float)
import GHC.Num (Num (fromInteger))
import GHC.Real (fromIntegral)
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import System.Directory
import System.IO.Error hiding (catch)
import System.IO.Unsafe
import System.Random
import Prelude hiding (catch)

main :: IO ()
main = do
  deleteFileIfExis "log.txt"
  putStrLn "Enter Start Velocity: "
  velInput <- getLine
  putStrLn "Enter Max Velocity: "
  maxVelInput <- getLine
  putStrLn "Enter Size Frame (width): "
  widthInput <- getLine
  putStrLn "Enter Size Frame (height): "
  heightInput <- getLine
  putStrLn "Enter added speed when the ball bounces: "
  timeInput <- getLine

  let width = (read widthInput :: Int)
      height = (read heightInput :: Int)
      randWidth = fromIntegral (unsafePerformIO (randomN 1 (width - 20))) :: Float
      randHeight = fromIntegral (unsafePerformIO (randomN 1 (height - 20))) :: Float
      maxCepat = (read maxVelInput :: Float) + 7
   in simulate
        (InWindow "Emurgo Indonesia Project Bouncing Ball" (width, height) (50, 50))
        white
        30
        Modelnya {posision = (randWidth, randHeight), angle = ((read velInput :: Float) + 7, 3)}
        (toPict width height)
        (iteration width height (read timeInput :: Float) maxCepat)

deleteFileIfExis :: FilePath -> IO ()
deleteFileIfExis fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

toPict :: Int -> Int -> Modelnya -> Picture
toPict a b Modelnya {posision = (px, py)} =
  translate (-(fromIntegral a * 0.5)) (-(fromIntegral b * 0.5)) $
    translate px py $ circleSolid 20

data Modelnya = Modelnya {posision :: Point, angle :: Point}

randomN :: Int -> Int -> IO Int
randomN strt end = getStdRandom (randomR (strt, fromIntegral end))

iteration :: Int -> Int -> Float -> Float -> ViewPort -> Float -> Modelnya -> Modelnya
iteration a b tambah akhir _ _ Modelnya {posision = (px, py), angle = (vx, vy)} =
  let vx' =
        if (px + vx + 20) >= fromIntegral a
          then
            if vx <= akhir
              then unsafePerformIO $ do
                cetak px py

                return ((-vx) * ((10 + tambah) * 0.1))
              else unsafePerformIO $ do
                cetak px py
                return (-vx)
          else
            if (px + vx - 20) <= 0
              then
                if vx >= (-akhir)
                  then unsafePerformIO $ do
                    cetak px py
                    return ((-vx) * ((10 + tambah) * 0.1))
                  else unsafePerformIO $ do
                    cetak px py
                    return (-vx)
              else vx

      vy' =
        if (py + vy + 20) >= fromIntegral b
          then
            if vx >= (-akhir) && vx <= akhir
              then unsafePerformIO $ do
                cetak px py
                return ((-vy) * ((10 + tambah) * 0.1))
              else unsafePerformIO $ do
                cetak px py
                return (-vy)
          else
            if (py + vy - 20) <= 0
              then
                if vx >= (-akhir) && vx <= akhir
                  then unsafePerformIO $ do
                    cetak px py
                    return ((-vy) * ((10 + tambah) * 0.1))
                  else unsafePerformIO $ do
                    cetak px py

                    return (-vy)
              else
                if (px + vx + 20) >= fromIntegral a
                  then
                    if vx <= akhir
                      then vy * ((10 + tambah) * 0.1) -- +(-10)
                      else vy
                  else
                    if (px + vx - 20) <= 0
                      then
                        if vx >= (-akhir)
                          then vy * ((10 + tambah) * 0.1)
                          else vy
                      else vy
   in Modelnya {posision = (px + vx, py + vy), angle = (vx', vy')}

newtype Stack a = Stack {unStack :: StateT Int (WriterT [Float] IO) a}

toStackSave :: Float -> Float -> Stack ()
toStackSave x y = Stack $ do
  lift $ tell [x]
  lift $ tell [y]
  return ()

exsekusi :: Stack a -> IO [Float]
exsekusi m = execWriterT (evalStateT (unStack m) 0)

cetak :: Float -> Float -> IO ()
cetak b c = do
  x <- exsekusi (toStackSave b c)
  print x
  appendFile "log.txt" (show x ++ "\n")
