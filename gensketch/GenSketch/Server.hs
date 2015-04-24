{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BC

import Control.Applicative ( (<|>) )
import Control.Monad.IO.Class

import Data.List (intercalate)
import Data.Tree

import Snap.Core
import Snap.Extras.CoreUtils
import Snap.Http.Server
import Snap.Util.FileServe

import System.IO
import System.IO.Temp
import System.Process

import Text.JSON.Generic

import GenSketch.Data.SketchFileFormat.Format
import GenSketch.GenFormat
import GenSketch.GenSketch

write :: (Show a, MonadSnap m) => a -> m ()
write = writeBS . BC.pack . show

writeFormats :: (MonadSnap m) => m ()
writeFormats = do
  jsonResponse
  writeBS $ BC.pack $ encodeJSON formatHistories

pathToHistory :: Forest Format -> [Format] -> [Int] -> [Format]
pathToHistory _      prevs [] = prevs
pathToHistory forest prevs (i:is) = pathToHistory branch (prevs ++ [cur]) is
  where
    tree = forest !! i
    branch = subForest tree
    cur = rootLabel tree

sketchPrefixLines = 3
sketchSuffixLines = 1

chopSketchExtraOutput :: String -> String
chopSketchExtraOutput str = intercalate "\n" reducedLines
  where
    strLines = lines str
    choppedLines = drop sketchPrefixLines strLines
    reducedLines = take (length choppedLines - sketchSuffixLines) choppedLines

sketch :: [Format] -> IO String
sketch hist = withSystemTempFile "sketch" $ \tmp h -> do
    hPutStr h $ sketchFormats hist
    hClose h
    javaCode <- readProcess "./scripts/runsketch.sh" [tmp] ""
    return $ chopSketchExtraOutput javaCode

sketchFormat :: (MonadSnap m) => m ()
sketchFormat = do
  pathStrOpt <- getParam "sketchpath"

  case pathStrOpt of
    Nothing -> return ()
    Just pathStr -> do let path = read (BC.unpack pathStr) :: [Int]
                       let history = pathToHistory formatHistories [] path
                       result <- liftIO $ sketch history
                       write result

routes :: (MonadSnap m) => m ()
routes = dir "static" (serveDirectory "../visualization")
     <|> route [ ("getsketches/", writeFormats) 
               , ("sketch/:sketchpath", sketchFormat)
               ]

main :: IO ()
main = do
  quickHttpServe routes