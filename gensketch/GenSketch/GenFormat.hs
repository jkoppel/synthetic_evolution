-- While presently this is being called from a server, going forward the server will be replaced with purely static content;

module GenSketch.GenFormat (
    formatHistories
  ) where

import Data.Tree

import Text.Printf

import GenSketch.Data.SketchFileFormat.Format

--------------------------------------------------------------------------------

addField :: [Format] -> Format -> [ChangedFormat]
addField prevFormats format = do
    let maxFieldHistorical = foldl max 0 (map maxField prevFormats)
    strat <- [AttributeCode Raw, Raw]
    let field = (IntField (maxFieldHistorical+1), strat)
    let nam   = prettyField (fst field)
    (pos, fmt) <- zip [0..] $ inserts [] field format
    return $ ChangedFormat { changeDesc = printf "Insert %s at %d" nam (pos :: Int) ++ renderStrat field
                           , newFormat  = fmt
                           }
    
  where
    renderStrat :: (Field,RenderStrategy) -> String
    renderStrat (f@(IntField n), (AttributeCode s)) = (printf " with prefix %d" n) ++ renderStrat (f,s)
    renderStrat (_,              Raw)               = ""

    maxField :: Format -> FieldName
    maxField = foldl max 0 . map (fieldName.fst)

    fieldName :: Field -> FieldName
    fieldName (IntField n) = n

    inserts :: [a] -> a -> [a] -> [[a]]
    inserts before x [] = [before ++ [x]]
    inserts before x (r:rest) = [before ++ [x] ++ (r:rest)] ++ inserts (before++[r]) x rest

removeField :: a -> Format -> [ChangedFormat]
removeField _ format = do
    pos <- [0..(length format - 1)]
    return $ ChangedFormat { changeDesc = printf "Remove %s" (prettyFormatField format pos)
                           , newFormat = take pos format ++ drop (pos+1) format
                           }

data FormatState = FormatState {
    height  :: Int
  , cur     :: ChangedFormat
  , history :: [Format]
  }

nextLayerFormatHistories :: FormatState -> (ChangedFormat, [FormatState])
nextLayerFormatHistories st
    | height st == 0 = (cur st, [])
    | otherwise      = (cur st, nextLayerStates)
       where
         nextLayerStates = do
           nextFn <- [addField, removeField]
           let prevs = history st
           next   <- nextFn prevs (newFormat $ cur st)
           return $ FormatState {
                      height  = (height st) - 1
                    , cur     = next
                    , history = prevs ++ [newFormat next]
                    }
  
formatHistories :: Forest ChangedFormat
formatHistories = subForest $ unfoldTree nextLayerFormatHistories startState
  where
    startState = FormatState {
                   height  = 4
                 , cur     = ChangedFormat {changeDesc="", newFormat=[]}
                 , history = [[]]
                 }
