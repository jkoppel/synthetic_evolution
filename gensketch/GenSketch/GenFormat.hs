-- While presently this is being called from a server, going forward the server will be replaced with purely static content;

module GenSketch.GenFormat (
    formatHistories
  ) where

import Data.Tree

import GenSketch.Data.SketchFileFormat.Format

addField :: [Format] -> Format -> [Format]
addField prevFormats format = do
    let maxFieldHistorical = foldl max 0 (map maxField prevFormats)
    strat <- [AttributeCode Raw, Raw]
    inserts [] (IntField (maxFieldHistorical+1), strat) format
    
  where
    maxField :: Format -> FieldName
    maxField = foldl max 0 . map (fieldName.fst)

    fieldName :: Field -> FieldName
    fieldName (IntField n) = n

    inserts :: [a] -> a -> [a] -> [[a]]
    inserts before x [] = [before ++ [x]]
    inserts before x (r:rest) = [before ++ [x] ++ (r:rest)] ++ inserts (before++[r]) x rest

removeField :: a -> Format -> [Format]
removeField _ format = do
    pos <- [0..(length format - 1)]
    return $ take pos format ++ drop (pos+1) format

data FormatState = FormatState {
    height  :: Int
  , cur     :: Format
  , history :: [Format]
  }

nextLayerFormatHistories :: FormatState -> (Format, [FormatState])
nextLayerFormatHistories st
    | height st == 0 = (cur st, [])
    | otherwise      = (cur st, nextLayerStates)
       where
         nextLayerStates = do
           nextFn <- [addField, removeField]
           let prevs = history st
           next   <- nextFn prevs (cur st)
           return $ FormatState {
                      height  = (height st) - 1
                    , cur     = next
                    , history = prevs ++ [next]
                    }
  
formatHistories :: Forest Format
formatHistories = subForest $ unfoldTree nextLayerFormatHistories startState
  where
    startState = FormatState {
                   height  = 3
                 , cur     = []
                 , history = [[]]
                 }
