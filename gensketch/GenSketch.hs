{-# LANGUAGE OverloadedStrings #-}

import Data.SketchFileFormat.Format
import Text.PrettyPrint

main :: IO ()
main = do
  x <- fmap lines getContents
  putStrLn $ render $ formatsToSketch $ stringToFormats $ head x

formatsToSketch :: [Format] -> Doc
formatsToSketch formats =
  let zipformats = zip [1..] formats in
  vcat [ formatsToStructs zipformats
       , fileSizeDecl formats ]

-- TODO: Intermediate format that's an AST, with methods for rendering
-- each sort of thing


-- Struct declarations

formatsToStructs :: [(Integer, Format)] -> Doc
formatsToStructs formats =
  vcat $ map formatToStruct formats

formatToStruct :: (Integer, Format) -> Doc
formatToStruct (index, format) =
  block
    (text "struct" <+> text "DataStruct_" <> integer index)
    (vcat $ map (fieldToStructDecl . fst) format)

fieldToStructDecl :: Field -> Doc
fieldToStructDecl (IntField fieldName) =
  text "int" <+> text "field" <> int fieldName <> semi


-- File size declaration

fileSizeDecl :: [Format] -> Doc
fileSizeDecl formats =
  text "int" <+> text "fileSize" <+> equals <+>
  integer (fileSize formats) <> semi --TODO

fileSize :: [Format] -> Integer
fileSize formats =
    maximum $ map (sum . (map fieldSize)) formats

fieldSize :: (Field, RenderStrategy) -> Integer
fieldSize (field, AttributeCode code) = 1 + fieldSize (field, code)
fieldSize (IntField _, Raw) = 2


-- Writer functions

-- Checker functions

-- Total cost declaration

-- Generator bit check()

-- Generator int readExp()

-- Read function

-- Harness void main()





-- Helper functions

block :: Doc -> Doc -> Doc
block header body = vcat [ header <+> char '{'
                         , nest 4 body
                         , char '}' ]

stringToFormats :: String -> [Format]
stringToFormats = read

