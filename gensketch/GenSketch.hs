{-# LANGUAGE OverloadedStrings #-}

import Data.SketchFileFormat.Format
import Text.PrettyPrint

-- data Struct = Struct
-- data ReadFunc = ReadFunc

-- class GenSketch t where
--  gen :: t -> [(Integer, Format)] -> Doc

-- instance GenSketch Struct where
--  gen _ formats = vcat $ map formatToStruct formats

main :: IO ()
main = do
  x <- fmap lines getContents
  putStrLn $ render $ formatsToSketch $ stringToFormats $ head x

formatsToSketch :: [Format] -> Doc
formatsToSketch formats =
  let zipformats = zip [1..] formats in
  vcat [ genStructs zipformats
       , genFileSizeDecl formats
       , genWriters zipformats
       , genCheckers zipformats
       , totalCostDeclaration
       , generatorCheck
       , generatorReadExp
       , formatToReader (last zipformats)
       , genHarnessMain zipformats
       ]


-- Struct declarations

genStructs :: [(Integer, Format)] -> Doc
genStructs formats =
  vcat $ map formatToStruct formats

formatToStruct :: (Integer, Format) -> Doc
formatToStruct (index, format) =
  block
    ("struct" <+> dataStructVariable index)
    (vcat $ map (fieldToStructDecl . fst) format)

fieldToStructDecl :: Field -> Doc
fieldToStructDecl (IntField fieldName) =
  "int" <+> "field" <> int fieldName <> semi


-- File size declaration

type FileSizeDeclaration = Doc

genFileSizeDecl :: [Format] -> FileSizeDeclaration
genFileSizeDecl formats =
  "int" <+> "fileSize" <+> equals <+>
  integer (fileSize formats) <> semi

fileSize :: [Format] -> Integer
fileSize formats =
    maximum $ map (sum . (map fieldSize)) formats

fieldSize :: (Field, RenderStrategy) -> Integer
fieldSize (field, AttributeCode code) = 1 + fieldSize (field, code)
fieldSize (IntField _, Raw) = 2


-- Writer functions

genWriters :: [(Integer, Format)] -> Doc
genWriters formats =
  vcat $ map formatToWriter formats


formatToWriter :: (Integer, Format) -> Doc
formatToWriter (index, format) =
  block ("void" <+> "write_" <> integer index
           <> argslist [ dataStructVariable index <+> "s"
                       , "ref int length"
                       , "ref int[fileSize] buf" ])
        (vcat $ genBufDecls 0 format)
  where genBufDecls :: Integer -> [(Field, RenderStrategy)] -> [Doc]
        genBufDecls index ((field, AttributeCode renderStrategy) : tailFormats) =
          ["buf[" <> integer index <> "] =" <+> renderField field <> semi]
          ++ genBufDecls (index+1) ((field, renderStrategy) : tailFormats)
        genBufDecls index ((IntField fieldName, Raw) : tailFormats) =
          ["buf[" <> integer index <> "] = s." <> fieldAttribute (IntField fieldName, Raw) <> semi]
          ++ genBufDecls (index+1) tailFormats
        genBufDecls index [] =
          ["length = " <> integer index <> semi]

renderField :: Field -> Doc
renderField (IntField fieldName) = int fieldName


-- Checker functions

genCheckers :: [(Integer, Format)] -> Doc
genCheckers formats =
  vcat $ map (formatToChecker (last formats)) formats
    where
      formatToChecker :: (Integer, Format) -> (Integer, Format) -> Doc
      formatToChecker (lastIndex, lastFormat) (index, format) =
        block (formatToCheckerHeader lastIndex index)
              (formatToCheckerBody lastFormat format)
      formatToCheckerHeader :: Integer -> Integer -> Doc
      formatToCheckerHeader lastIndex index =
        "void check_" <> integer index <>
                      argslist [ dataStructVariable index <+> "in"
                               , dataStructVariable lastIndex <+> "out"
                               ]
      formatToCheckerBody :: Format -> Format -> Doc
      formatToCheckerBody lastFormat format =
        vcat $ map (fieldToAssert format) lastFormat
      fieldToAssert :: Format -> (Field, RenderStrategy) -> Doc
      fieldToAssert format field =
        -- Test if the format contains this field name belonging to the final format.
        if (field `elem` format)
        then "assert in." <> fieldAttribute field <+> "==" <+>
                            "out." <> fieldAttribute field <> semi
        else "assert out."<> fieldAttribute field <+> "==" <+> "-1" <> semi

-- Total cost declaration

totalCostDeclaration :: Doc
totalCostDeclaration = "int totalCost = 0;"


-- Generator bit check()

generatorCheck :: Doc
generatorCheck =
  "generator bit check(int[fileSize] buf, int length, int depth) {\n    assert depth > 0;\n    \n    int t = ??;\n\n    if (t == 0) {\n        return true;\n    } else if (t == 1) {\n        totalCost += 1;\n        return buf[??] == ??;\n    } else if(t == 2) {\n        totalCost += 1;\n        return {| length (< | == | >) ?? |};\n    } else {\n        bit IUsedAnAnd = check(buf, length, depth-1) && check(buf, length, depth-1);\n        return IUsedAnAnd;\n    }\n}"


-- Generator int readExp()

generatorReadExp :: Doc
generatorReadExp =
  "generator int readExp(int[fileSize] buf, int length, int depth) {\n    if (?? || depth == 0) {\n        return -1;\n    }\n    \n    if (check(buf, length, 2)) {\n        return buf[??];\n    } else {\n        return readExp(buf, length, depth-1);\n    }\n}"


-- Read function -- only need the lastmost format's read function!

formatToReader :: (Integer, Format) -> Doc
formatToReader (index, format) = block readerHeader readerBody
  where readerHeader :: Doc
        readerHeader =
          dataStructVariable index <+> "read" <>
          argslist ["int[fileSize] buf", "int length"]

        readerBody :: Doc
        readerBody =
          dataStructVariable index <+> "result = new" <+> dataStructVariable index <>
          "();" $$ vcat (map readerBodyResult format) $$ "return result;"

readExpDepth :: Doc
readExpDepth = int 3

readerBodyResult :: (Field, RenderStrategy) -> Doc
readerBodyResult (IntField fieldName, _) = "result.field" <> (int fieldName) <+> "= readExp(buf, length, "<> readExpDepth <>");"


-- Harness void main()

genHarnessMain :: [(Integer, Format)] -> Doc
genHarnessMain formats = block genHarnessMainHeader
                               (genHarnessMainBody formats)
  where genHarnessMainHeader :: Doc
        genHarnessMainHeader =
          "harness void main" <> argslist ((argsFromFormats formats)
                                           ++ (bufsFromFormats formats))

argsFromFormats :: [(Integer, Format)] -> [Doc]
argsFromFormats formats =
  concatMap argsFromFormat formats
  where argsFromFormat :: (Integer, Format) -> [Doc]
        argsFromFormat (index, format) =
          map (argFromField index) format

argFromField :: Integer -> (Field, RenderStrategy) -> Doc
argFromField index (IntField fieldName, _) =
  "int" <+> "field" <> int fieldName <> "_" <> integer index
  -- Note: This function is used in more than one place!

bufsFromFormats :: [(Integer, Format)] -> [Doc]
bufsFromFormats formats =
  map bufFromFormat formats

bufFromFormat :: (Integer, Format) -> Doc
bufFromFormat (index, _) =
  "int[fileSize]" <+> "buf" <> integer index

genHarnessMainBody :: [(Integer, Format)] -> Doc
genHarnessMainBody formats =
  vcat [ "int length;"
       , genMainStructDecls formats
       , genMainWriteChecks formats
       , "minimize(totalCost);"
       ]

genMainStructDecls :: [(Integer, Format)] -> Doc
genMainStructDecls formats =
  vcat $ map genMainStructDecl formats
-- DataStruct_2 in2 = new DataStruct_2(field1=field1_2, field2=field2_2);

genMainStructDecl :: (Integer, Format) -> Doc
genMainStructDecl (index, format) =
   dataStructVariable index <+> "in" <> integer index
                  <+> equals <+> "new" <+> dataStructVariable index
                  <> argslist (structDeclArgs index format) <> semi

structDeclArgs :: Integer -> Format -> [Doc]
structDeclArgs index format =
  map (structDeclArg index) format

structDeclArg :: Integer -> (Field, RenderStrategy) -> Doc
  -- field1=field1_2. note that '2' is the index
structDeclArg index (IntField fieldName, _) =
  "field" <> int fieldName <> equals <> "field" <> int fieldName <>
  "_" <> integer index

genMainWriteChecks :: [(Integer, Format)] -> Doc
genMainWriteChecks formats =
  vcat $ map genMainWriteCheck formats
--     write_1(in1, length, buf1);
--     check_1(in1, read(buf1, length));

genMainWriteCheck :: (Integer, Format) -> Doc
genMainWriteCheck (index, _) =
  vcat [ "write_" <> integer index <> argslist
                    ["in" <> integer index,"length", "buf" <> integer index] <> semi
       , "check_" <> integer index <> argslist
                    ["in" <> integer index, "read" <> argslist ["buf" <> integer index, "length"]]
                    <> semi
       ]

-- Helper functions

fieldAttribute :: (Field, RenderStrategy) -> Doc
fieldAttribute (IntField fieldName, _) =
  "field" <> int fieldName

dataStructVariable :: Integer -> Doc
dataStructVariable index = "DataStruct_" <> integer index

argslist :: [Doc] -> Doc
argslist args =
  lparen <> (foldl1 (\x y -> x <> comma <+> y) args) <> rparen

block :: Doc -> Doc -> Doc
block header body = vcat [ header <+> char '{'
                         , nest 4 body
                         , char '}' ]

stringToFormats :: String -> [Format]
stringToFormats = read

