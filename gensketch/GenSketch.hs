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
       , formatsToFileSizeDecl formats
       , formatsToWriters zipformats
       , formatsToCheckers zipformats
       , totalCostDeclaration
       , generatorCheck
       , generatorReadExp
       , formatToReader (last zipformats)
       , formatsToHarnessMain formats 
       ]


-- Struct declarations

type StructDeclaration = Doc

formatsToStructs :: [(Integer, Format)] -> Doc
formatsToStructs formats =
  vcat $ map formatToStruct formats

formatToStruct :: (Integer, Format) -> StructDeclaration
formatToStruct (index, format) =
  block
    (text "struct" <+> text "DataStruct_" <> integer index)
    (vcat $ map (fieldToStructDecl . fst) format)

fieldToStructDecl :: Field -> Doc
fieldToStructDecl (IntField fieldName) =
  text "int" <+> text "field" <> int fieldName <> semi


-- File size declaration

type FileSizeDeclaration = Doc

formatsToFileSizeDecl :: [Format] -> FileSizeDeclaration
formatsToFileSizeDecl formats =
  text "int" <+> text "fileSize" <+> equals <+>
  integer (fileSize formats) <> semi

fileSize :: [Format] -> Integer
fileSize formats =
    maximum $ map (sum . (map fieldSize)) formats

fieldSize :: (Field, RenderStrategy) -> Integer
fieldSize (field, AttributeCode code) = 1 + fieldSize (field, code)
fieldSize (IntField _, Raw) = 2


-- Writer functions

type WriterDeclaration = Doc

formatsToWriters :: [(Integer, Format)] -> Doc
formatsToWriters formats =
  vcat $ map formatToWriter formats

formatToWriter :: (Integer, Format) -> WriterDeclaration
formatToWriter format =
  "todo"

-- void write_i(DataStruct_i s, ref int length, ref int[fileSize] buf) {
    -- buf[0] = 1;
    -- buf[1] = s.field1;
    -- buf[2] = 3;
    -- buf[3] = s.field3;
    -- length = 4;
-- }


-- Checker functions

type CheckerDeclaration = Doc

formatsToCheckers :: [(Integer, Format)] -> Doc
formatsToCheckers formats =
  vcat $ map formatToChecker formats

formatToChecker :: (Integer, Format) -> CheckerDeclaration
formatToChecker format =
  "todo"

-- void check_4(DataStruct_4 in, DataStruct_4 out) {
--     assert in.field1 == out.field1;
--     assert in.field3 == out.field3;
-- }


-- Total cost declaration

totalCostDeclaration :: Doc
totalCostDeclaration = text "int totalCost = 0;"


-- Generator bit check()

generatorCheck :: Doc
generatorCheck =
  "generator bit check() {} //TODO, but it's static content"


-- Generator int readExp()

generatorReadExp :: Doc
generatorReadExp = 
  "generator int readExp() {} //TODO, but it's static content"


-- Read function -- only need the lastmost format's!

-- DataStruct_4 read(int[fileSize] buf, int length) {
--     DataStruct_4 result = new DataStruct_4();
--     result.field1 = readExp(buf, length, 2);
--     result.field3 = readExp(buf, length, 2);    
--     return result;
-- }

formatToReader :: (Integer, Format) -> Doc
formatToReader (index, format) =
  block (readerHeader index)
        (readerBody index format)

readerHeader :: Integer -> Doc
readerHeader index = 
  "DataStruct_" <> integer index <+> "read" <> 
                  (argslist ["int[fileSize] buf", "int length"])

readerBody :: Integer -> Format -> Doc
readerBody index format =
  "DataStruct_" <> integer index <+> "result = new DataStruct_" <> integer index <> "();" $$ vcat (map readerBodyResult format) $$ "return result;"

readerBodyResult :: (Field, RenderStrategy) -> Doc
readerBodyResult (IntField fieldName, _) = "result.field" <> (int fieldName) <+> "= readExp(buf, length, 2);"

-- Harness void main()

formatsToHarnessMain :: [Format] -> Doc
formatsToHarnessMain formats =
  block (formatsToHarnessMainHeader formats)
          (formatsToHarnessMainBody formats)

formatsToHarnessMainHeader :: [Format] -> Doc
formatsToHarnessMainHeader formats =
  "harness void main()"

formatsToHarnessMainBody :: [Format] -> Doc
formatsToHarnessMainBody formats =
  "int length;"



-- harness void main(int field1, int field2, int field3,
--                   int[fileSize] buf1, int[fileSize] buf2, int[fileSize] buf3, int[fileSize] buf4) {
--     int length;
--     DataStruct_1 in1 = new DataStruct_1(field1=field1);
--     DataStruct_2 in2 = new DataStruct_2(field1=field1, field2=field2);
--     DataStruct_3 in3 = new DataStruct_3(field1=field1, field2=field2, field3=field3);
--     DataStruct_4 in4 = new DataStruct_4(field1=field1, field3=field3);
--     write_1(in1, length, buf1);
--     check_1(in1, read(buf1, length));
--     write_2(in2, length, buf2);
--     check_2(in2, read(buf2, length));
--     write_3(in3, length, buf3);
--     check_3(in3, read(buf3, length));
--     write_4(in4, length, buf4);
--     check_4(in4, read(buf4, length));
--     minimize(totalCost);
-- }


-- Helper functions

argslist :: [Doc] -> Doc
argslist args =
  lparen <> (foldl1 (\x y -> x <> comma <+> y) args) <> rparen

block :: Doc -> Doc -> Doc
block header body = vcat [ header <+> char '{'
                         , nest 4 body
                         , char '}' ]

stringToFormats :: String -> [Format]
stringToFormats = read

