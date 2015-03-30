import Data.SketchFileFormat.Format

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

generateNextFormat :: [Format] -> Format -> [Format]
generateNextFormat prevFormats format = do
    fn <- [addField, removeField]
    fn prevFormats format

generateFormatHistories :: [Format] -> Int -> [[Format]]
generateFormatHistories prev 0 = return prev
generateFormatHistories prev n = do
  next <- generateNextFormat prev (last prev)
  generateFormatHistories (prev ++ [next]) (n-1)

main :: IO ()
main = mapM_ putStrLn $ map show $ generateFormatHistories [[]] 3