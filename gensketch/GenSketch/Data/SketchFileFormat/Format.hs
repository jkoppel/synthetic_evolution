{-# LANGUAGE DeriveDataTypeable #-}

module GenSketch.Data.SketchFileFormat.Format where

import Data.Data (Data, Typeable)

import Text.Printf

--------------------------------------------------------------------------------

type FieldName = Int
data Field = IntField FieldName
  deriving (Eq, Ord, Read, Show, Data, Typeable)

data RenderStrategy = AttributeCode RenderStrategy | Raw
  deriving (Eq, Ord, Read, Show, Data, Typeable)

type Format = [(Field, RenderStrategy)]

type FormatChangeDesc = String
data ChangedFormat = ChangedFormat { changeDesc :: String
                                   , newFormat  :: Format
                                   }
  deriving (Eq, Ord, Read, Show, Data, Typeable)

prettyFormatField :: Format -> Int -> String
prettyFormatField f i = prettyField $ fst $ f !! i

prettyField :: Field -> String
prettyField (IntField n) = printf "field%d" n