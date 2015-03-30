module Data.SketchFileFormat.Format where

type FieldName = Int
data Field = IntField FieldName
  deriving (Eq, Ord, Read, Show)

data RenderStrategy = AttributeCode RenderStrategy | Raw
  deriving (Eq, Ord, Read, Show)

type Format = [(Field, RenderStrategy)]