module PrettyPrint where

class PrettyPrint p where
  prettyPrint :: p -> String
