{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import qualified Data.ByteString.Char8 as BS.Char8
import System.Environment
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import qualified Data.List as List
-- import Debug.Trace

data Table = Table { table_name :: ClassName
                   , table_columns :: HashSet Field
                   } deriving (Eq, Show)

data Schema = Schema [Table] deriving (Eq, Show)

newtype Field = F { fromFieldName :: BS.Char8.ByteString } deriving (Eq, Show, Hashable, Ord)
newtype ClassName = C { fromClassName :: BS.Char8.ByteString } deriving (Eq, Show, Hashable, Ord)

toTable :: BS.Char8.ByteString -> Table
toTable line = let f = BS.Char8.words line
               in Table (C . head $ f) (HashSet.fromList (map F $ tail f))

loadSchemaFromFile :: String -> Int -> IO Schema
loadSchemaFromFile fname num = do
  content <- BS.Char8.readFile fname
  return $ Schema $ fmap toTable $ take num $ BS.Char8.lines content

addTableToIntermediateResult :: HashMap Field (HashSet ClassName) -> Table -> HashMap Field (HashSet ClassName)
addTableToIntermediateResult fcs (Table cla fields) = foldl' (\h f -> HashMap.alter (insert cla) f h) fcs fields
  where
    insert f v = Just $ case  v of
      Nothing -> HashSet.singleton f
      Just set -> HashSet.insert f set

showClassNames :: HashSet ClassName -> String
showClassNames s = BS.Char8.unpack . BS.Char8.intercalate "_" . List.sort . fmap fromClassName . HashSet.toList $ s

showFields :: HashSet Field -> String
showFields s = BS.Char8.unpack . BS.Char8.intercalate " " . List.sort . fmap fromFieldName . HashSet.toList $ s

doIt :: String -> Int -> IO ()
doIt fname num = do
  (Schema ts) <- loadSchemaFromFile fname num
  let intermediateResult = foldl' addTableToIntermediateResult HashMap.empty  ts
  let classList = HashMap.foldlWithKey' (\h k v -> HashMap.alter (insert k) v h) HashMap.empty intermediateResult
  sequence_ $ HashMap.foldlWithKey' (\l k v -> (putStrLn $ showClassNames k ++ ": " ++ showFields v) : l) [] classList
 where
  insert k v = Just $ case v of
    Nothing -> HashSet.singleton k
    Just set -> HashSet.insert k set


main :: IO ()
main = do
  args <- getArgs
  case args of
    [x, n] -> doIt x (read n)
    _   -> putStrLn "Usage: mySchema2Class <inputfilename> <num>"
