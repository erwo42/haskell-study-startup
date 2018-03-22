-- Written as part of the "Coding Dojo: Containers" by Simon Fromme, 2018

module Set where

import           Data.List (intercalate, sort)
import           Data.Set  (Set, empty, fromList, intersection, singleton,
                            toList, union, unions, (\\))


type Database = (String, Set String)
type Class = (Set String, Set String)

parseFileContent :: String -> [Database]
parseFileContent = map parseLine . sort . lines

parseLine :: String -> Database
parseLine s = let xs = words s in (head xs, (fromList . sort . tail) xs)

classToStr :: Class -> String
classToStr (names, attributes) = "CL " ++ nameStr ++ ": " ++ attrStr where
  nameStr = intercalate "_"  (toList names)
  attrStr = intercalate ", " (toList attributes)

databaseToStr :: Database -> String
databaseToStr (name, columns) = "TB " ++ name ++ ": " ++ columnStr where
  columnStr = intercalate ", " (toList columns)

classHierarchy :: [Database] -> [Class]
classHierarchy dbs = go leafClasses 0 [] where
  leafClasses = map (\(s, elems) -> (singleton s, elems)) dbs
  go cls n akk
    | n == length leafClasses = akk
    | otherwise  = go (unique cls') (n + 1) (akk' ++ akk) where
        akk'   = if n == length leafClasses - 1
                 then cls
                 else [(e, n \\ others) | (e, n) <- cls,
                       let others = unions [n' | (e', n') <- cls, e' /= e]]
        cls'   = [(union n1 n2, intersection e1 e2) | (n1, e1) <- cls, (n2, e2) <- cls, n1 < n2]
        unique = toList . fromList

compileTable :: String -> IO ()
compileTable filename = do
  input <- readFile filename
  putStrLn $ "Test: " ++ filename
  let dbs   = parseFileContent input
      out f = putStrLn . intercalate "\n" . map f in do
    out databaseToStr dbs
    out classToStr (classHierarchy dbs)

main :: IO ()
main = compileTable "/tmp/in"
