module Main where

import Data.List
import Control.Lens
import Streaming
import Control.Monad.Trans.Resource
import qualified Streaming.Prelude as Streaming
import qualified Streaming.ByteString.Char8 as SChar8
import qualified Data.Attoparsec.ByteString.Char8 as AChar8
import qualified Data.Attoparsec.ByteString.Streaming as AStreaming

data Day = Day { _dayCode :: String, _dayContinent :: String, _dayCases :: Double, _dayDeaths :: Double, _dayVaccinations :: Double, _dayPopulation :: String}
  deriving (Eq,Show)

data Grouped = Grouped { _groupedName :: String, _groupedContinent :: String, _groupedTotalCases :: Double, _groupedTotalDeaths :: Double, _groupedTotalVaccinations :: Double, _groupedPopulation :: Double}
  deriving (Eq,Show)

makeLenses ''Day
makeLenses ''Grouped

main = do
  indexes <- getIndexes
  days <- getParsedCsv indexes
  countries <- getGroupedCountries days
  world <- Streaming.fold foldContinent (Grouped "" "" 0.0 0.0 0.0 0.0) id
                                        $ Streaming.each countries
  putStrLn "For Continents"
  Streaming.print
    . Streaming.mapped (Streaming.fold foldContinent (Grouped "" "" 0.0 0.0 0.0 0.0) id)
    . Streaming.groupBy (\x y -> x ^. groupedContinent == y ^. groupedContinent)
    $ Streaming.each (sortBy (\x y -> compare (x ^. groupedContinent) (y ^. groupedContinent)) countries)
  putStrLn "For World"
  putStrLn (show ((Streaming.fst' world) & groupedContinent .~ ""))

getIndexes = do
  (r, _) <- runResourceT . AStreaming.parse (AChar8.sepBy' getFieldParser (AChar8.char ','))
      $ SChar8.readFile "./owid-covid-data.csv"
  case r of
    Right xs -> return (foldr (\x -> if elem x columns
                                     then (++) (helperInt x xs)
                                     else (++) []) [] xs)
    Left _ -> return []

columns = [ "iso_code", "continent", "new_cases_smoothed", "new_deaths_smoothed", "new_vaccinations_smoothed", "population"]
helperInt x list = case elemIndex x list of
  Just n -> [n]
  Nothing -> []

getParsedCsv x = do
  (r, _) <- runResourceT . AStreaming.parse (getCsvParser x)
     $ SChar8.readFile "./owid-covid-data.csv"
  case r of
    Right x -> return (init x)
    Left _ -> return []

getFieldParser = AChar8.many' $ (AChar8.satisfy $ AChar8.inClass "a-zA-Z0-9") <|> (AChar8.satisfy $ AChar8.inClass "-_. '()")

getParsedRow x = do
  row <- AChar8.sepBy' getFieldParser (AChar8.char ',')
  return $ [ row !! i | i <- x]

getCsvParser x = do
  _ <- AChar8.sepBy' getFieldParser (AChar8.char ',') *> (AChar8.char '\n')
  r <- AChar8.sepBy' (getParsedRow x) (AChar8.char '\n')
  return $ map (\x -> Day (x !! 0) (x !! 1) (toDouble (x !! 2)) (toDouble (x !! 3)) (toDouble (x !! 4)) (x !! 5)) r

toDouble arg = if arg == ""
               then 0.0
               else (read arg :: Double)

getGroupedCountries days = do
  r <- Streaming.toList
    . Streaming.map (\x -> foldr foldCountry (Grouped "" "" 0.0 0.0 0.0 0.0) x)
    . Streaming.map (\x -> map getCountry x)
    . Streaming.mapped Streaming.toList
    . Streaming.groupBy (\x y -> x ^. dayCode == y ^. dayCode)
    $ Streaming.each days
  return (Streaming.fst' r)

getCountry x = Grouped (x ^. dayCode)
                      (x ^. dayContinent)
                      (x ^. dayCases)
                      (x ^. dayDeaths)
                      (x ^. dayVaccinations)
                      (toDouble (x ^. dayPopulation))

foldCountry x1 x2 = x1 & groupedTotalCases .~ (x1 ^. groupedTotalCases + x2 ^. groupedTotalCases)
  & groupedTotalDeaths .~ (x1 ^. groupedTotalDeaths + x2 ^. groupedTotalDeaths)
  & groupedTotalVaccinations .~ (x1 ^. groupedTotalVaccinations + x2 ^. groupedTotalVaccinations)

foldContinent x1 x2 = x1 & groupedName
  .~ (if (x1 ^. groupedName) == ""
      then (x2 ^. groupedName)
      else (x1 ^. groupedName) ++ "," ++ (x2 ^. groupedName))
  & groupedTotalCases .~ (x1 ^. groupedTotalCases + x2 ^. groupedTotalCases)
  & groupedTotalDeaths .~ (x1 ^. groupedTotalDeaths + x2 ^. groupedTotalDeaths)
  & groupedTotalVaccinations .~ ( x1 ^. groupedTotalVaccinations + x2 ^. groupedTotalVaccinations)
  & groupedPopulation .~ (x1 ^. groupedPopulation + x2 ^. groupedPopulation)
  & groupedContinent .~ (x2 ^. groupedContinent)
