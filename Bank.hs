import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.List
import System.Random
import Debug.Trace

fst4 (a, _, _, _) = a
snd4 (_, b, _, _) = b
thd4 (_, _, c, _) = c
fth4 (_, _, _, d) = d

-- Банк: список счетов ("имя"-"количество денег"-"кол-во переводов"-"сумма переводов")
type TBank = TVar [(String, Int, Int, Int)]

addMoney :: TBank -> String -> Int -> STM ()
addMoney bank name amount = do
  accounts <- readTVar bank
  let newAccs = case findIndex ((==name) . fst4) accounts of
        Nothing -> (name, amount, 1, amount) : accounts
        Just i -> take i accounts ++
                  [(name, snd4 (accounts!!i) + amount, thd4 (accounts!!i) + 1, fth4 (accounts!!i) + amount)] ++
                  drop (i+1) accounts
  writeTVar bank newAccs

withdraw :: TBank -> String -> Int -> STM Bool
withdraw bank name amount = do
  accounts <- readTVar bank
  let macctId = findIndex ((==name) . fst4) accounts
  case macctId of
    Nothing -> pure False
    Just i -> do
      let oldAmt = snd4 $ accounts!!i
      let transactionCount = thd4 $ accounts!!i
      let transactionAmount = fth4 $ accounts!!i
      if isWithdrawOk amount oldAmt transactionCount transactionAmount
        then do
          let newAccs = take i accounts ++
                        [(name, oldAmt - amount, transactionCount + 1, transactionAmount + amount)] ++
                        drop (i+1) accounts
          writeTVar bank newAccs
          pure True
        else pure False

isWithdrawOk :: Int -> Int -> Int -> Int -> Bool
isWithdrawOk withdrawnAmount oldAmount transactionCount transactionAmount = do
  let fiveTimesAverage = 5 * transactionAmount `div` transactionCount
  if withdrawnAmount > oldAmount then
      -- trace ("Can't withdraw more than is stored (" ++ show withdrawnAmount ++ " > " ++ show oldAmount ++ ")")
      False
  else if withdrawnAmount > 1000 && transactionCount <= 10 then
      -- trace ("Not enough transactions (" ++ show transactionCount ++ " <= 10) to withdraw more than 1000 (" ++ show withdrawnAmount ++ ")")
      False
  else if withdrawnAmount > fiveTimesAverage then
      -- trace ("Can't withdraw " ++ show withdrawnAmount ++ " because it's less than 5 * avg (" ++ show fiveTimesAverage ++ ")")
      False
  else True

transfer :: TBank -> String -> String -> Int -> STM ()
transfer bank from to amt = do
  b <- withdraw bank from amt
  when b $ addMoney bank to amt

printBank :: String -> TBank -> IO ()
printBank prefix bank = do
  putStrLn prefix
  accs <- readTVarIO bank
  forM_ accs $ \(name, amt, count, totalAmt) -> do
    putStrLn $ name ++ "\t" ++ show amt ++ "\t" ++ show count ++ "\t" ++ show totalAmt

randomTransfers :: TBank -> [String] -> Int -> IO ()
randomTransfers bank accounts count = replicateM_ count $ do
  let n = length accounts
  f <- randomRIO (0, n-1)
  t <- randomRIO (0, n-1)
  let from = accounts !! f
      to = accounts !! t
  amt <- randomRIO (0, 1000)
  atomically $ transfer bank from to amt

main :: IO ()
main = do
  bank <- newTVarIO []

  let actions =
        [ ("Alice", 100)
        , ("Bob", 1000)
        , ("Alice", 300)
        , ("Bob", 200)
        , ("Alice", 50)
        , ("Bob", 30)
        , ("Alice", 70)
        , ("Bob", 50)
        , ("Charlie", 10)
        , ("Charlie", 20)
        , ("Charlie", 30)
        , ("Charlie", 40)
        , ("Charlie", 50)
        , ("Charlie", 60)
        ]
  forConcurrently_ actions $ \(who, amt) -> do
    atomically $ addMoney bank who amt
  printBank "Initial" bank

  atomically $ withdraw bank "Alice" 2000  -- больше, чем можем снять
  atomically $ withdraw bank "Bob" 1100    -- снятие больше 1000, переводов < 10 
  atomically $ withdraw bank "Charlie" 180 -- сумма больше, чем 5 * средний размер перевода
  printBank "Withdrawals (not OK)" bank    -- результат не должен отличаться от Initial

  atomically $ withdraw bank "Alice" 50
  atomically $ withdraw bank "Bob" 100
  atomically $ withdraw bank "Charlie" 150
  printBank "Withdrawals (OK)" bank

  atomically $ transfer bank "Bob" "Alice" 500
  atomically $ transfer bank "Alice" "Charlie" 300
  printBank "Transfers" bank

  accounts <- map fst4 <$> readTVarIO bank
  oldMoney <- sum . map snd4 <$> readTVarIO bank

  asyncs <- forM [1..100] $ \_ -> async $
    randomTransfers bank accounts 100

  foldM_ (\() a -> wait a) () asyncs

  newMoney <- sum . map snd4 <$> readTVarIO bank
  printBank "Random" bank
  print (oldMoney, newMoney)
