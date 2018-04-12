import Data.List
import Data.Lists
import System.Random
import Criterion.Main
import Control.Parallel
import Control.Parallel.Strategies as S
import Control.Monad.Par
import Test.QuickCheck.Modifiers
import Control.DeepSeq
--import Data.List.Utils


-- code borrowed from the Stanford Course 240h (Functional Systems in Haskell)
-- I suspect it comes from Bryan O'Sullivan, author of Criterion

data T a = T !a !Int


mean :: (RealFrac a) => [a] -> a
mean = fini . foldl' go (T 0 0)
  where
    fini (T a _) = a
    go (T m n) x = T m' n'
      where m' = m + (x - m) / fromIntegral n'
            n' = n + 1


resamples :: Int -> [a] -> [[a]]
resamples k xs =
    take (length xs - k) $
    zipWith (++) (inits xs) (map (drop k) (tails xs))


jackknife :: ([a] -> b) -> [a] -> [b]
jackknife f = map f . resamples 500

crud = zipWith (\x a -> sin (x / 300)**2 + a)  [0..]

main = do
  let (xs,ys) = splitAt 1500  (take 6000
                               (randoms (mkStdGen 211570155)) :: [Float] )

  let unsortedList = take 22000 (randoms (mkStdGen 211570155)) :: [Float]
  -- handy (later) to give same input different parallel functions

  let rs = crud xs ++ ys
  putStrLn $ "sample mean:    " ++ show (mean rs)

  let j = jackknife mean rs :: [Float]
  putStrLn $ "jack mean min:  " ++ show (minimum j)
  putStrLn $ "jack mean max:  " ++ show (maximum j)
  defaultMain

        [
           bench "mergeSort"      (nf (mergeSort ) unsortedList),
           bench "mMergeSort"     (nf (mMergeSort) unsortedList),
           bench "pMergeSort"     (nf (pMergeSort) unsortedList)
         ]
{-
        [
           bench "jackknife"       (nf (jackknife       mean) rs),
           bench "pjackknife"      (nf (pjackknife      mean) rs),
           bench "parMapjackknife" (nf (parMapjackknife      mean) rs),
           bench "rjackknife"      (nf (rjackknife      mean) rs),
           bench "sjackknife"      (nf (sjackknife  mean) rs),
           bench "mjackknife"      (nf (mjackknife  mean) rs)
         ]
-}
-- 1a
pmap_ :: (NFData b) => (a -> b) -> [a] -> [b]
pmap_ f xs = (pmap 60 f (take 1500 xs)) -- ++  --(pmap 500 f $ drop 1500 xs)

pmap :: (NFData b) => Int -> (a -> b) -> [a] -> [b]
pmap th f [] = []
pmap size f xs = fx `par` (fxs `pseq` (fx ++ fxs))
  where
    fx     = force $ map f (take size xs)
    fxs    = pmap size f (drop size xs)

pjackknife :: (NFData b) => ([a] -> b) -> [a] -> [b]
pjackknife f xs' = (pmap 100 f (take 1500 xs)) ++ (jackknife f $ drop 1500 xs')
                      where xs = resamples 500 xs'

parMapjackknife :: (NFData b) => ([a] -> b) -> [a] -> [b]
parMapjackknife f xs' = (S.parMap rdeepseq f (take 1500 xs)) ++ (jackknife f $ drop 1500 xs')
                      where xs = resamples 500 xs'

-- 1b
rmap :: (NFData b) => Int -> (a -> b) -> [a] -> [b]
rmap size _ []     = []
rmap size f (x:xs) = if (size < 2500) then map f xs else runEval $ do
        fx  <- rpar (force $ f x)
        fxs <- rseq (rmap (size - 1) f xs)
        return (fx:fxs)

rjackknife :: (NFData b) => ([a] -> b) -> [a] -> [b]
rjackknife f = rmap 6000 f . resamples 500


sjackknife :: (NFData b) => ([a] -> b) -> [a] -> [b]
sjackknife f xs = using list (parListChunk 150 rdeepseq)
    where list = jackknife f xs

mjackknife :: (NFData b) => ([a] -> b) -> [a] -> [b]
mjackknife f xs = runPar $ parMapM (return . f) $ resamples 500 xs

split :: (NFData a) => [a] -> ([a], [a])
split myList = splitAt (((length myList) + 1) `div` 2) myList



mergeSort :: (Ord a, NFData a) => [a] -> [a]
mergeSort xs = case Main.split xs of
        ([], [])       -> []
        (x:[], [])     -> [x]
        (sp1, sp2) -> merge (mergeSort sp1) (mergeSort sp2)

mMergeSort :: (Ord a, NFData a) => [a] -> [a]
mMergeSort xs = case Main.split xs of
        ([], [])       -> []
        (x:[], [])     -> [x]
        (sp1, sp2) -> if ((length sp1) < 500) then merge (mergeSort sp1) (mergeSort sp2)
                                              else runPar $ do
              i <- new
              j <- new
              fork (put i (mMergeSort sp1))
              fork (put j (mMergeSort sp2))
              rs1 <- get i
              rs2 <- get j
              return $ merge rs1 rs2

pMergeSort :: (Ord a, NFData a) => [a] -> [a]
pMergeSort xs = case Main.split xs of
        ([], [])       -> []
        (x:[], [])     -> [x]
        (sp1, sp2) -> if ((length sp1) < 1000) then merge (mergeSort sp1) (mergeSort sp1)
                              else par rs1 $ pseq rs2 $ merge rs1 rs2
                      where rs1 = force $ pMergeSort sp1
                            rs2 = force $ pMergeSort sp2
