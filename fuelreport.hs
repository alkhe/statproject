import Stat
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Monoid ((<>))

humanize :: String -> Double -> String
humanize s n = s <> ": " <> show n

readArray :: (Read a) => FilePath -> IO a
readArray = fmap read . readFile

main :: IO ()
main = do
  efficiency <- readArray "efficiency.dat" :: IO [Double]
  air <- readArray "air.dat" :: IO [String]
  let points = sortBy (comparing fst) $ zip efficiency air
  mapM_ putStrLn $ (\(s, f) -> humanize s . f $ fst <$> points) <$> summaries
  where summaries = [
                    ("Mean", mean),
                    ("Median", median),
                    ("Standard Deviation", sdev),
                    ("Variance", variance),
                    ("Interquartile Range", iqr),
                    ("Minimum", head),
                    ("Q1", q1),
                    ("Q3", q3),
                    ("Maximum", last),
                    ("Lower Fence", lowerFence),
                    ("Upper Fence", upperFence)
                    ]
