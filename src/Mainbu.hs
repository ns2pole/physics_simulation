module Main where

-- データ出力に必要
import System.IO

-- パラメータ
x0, y0, vx0, vy0:: Double
x0 = 0
y0 = 0
vx0 = 10
vy0 = 30
generateData :: [(Double, Double, Double)]
generateData = [(t, vx0 , vy0 * t) | t <- [0, 0.5 .. 10]]

writeDataToFile :: FilePath -> IO ()
writeDataToFile path = do
    let csvLines = map (\(t, x, y) -> show t ++ "," ++ show x ++ "," ++ show y) generateData
    writeFile path (unlines csvLines)

main :: IO ()
main = writeDataToFile "uniform_motion.csv"
