module Main where

import Text.Printf (printf)

-- 時間刻み幅

dt :: Double
dt = 0.01

-- 質量 m と重力加速度 g
m :: Double
m = 2.0

g :: Double
g = 9.8

-- 時刻リスト: 0.0 から 10.0 まで dt 間隔
times :: [Double]
times = [0, dt .. 10]

forces :: [Double]
forces = replicate (length times) (m * (-g))


-- 力リスト: 一定の重力を 0～10秒まで dt 間隔で 21 ステップ

-- 質量 m と力リストから加速度リストを生成: a_i = F_i / m
getAcc :: Double -> [Double] -> [Double]
getAcc mass fs = map (/ mass) fs

-- 現在速度 v_n と加速度 a_n から v_{n+1} を計算
getNextV :: Double -> Double -> Double
getNextV v a = v + a * dt

-- 現在位置 x_n と速度 v_n から x_{n+1} を計算
getNextX :: Double -> Double -> Double
getNextX x v = x + v * dt

-- 初速度 v0 と加速度リストから速度リストを再帰生成
getVList :: Double -> [Double] -> [Double]
getVList _  []     = []
getVList v0 (a:as) =
    let v1 = getNextV v0 a
    in  v1 : getVList v1 as

-- 初期位置 x0 と速度リストから位置リストを再帰生成
getXList :: Double -> [Double] -> [Double]
getXList _  []     = []
getXList x0 (v:vs) =
    let x1 = getNextX x0 v
    in  x1 : getXList x1 vs


-- 加速度リスト
accs :: [Double]
accs = getAcc m forces

-- 初期速度・初期位置
v0 :: Double
v0 = 10.0

x0 :: Double
x0 = 10.0

-- 速度・位置リスト
vels :: [Double]
vels = getVList v0 accs

poses :: [Double]
poses = getXList x0 vels

-- CSV 用行リストを生成
csvLines :: [String]
csvLines = zipWith3 (\t x v -> printf "%.2f,%.1f,%.1f" t x v) times poses vels

-- main: CSV ファイル "motion.csv" に書き出す
main :: IO ()
main = writeFile "motion.csv" (unlines csvLines)