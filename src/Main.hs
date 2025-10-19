module Main (main) where
import Text.Printf (printf)


-- type synonyms（シノニム）
type Time     = Double
type Position = Double
type Velocity = Double
type Accel    = Double
type Force    = Double
type Mass     = Double
type Dt       = Double

-- シミュレーションパラメータ
dt :: Dt
dt = 0.01

totalT :: Time
totalT = 10.0

-- ステップ数（時間点は steps + 1 個）
steps :: Int
steps = floor (totalT / dt)

m :: Mass
m = 2.0

g :: Accel
g = 9.8

-- 時刻リスト: 0, dt, 2dt, ..., totalT  （長さ = steps + 1）
times :: [Time]
times = [0, dt .. totalT]

-- 力リスト: 各区間に対応する力（長さ = steps）
-- ここでは重力のみを一定としている
forces :: [Force]
forces = replicate steps (m * (-g))

-- example: time-dependent force (here: constant gravity)
forceAt :: Time -> Force
forceAt _ = m * (-g)

-- 質量 m と力リストから加速度リストを生成: a_i = F_i / m
getAcc :: Mass -> [Force] -> [Accel]
getAcc mass fs = map (/ mass) fs

-- 初期条件
v0 :: Velocity
v0 = 2.0

x0 :: Position
x0 = 10.0


-- CSV 用行リストを生成（ヘッダ付き）
csvHeader :: String
csvHeader = "time,position,velocity"



-- 物理的な状態を表すデータ型
data PhysicsState = PhysicsState
  { step :: Int
  , t :: Time  -- 時間
  , x :: Position  -- 位置
  , v :: Velocity  -- 速度
  , a :: Accel  -- 加速度
  , f :: Force  -- 力
  } deriving (Show, Eq)

-- 範囲を丸める関数
clamp :: Int -> Int -> Int -> Int
clamp lo hi xx = max lo (min hi xx)

initialState :: PhysicsState
initialState =
  let f0 = forceAt 0
      a0 = f0 / m
  in PhysicsState
       { step = 0
       , t = 0
       , x = x0
       , v = v0
       , a = a0
       , f = f0
       }

-- 次の状態を計算
getNextState :: PhysicsState -> PhysicsState
getNextState s =
  let i  = step s
      t' = t s + dt
      v' = v s + a s * dt
      x' = x s + v s * dt
      f' = getForceByStep (i + 1)
      a' = f' / m
  in PhysicsState { step = i + 1, t = t', x = x', v = v', a = a', f = f' }

-- ステップ番号で forces を参照（範囲外は丸め、空なら 0）
getForceByStep :: Int -> Force
getForceByStep idx
  | null forces = 0
  | otherwise   = forces !! safeIdx
  where
    safeIdx = clamp 0 (length forces - 1) idx


-- 状態をCSV 1 行に変換
stateToCsv :: PhysicsState -> String
stateToCsv s =
  printf "%.4f,%.6f,%.6f,%.6f,%.6f"
    (t s) (x s) (v s) (a s) (f s)

-- シミュレーション実行
simulate :: Int -> PhysicsState -> [PhysicsState]
simulate n s = take n (iterate getNextState s)

main :: IO ()
main = do
  let states = simulate 1000 initialState   -- 1000 ステップ分
      csvHeader = "time,position,velocity,accel,force"
      csvLines  = map stateToCsv states
      content   = unlines (csvHeader : csvLines)
  writeFile "motion.csv" content
  putStrLn $ "Wrote motion.csv (" ++ show (length states) ++ " rows)."

