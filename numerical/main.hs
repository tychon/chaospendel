

----------------
-- prerequisites
import System.IO
import System.Environment
import Data.List.Split

-- Naturkonstante:
-- Durchschnittliche Ortskraft in Mitteleuropa.
g = -9.81
-- Andere Konstanten
time = 60.0
timestep = 0.0001
optFps = 1000
l1 = 4
l2 = 3
m1 = 2
m2 = 1

phi1_0 = (pi/3)
phi2_0 = (0)

k1 = (1/3) * l1^2 * m1
k2 = (1/2) * l1   * m1
k3 = m2
k4 = (1/3) * l2^2 * m2
k5 = (1/2) * l2   * m2

-- Data structure for Pendulum:
data Pendulum = Pendulum {
  getPhi1     :: Double, -- Auslenkung 1 als Winkel
  getPhi1Diff :: Double,
  getP1     :: Double,
  getP1Diff :: Double,
  getPhi2     :: Double, -- Auslenkung 2 als Winkel
  getPhi2Diff :: Double,
  getP2     :: Double,
  getP2Diff :: Double
} deriving (Read, Show)

-------
-- main

main :: IO ()
main = do
  hPutStrLn stderr $ "time          ="++(show time)
  hPutStrLn stderr $ "integral_step ="++(show timestep)
  hPutStrLn stderr $ "opt_fps       ="++(show optFps)
  hPutStrLn stderr $ "time_step     ="++(show (1/optFps))
  hPutStrLn stderr $ "frames_loss   ="++(show $ 1-optFps*timestep)
  let pend = Pendulum phi1_0 0 0 0 phi2_0 0 0 0
      fullres = step pend timestep time
      outres = every (fromIntegral $ toInteger $ ceiling ((1/timestep)/optFps)) fullres
  putStr . formatCSV $ outres
  

-- Formatiert eine Liste von Listen mit Doubles als Comma-separated values in einen String.
formatCSV :: [[Double]] -> String
formatCSV = unlines . splitOn "],[" . drop 2 . init . init . show

-- Gibt aus einer Liste xs jedes n-te Element zurueck.
every n xs =
  case drop (n-1) xs of
    (y:ys) -> y : every n ys
    [] -> []

-- Konvertiert einen Winkel im Bogenmass zum Gradmass.
toDeg :: Double -> Double
toDeg = (180/pi *)

step :: Pendulum -> Double -> Double -> [[Double]]
step (Pendulum phi1 phi1d p1 p1d phi2 phi2d p2 p2d) timeStep time
  | time <= 0 = [] -- Abbruchbedingung fuer Rekursion
  | otherwise =
    let phi1d' = ((-1)*k5*l1*(cos (phi1-phi2))*phi2d + p1) / (k1 + k3*l1)
        phi1'  = phi1 + phi1d' * timeStep
        phi2d' = ((-1)*k5*l1*(cos (phi1-phi2))*phi1d - p2) / k4
        phi2'  = phi2 + phi2d' * timeStep
        p1d' = (-1)*k5*l1*phi1d*phi2d*(sin (phi1-phi2)) - g*k2*(sin phi1) - l1*g*k3*(sin phi1)
        p1'  = p1 + p1d' * timeStep
        p2d' =      k5*l1*phi1d*phi2d*(sin (phi2-phi2)) - g*k5*(sin phi2)
        p2'  = p2 + p2d' * timeStep
    in  [phi1d, phi1, phi2d, phi2, p1d, p1, p2d, p2] : (step (Pendulum phi1' phi1d' p1' p1d' phi2' phi2d' p2' p2d') timeStep (time-timeStep))

