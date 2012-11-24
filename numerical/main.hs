

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
timestep = 0.001
optFps = 1000

-- Startbedingungen
l1 = 1
l2 = 1
m1 = 1
m2 = 1

phi1_0 = (pi+0.01)
phi2_0 = (pi)

k1 = (1/3) * l1^2 * m1
k2 = (1/2) * l1   * m1
k3 = m2
k4 = (1/3) * l2^2 * m2
k5 = (1/2) * l2   * m2

-- Differentialgleichungen
fphi1 p1 phi2 phi1d phi2d phi1 = ((-1)*k5*l1*(cos (phi1-phi2))*phi2d + p1)
                                 / (k1 + k3*l1)
fphi2 p2 phi1 phi1d phi2d phi2 = ((-1)*k5*l1*(cos (phi1-phi2))*phi1d + p2) / k4
fp1 phi1 phi2 phi1d phi2d = (-1)*k5*l1*phi1d*phi2d*(sin (phi1-phi2))
                               - g*k2*(sin phi1)- l1*g*k3*(sin phi1)
fp2 phi1 phi2 phi1d phi2d = k5*l1*phi1d*phi2d*(sin (phi2-phi2)) - g*k5*(sin phi2)

-- energies
t1 phi1' = (1/2) * phi1'^2 * k1
v1 phi1  = (-g) * k2 * (cos phi1)
t2 phi1 phi2 phi1' phi2' = (1/2)*l1*phi1'^2*k3+(1/2)*phi2'^2*k4+l1*phi1'*phi2'*k5*(cos (phi1-phi2))
v2 phi1 phi2 = (-g) * l1 * k3 * (cos phi1) - g * k5 * (cos phi2)

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
    let phi1d' = fphi1 p1 phi2 phi1d phi2d phi1
        phi1'  = phi1 + phi1d' * timeStep
        phi2d' = fphi2 p2 phi1 phi1d phi2d phi2
        phi2'  = phi2 + phi2d' * timeStep
        p1d' = fp1 phi1 phi2 phi1d phi2d
        p1'  = p1 + p1d' * timeStep
        p2d' = fp2 phi1 phi2 phi1d phi2d
        p2'  = p2 + p2d' * timeStep
        kin1 = t1 phi1d
        pot1 = v1 phi1
        kin2 = t2 phi1 phi2 phi1d phi2d
        pot2 = v2 phi1 phi2
    in  [phi1d, phi1, phi2d, phi2, p1d, p1, p2d, p2, kin1, pot1, kin2, pot2, (kin1+kin2), (pot1+pot2)] : (step (Pendulum phi1' phi1d' p1' p1d' phi2' phi2d' p2' p2d') timeStep (time-timeStep))

