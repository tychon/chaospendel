
----------------
-- prerequisites
import System.IO
import System.Exit
import System.Environment
import Data.List.Split
import RungeKutta

-- Naturkonstante:
-- Durchschnittliche Ortskraft in Mitteleuropa.
g = 9.81
-- Andere Konstanten
time = 60.0
timestep = 0.001
optFps = 60

-- Startbedingungen
l1 = 4
l2 = 3
m1 = 2
m2 = 1

phi1_0 = pi/5
phi2_0 = pi/4

k1 = (1/3) * l1^2 * m1
k2 = (1/2) * l1   * m1
k3 = m2
k4 = (1/3) * l2^2 * m2
k5 = (1/2) * l2   * m2

-- Differentialgleichungen
fphi1 (phi1:phi2:p1:p2:_) = (k4*p1-k5*l1*(cos (phi1-phi2))*p2) / (k1*k4+l1*(k3*k4-k5^2*l1*(cos (phi1-phi2))^2))
fphi2 (phi1:phi2:p1:p2:_) = (k1*p2+l1*(k3*p2-k5*p1*(cos (phi1-phi2)))) / (k1*k4+l1*(k3*k4-k5^2*l1*(cos (phi1-phi2))^2))
fp1 input@(phi1:phi2:p1:p2:_) = -l1*(fphi1 input)*(fphi2 input)*k5*(sin (phi1-phi2)) - g*k2*(sin phi1) - g*l1*k3*(sin phi1)
fp2 input@(phi1:phi2:p1:p2:_) =  l1*(fphi1 input)*(fphi2 input)*k5*(sin (phi1-phi2)) - g*k5*(sin phi2)

-- energies
t1 phi1' = (1/2) * phi1'^2 * k1
v1 phi1  = (-g) * k2 * (cos phi1)
t2 phi1 phi2 phi1' phi2' = (1/2)*l1*phi1'^2*k3+(1/2)*phi2'^2*k4+l1*phi1'*phi2'*k5*(cos (phi1-phi2))
v2 phi1 phi2 = (-g) * l1 * k3 * (cos phi1) - g * k5 * (cos phi2)

-- Data structure for Pendulum:
data Pendulum = Pendulum {
  getPhi1     :: Double, -- Auslenkung 1 als Winkel
  getPhi2     :: Double, -- Auslenkung 2 als Winkel
  getP1     :: Double,
  getP2     :: Double
} deriving (Read, Show)

-------
-- main

main :: IO ()
main = do
  -- read command line arguments
  args <- getArgs
  let pend = parseArgs (Pendulum phi1_0 phi2_0 0 0) args
  -- write info data to stderr
  hPutStrLn stderr $ "time          ="++(show time)
  hPutStrLn stderr $ "integral_step ="++(show timestep)
  hPutStrLn stderr $ "opt_fps       ="++(show optFps)
  hPutStrLn stderr $ "time_step     ="++(show (1/optFps))
  hPutStrLn stderr $ "frames_loss   ="++(show $ 1-optFps*timestep)
  hPutStrLn stderr $ "l1="++(show l1)
  hPutStrLn stderr $ "l2="++(show l2)
  hPutStrLn stderr $ "phi1="++(show (getPhi1 pend))
  hPutStrLn stderr $ "phi2="++(show (getPhi2 pend))
  -- run simulation
  let fullres = step pend timestep time
      outres = every (fromIntegral $ toInteger $ ceiling ((1/timestep)/optFps)) fullres
  -- write simulation data to stdout
  putStr . formatCSV $ outres
  exitWith ExitSuccess

parseArgs (Pendulum phi1 phi2 mom1 mom2) ("1":angle:args) =
  parseArgs (Pendulum (read angle) phi2 mom1 mom2) args
parseArgs (Pendulum phi1 phi2 mom1 mom2) ("2":angle:args) =
  parseArgs (Pendulum phi1 (read angle) mom1 mom2) args
parseArgs pend [] = pend

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
step (Pendulum phi1 phi2 p1 p2) timeStep time
  | time <= 0 = [] -- Abbruchbedingung fuer Rekursion
  | (isNaN phi1) || (isNaN phi2) || (isNaN p1) || (isNaN p2) || (isInfinite phi1) || (isInfinite phi2) || (isInfinite p1) || (isInfinite p2) = []
  | otherwise =
    let input = [phi1, phi2, p1, p2]
        kin1 = t1 $ fphi1 input
        pot1 = v1 phi1
        kin2 = t2 phi1 phi2 (fphi1 input) (fphi2 input)
        pot2 = v2 phi1 phi2
        (phi1':phi2':p1':p2':_) = multiRungeKuttaStep [fphi1, fphi2, fp1, fp2] input timeStep
    in  [phi1, phi2, p1, p2, kin1, pot1, kin2, pot2, (kin1+kin2), (pot1+pot2), (kin1+kin2+pot1+pot2)] : (step (Pendulum phi1' phi2' p1' p2') timeStep (time-timeStep))

