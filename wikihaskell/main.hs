

----------------
-- prerequisites
import System.Environment
import Data.List.Split

-- Durchschnittliche Ortskraft in Mitteleuropa.
g = -9.81

-- Data structure for Pendulum:
data Pendulum = Pendulum {
  getL :: Double,    -- Laenge der Pendel
  getMass :: Double, -- Masse pro Pendel
  
  getPhi1 :: Double, -- Auslenkung 1 als Winkel
  getP1 :: Double,
  getPhi2 :: Double, -- Auslenkung 2 als Winkel
  getP2 :: Double
} deriving (Read, Show)

-------
-- main

main :: IO ()
main =
  let pend = Pendulum 1 1 0.001 0 0 0
  in putStr . formatCSV $ step pend 0.001 60.0

-- Formatiert eine Liste von Listen mit Doubles als Comma-separated values in einen String.
formatCSV :: [[Double]] -> String
formatCSV = unlines . splitOn "],[" . drop 2 . init . init . show

-- Konvertiert einen Winkel im Bogenmass zum Gradmass.
toDeg :: Double -> Double
toDeg = (180/pi *)

step :: Pendulum -> Double -> Double -> [[Double]]
step (Pendulum l mass phi1 p1 phi2 p2) timeStep time
  | time <= 0 = [] -- Abbruchbedingung fuer Rekursion
  | otherwise =
    let phi1vel = (6/mass*l^2) * ((2*p1 - 3*(cos (phi1-phi2))*p2) / (16 - 9*(cos (phi1 - phi2))^2))
        phi1' = phi1 + phi1vel * timeStep
        phi2vel = (6/mass*l^2) * ((8*p2 - 3*(cos (phi1-phi2))*p1) / (16 - 9*(cos (phi1 - phi2))^2))
        phi2' = phi2 + phi2vel * timeStep
        p1vel = ((-1)/2*mass*l^2) * (phi1vel*phi2vel*(sin (phi1 - phi2)) + 3*(g/l)*(sin phi1))
        p1' = p1 + p1vel * timeStep
        p2vel = ((-1)/2*mass*l^2) * ((-phi1vel)*phi2vel*(sin (phi1 - phi2)) + (g/l)*(sin phi2))
        p2' = p2 + p2vel * timeStep
    in  [phi1vel, phi1, phi2vel, phi2] : (step (Pendulum l mass phi1' p1' phi2' p2') timeStep (time-timeStep))

