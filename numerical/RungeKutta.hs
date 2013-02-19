{-|

This module provides functions to run the Runge-Kutta-Method on one or more
differential equations.

-}

module RungeKutta (
  rungeKutta,
  rungeKuttaStep,
  multiRungeKutta,
  multiRungeKuttaStep
) where

-- | Runs the Runge-Kutta-Method on the given function
rungeKutta :: (Fractional a)
    => (a -> a) -- ^ differential equation with 'y0' as argument and the
                -- result should be the first derivative
    -> a        -- ^ starting value
    -> a        -- ^ x step
    -> Integer  -- ^ number of samples to generates
    -> [a]      -- ^ resulting samples
rungeKutta f y0 h n
  | n <= 0 = [y0]
  | otherwise =
      let y1 = rungeKuttaStep (f) y0 h
      in y0 : rungeKutta (f) y1 h (n-1)

-- | Execute one step of the Runge-Kutta-Method for one equation.
rungeKuttaStep :: (Fractional a)
    => (a -> a) -- ^ differential equation with 'y0' as argument and the result
                -- should be the first derivative
    -> a        -- ^ starting value
    -> a        -- ^ x step
    -> a        -- ^ result value
rungeKuttaStep f y0 h =
  let y0' = f y0
      yA = y0 + h/2 * y0'
      yA' = f yA
      yB = y0 + h/2 * yA'
      yB' = f yB
      yC = y0 + h * yB'
      yC' = f yC
  in y0 + h/6 * (y0' + 2 * (yA' + yB') + yC')

-- | This function executes the Runge-Kutta-Method for a system of differential
-- equations.
multiRungeKutta :: (Fractional a)
    => [([a] -> a)] -- ^ The system of differential equations.
                    -- Each of this functions should take as many arguments as there are functions
    -> [a]          -- ^ A list with all the starting values
    -> a            -- ^ x step
    -> Integer      -- ^ number of steps to execute
    -> [[a]]        -- ^ list of resulting values
multiRungeKutta funs y0s h n
  | n <= 0 = [y0s]
  | otherwise =
      let y1s = multiRungeKuttaStep funs y0s h
      in y0s : multiRungeKutta funs y1s h (n-1)

-- | This function executes one step of the Runge-Kutta-Method for every given
-- differential equation.
multiRungeKuttaStep :: (Fractional a)
    => [([a] -> a)] -- ^ The system of differential equations.
                    -- Each of this functions should take as many arguments as there are functions
    -> [a]          -- ^ A list with all the starting values
    -> a            -- ^ x step
    -> [a]          -- ^ resulting values
multiRungeKuttaStep funs y0s h = multiRungeKuttaStepRecursive 0 (length funs) funs y0s h

-- | This function executes one 'rungeKuttaStep' for one differential equ and
-- calls itself recursively to do the other differentials. You need to give it
-- the number of differential equations and initialize the
-- recursion with 0 for n.
-- This function is not visible outside this module.
multiRungeKuttaStepRecursive :: (Fractional a) => Int -> Int -> [([a] -> a)] -> [a] -> a -> [a]
multiRungeKuttaStepRecursive n nbound funs y0s h
  | n == nbound  = []
  | otherwise =
      let y0 = rungeKuttaStep (wrapper) (y0s !! n) h
      in y0 : multiRungeKuttaStepRecursive (n+1) nbound funs y0s h
      where wrapper y = (funs !! n) (replaceNth n y y0s)
            replaceNth n newVal (x:xs)
              | n == 0 = newVal : xs
              | otherwise = x : replaceNth (n-1) newVal xs


