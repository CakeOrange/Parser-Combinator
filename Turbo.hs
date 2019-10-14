module Turbo where

    import           Control.Applicative
    import           Data.Map (Map)
    import qualified Data.Map as Map
    
    import           TurboDef
    
    
    -- "Run" a Turbo program to produce SVG path commands.
    -- This is the only function that will be tested.
    runTurbo :: Stmt -> [SVGPathCmd]
    runTurbo stmt = snd (deState (turbo stmt) initTurboMem)
    -- But the actual execution engine and recursion are in "turbo" and "evalReal"
    -- below.
    
    
    -- Evaluate an expression. The State monad is needed to look up variables.
    evalReal :: RealExpr -> State TurboMem Double
    evalReal (RLit num) = return num
    evalReal (RVar str) = getVar str
    evalReal (Neg expr) = fmap negate (evalReal expr)
    evalReal (expr1 :+ expr2) =  (fmap (+) (evalReal expr1)) <*> (evalReal expr2)
    evalReal (expr1 :- expr2) =  (fmap (-) (evalReal expr1)) <*> (evalReal expr2)
    evalReal (expr1 :* expr2) =  (fmap (*) (evalReal expr1)) <*> (evalReal expr2)
    evalReal (expr1 :/ expr2) =  (fmap (/) (evalReal expr1)) <*> (evalReal expr2)
    
    
    -- Run a Turbo statement. Use the State monad to keep state. Return SVG path
    -- commands.
    turbo :: Stmt -> State TurboMem [SVGPathCmd]
    turbo (var := expr) = do evalReal expr >>= \value -> setVar var value
                             return []
    turbo PenDown = do setPen True
                       return []
    turbo PenUp = do setPen False 
                     return [] 
    turbo (Turn expr) = do evalReal expr >>= \angle -> turn angle
                           return []
    turbo (Forward expr) = (evalReal expr) >>= (\dis -> createSVGPath dis)
    -- Seq [Stmt]
    turbo (Seq []) = return [] 
    turbo (Seq (x:xs)) = (fmap (++) (turbo x)) <*> (turbo (Seq xs))
    -- For Loop
    turbo (For var expr1 expr2 lst) = do evalReal expr1 >>= \value -> setVar var value
                                         i <- getVar var
                                         to <- evalReal expr2
                                         case i > to of
                                           True -> return []
                                           otherwise -> (fmap (++) (turbo (Seq lst))) <*> (turbo (For var (expr1 :+ RLit 1) expr2 lst))
    
    -- Turbo state:
    -- * dictionary of variables->values
    -- * current direction (degrees away from x-axis, counterclockwise, e.g.,
    --   0 points east, 90 points north)
    -- * pen state (True means touching paper)
    data TurboMem = TurboMem (Map String Double) Double Bool
        deriving (Eq, Show)
    
    -- Initial Turbo state: No variables set, direction is 0 degrees, pen is up.
    initTurboMem = TurboMem Map.empty 0 False
    
    -- If you could code up the following helpers, your "turbo" implementation could
    -- be pretty clean and easy to follow.  fmap, get, modify, Map.lookup, and
    -- Map.insert will get you a long way.
    
    
    -- Change TurboMem's direction by adding the given angle.
    updateAngle :: TurboMem -> Double -> TurboMem
    updateAngle (TurboMem dic dir penstate) angle = 
                                TurboMem dic (dir+angle) penstate
    
    -- Get a variable's current value. If Nothing, return 0;
    -- If having value, extract it from Just
    getValue :: TurboMem -> String -> Double
    getValue (TurboMem dic dir penstate) var = case result of
                                                 Just a -> a
                                                 Nothing -> 0
                                                 where
                                                       result = Map.lookup var dic
        
    -- Insert a new (var, value) to dictionary                                            
    insertPair :: TurboMem -> String -> Double -> TurboMem
    insertPair (TurboMem dic dir penstate) var value = TurboMem (Map.insert var value dic) dir penstate
    
    -- Change the pen state to input bool value     
    changePenState :: TurboMem -> Bool -> TurboMem
    changePenState (TurboMem dic dir penstate) new = TurboMem dic dir new
    
    -- Calculate one single SVGPath Cmd with input distance
    calculatePath :: TurboMem -> Double -> SVGPathCmd
    calculatePath (TurboMem _ angle penstate) dis
        | penstate == True = LineTo x y
        | otherwise = MoveTo x y
        where
              x = dis * cos (angle * (pi/180))
              y = dis * sin (angle * (pi/180))
    
    -- Get current direction.
    getAngle :: State TurboMem Double
    getAngle = get >>= \mem@(TurboMem _ dir _) -> return dir
    
    -- Change direction by adding the given angle.
    turn :: Double -> State TurboMem ()
    turn angle = modify (\mem -> updateAngle mem angle)
    
    -- Get pen state.
    getPen :: State TurboMem Bool
    getPen = get >>= \mem@(TurboMem _ _ penstate) -> return penstate
    
    -- Set pen state.
    setPen :: Bool -> State TurboMem ()
    setPen penstate = modify (\mem -> changePenState mem penstate)
    
    -- Get a variable's current value.
    getVar :: String -> State TurboMem Double
    getVar var = get >>= \mem -> return (getValue mem var)
    
    -- Set a variable to value.
    setVar :: String -> Double -> State TurboMem ()
    setVar var value = modify (\mem -> insertPair mem var value)
    
    -- Create a SVG Path with givn Distence
    -- pen state (True means touching paper) 
    createSVGPath :: Double -> State TurboMem [SVGPathCmd]
    createSVGPath dis = get >>= \mem -> return [calculatePath mem dis]
        