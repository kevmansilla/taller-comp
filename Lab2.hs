{-# LANGUAGE GADTs #-}
--         ∞
-- fix f = ⊔ fⁱ ⊥
--        i=0
fix :: (a -> a) -> a
fix f = f (fix f)

type Iden = String
type Σ = Iden -> Int

update :: Iden -> Int -> Σ -> Σ
update v n σ v'
  | v == v' = n
  | otherwise = σ v'

eInicial, eIniTest :: Σ
eInicial = \v -> undefined
eIniTest = \v -> 0

{- Ω ≈ (Σ' + Z × Ω + Z → Ω)⊥ -}
data Ω = Normal Σ | Abort Σ | Out (Int, Ω) | In (Int -> Ω)
{- Notar:
   * Normal : Σ → Ω
   * Abort  : Σ → Ω
   * Out    : (Z, Ω) → Ω
   * In     : (Z → Ω) → Ω
-}

data Expr a where
  -- Expresiones enteras
  Const  :: Int       -> Expr Int                      -- n
  Var    :: String    -> Expr Int                      -- v
  Plus, Diff, Div, Mod, Times :: Expr Int  -> Expr Int -> Expr Int          -- e + e'

  -- Expresiones booleanas
  Eq, NotEq, LeftEq, RightEq  :: Expr Int  -> Expr Int -> Expr Bool
  Not :: Expr Bool -> Expr Bool
  And, Or, Then :: Expr Bool -> Expr Bool -> Expr Bool

  -- Comandos
  Skip, Fail :: Expr Ω
  NewVar :: Iden -> Expr Int -> Expr Ω -> Expr Ω
  Assign :: Iden -> Expr Int -> Expr Ω
  Catch, Seq :: Expr Ω -> Expr Ω -> Expr Ω
  If :: Expr Bool -> Expr Ω -> Expr Ω -> Expr Ω
  While :: Expr Bool -> Expr Ω -> Expr Ω
  Output :: Expr Int -> Expr Ω
  Input :: Iden -> Expr Ω
    
class DomSem dom where
  sem :: Expr dom -> Σ -> dom

instance DomSem Int where
  sem :: Expr Int -> Σ -> Int
  sem (Const a)    _ = a
  sem (Var v)      σ = σ v
  sem (Plus e1 e2) σ = sem e1 σ + sem e2 σ
  sem (Diff e1 e2) σ = sem e1 σ - sem e2 σ
  sem (Times e1 e2) σ = sem e1 σ * sem e2 σ
  sem (Div e1 e2) σ = sem e1 σ `div` sem e2 σ
  sem (Mod e1 e2) σ = sem e1 σ `mod` sem e2 σ

instance DomSem Bool where
  sem :: Expr Bool -> Σ -> Bool
  sem (Eq e1 e2) σ = sem e1 σ == sem e2 σ
  sem (NotEq e1 e2) σ = sem e1 σ /= sem e2 σ
  sem (LeftEq e1 e2) σ = sem e1 σ <= sem e2 σ
  sem (RightEq e1 e2) σ = sem e1 σ >= sem e2 σ
  sem (Not e) σ = not (sem e σ)
  sem (And e1 e2) σ = sem e1 σ && sem e2 σ
  sem (Or e1 e2) σ = sem e1 σ || sem e2 σ
  sem (Then e1 e2) σ = not (sem e1 σ) || sem e2 σ

(*.) :: (Σ -> Ω) -> Ω -> Ω
(*.) f (Normal σ)  = f σ
(*.) _ (Abort σ)   = Abort σ
(*.) f (Out (n,ω)) = Out (n, f *. ω)
(*.) f (In g)      = In ((f *.) . g)

(†.) :: (Σ -> Σ) -> Ω -> Ω
(†.) f (Normal σ)  = Normal $ f σ
(†.) f (Abort σ)   = Abort $ f σ
(†.) f (Out (n,ω)) = Out (n, f †. ω)
(†.) f (In g)      = In ((f †.) . g)

(+.) :: (Σ -> Ω) -> Ω -> Ω
(+.) _ (Normal σ)  = Normal σ
(+.) f (Abort σ)   = f σ
(+.) f (Out (n,ω)) = Out (n, f +. ω)
(+.) f (In g)      = In ((f +.) . g)

instance DomSem Ω where
  sem :: Expr Ω -> Σ -> Ω
  sem Skip        = \σ -> Normal σ
  sem Fail        = \σ -> Abort σ
  sem (NewVar v e c) = \σ -> sem c (update v (sem e σ) σ)
  sem (Assign v e)   = \σ -> Normal $ update v (sem e σ) σ
  sem (Catch c c')   = \σ -> sem c' +. sem c σ
  sem (Seq c c')     = \σ -> sem c' *. sem c σ
  sem (If b c c')    = \σ -> if sem b σ then sem c σ else sem c' σ
  sem (While b c)    = \σ -> fix f σ
    where
      f :: (Σ -> Ω) -> Σ -> Ω
      f g σ'
        | sem b σ' = g *. sem c σ'
        | otherwise = Normal σ'
  sem (Output e) = \σ -> Out (sem e σ, Normal σ)
  sem (Input v)  = \σ -> In $ \n -> Normal $ update v n σ



{- ################# Funciones de evaluación de dom ################# -}

class Eval dom where 
  eval :: Expr dom -> Σ -> IO ()

instance Eval Int where
  eval e = print . sem e

instance Eval Bool where
  eval e = print . sem e

instance Eval Ω where
  eval e = unrollOmega . sem e
    where unrollOmega :: Ω -> IO ()
          unrollOmega (Normal σ)   = return ()
          unrollOmega (Abort σ)    = putStrLn "Abort"
          unrollOmega (Out (n, ω)) = print n >> unrollOmega ω
          unrollOmega (In f)       = getLine >>= unrollOmega . f . read

-- Ejemplo

-- Ejemplo factorial
ejemploFact :: Expr Ω
ejemploFact =
  Seq (Input "x") $
  Seq (Assign "y" (Const 1)) $
  Seq (While (NotEq (Var "x") (Const 0)) $
    Seq (Assign "y" (Times (Var "y") (Var "x"))) $
    Assign "x" (Diff (Var "x") (Const 1))
  ) $
  Output (Var "y")

ejecutarFact :: IO ()
ejecutarFact = eval ejemploFact eInicial

-- Agrego main para que solo ejecute el factorial y pida el número
main :: IO ()
main = do
  putStrLn "Ingrese un número para calcular su factorial:"
  ejecutarFact