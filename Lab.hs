module Lab where

{-  LANGUAGE GADTs -}

--         ∞
-- fix f = ⊔ fⁱ ⊥
--        i=0
fix :: (a -> a) -> a
fix f = f (fix f) -- A kind of magic!

type Iden = String

type Σ = Iden -> Int

-- Alias por si escribir Σ les resulta complicado
type State = Σ

-- Función de actualización de estado
--[σ|v:n]w = si (w = v) entonces n sino σw (original)
--[σ|v:n]v' = si (v = v') entonces n sino σv'
update :: Σ -> Iden -> Int -> Σ
update σ v n v' =
  if v == v'
    then n
    else σ v'

{- Para probar con eval: usen al principio eIniTest que no rompe nada si quieren
    saber cuánto termina valiendo una variable  -}

eInicial, eIniTest :: Σ
eInicial = \v -> undefined
eIniTest = \v -> 0

-- para fallas
{- Ω ≈ Σ + Σ -}
data Ω
  = Normal Σ
  | Abort Σ

{- Notar:
   * Normal : Σ → Ω
   * Abort  : Σ → Ω
   -}

-- Alias por si escribir Ω les resulta complicado
type Omega = Ω

data Expr a where
  {- Expresiones enteras -}

  -- n
  Const :: Int                  -> Expr Int
  -- v
  Var   :: Iden                 -> Expr Int
  -- e + e'
  Plus  :: Expr Int -> Expr Int -> Expr Int
  -- e - e'
  Dif   :: Expr Int -> Expr Int -> Expr Int
  -- e * e'
  Times :: Expr Int -> Expr Int -> Expr Int
  -- e / e' (división entera)
  Div   :: Expr Int -> Expr Int -> Expr Int
  -- Si e' evalúa a 0, hagan lo que quieran.

  {- Expresiones booleanas -}

  -- e = e'
  Eq   :: Expr Int  -> Expr Int -> Expr Bool
  -- e /= e'
  Neq  :: Expr Int  -> Expr Int -> Expr Bool
  -- e < e'
  Less :: Expr Int  -> Expr Int -> Expr Bool
  -- b && b'
  And  :: Expr Bool -> Expr Bool -> Expr Bool
  -- b || b'
  Or   :: Expr Bool -> Expr Bool -> Expr Bool
  -- ¬b
  Not  :: Expr Bool              -> Expr Bool

  {- Comandos -}

  -- SKIP
  Skip   ::                                    Expr Ω
  -- NEWVAR v := e IN c
  Local  :: Iden      -> Expr Int -> Expr Ω -> Expr Ω
  -- v := e
  Assign :: Iden      -> Expr Int           -> Expr Ω
  -- FAIL
  Fail   ::                                    Expr Ω
  -- CATCHIN c WITH c'
  Catch  :: Expr Ω    -> Expr Ω             -> Expr Ω
  -- WHILE b DO c
  While  :: Expr Bool -> Expr Ω             -> Expr Ω
  -- IF b THEN c ELSE c'
  If     :: Expr Bool -> Expr Ω   -> Expr Ω -> Expr Ω
  -- c ; c'
  Seq    :: Expr Ω    -> Expr Ω             -> Expr Ω


{- Completar las ecuaciones semánticas -}

class DomSem dom where
  sem :: Expr dom -> Σ -> dom

{- expresiones enteras -}
instance DomSem Int where
  sem (Const a) _     = a
  sem (Var v) σ       = σ v
  sem (Plus e1 e2) σ  = sem e1 σ + sem e2 σ -- aca tengo que completar
  sem (Dif e1 e2) σ   = sem e1 σ - sem e2 σ
  sem (Times e1 e2) σ = sem e1 σ * sem e2 σ
  sem (Div e1 e2) σ   =
    if sem e2 σ == 0
      then 0 -- se puede poner lo que quieras (convension 0)
      else sem e1 σ `div` sem e2 σ

{- expresiones booleanas -}
instance DomSem Bool where
  sem (Eq e1 e2) σ   = sem e1 σ == sem e2 σ -- aca tengo que completar
  sem (Neq e1 e2) σ  = sem e1 σ /= sem e2 σ
  sem (Less e1 e2) σ = sem e1 σ < sem e2 σ
  sem (And e1 e2) σ  = sem e1 σ && sem e2 σ
  sem (Or e1 e2) σ   = sem e1 σ || sem e2 σ
  sem (Not e) σ      = not (sem e σ)
  
{- Función de control para Ω (f_{+} y f_{*})-}
{- f_{+} se transfiere el control a f -> catchin-} 
{- f_{*} no se transfiere el control a f -> [|c_{0};c_{1} |] σ y while -}
{- f_{†} transferencia de control en cualcuier situacion -> newvar -}

(*.) :: (Σ -> Ω) -> Ω -> Ω
(*.) f (Normal σ) = f σ
(*.) _ (Abort σ)  = Abort σ

(+.) :: (Σ -> Ω) -> Ω -> Ω
(+.) _ (Normal σ) = Normal σ
(+.) f (Abort σ)  = f σ

(†.) :: (Σ -> Σ) -> Ω -> Ω
(†.) f (Normal σ) = Normal (f σ)
(†.) f (Abort σ)  = Abort (f σ)

{- Comandos -}
{- update σ v n v' -}
instance DomSem Ω where
  sem Skip σ = Normal σ -- aca tengo que completar
  sem Fail σ = Abort σ
  sem (Assign v e) σ = Normal (update σ v (sem e σ))
  sem (If b c c') σ
    | sem b σ = sem c σ
    | otherwise = sem c' σ
  sem (Seq c c') σ = sem c' *. sem c σ
  sem (Catch c c') σ = sem c' +. sem c σ
  sem (While b c) σ = fix f σ
    where
      f :: (Σ -> Ω) -> Σ -> Ω
      f g σ'
        | sem b σ' = g *. sem c σ'
        | otherwise = Normal σ'
  sem (Local v e c) σ = restore †. sem c σ'
    where
      σ' = update σ v (sem e σ)
      restore σ'' = update σ'' v (σ v)

{- ################# Funciones de evaluación de dom ################# -}
class Eval dom where
  eval :: [Iden] -> Expr dom -> Σ -> IO ()

instance Eval Int where
  eval _ e = print . sem e

instance Eval Bool where
  eval _ e = print . sem e

instance Eval Ω where
  eval lvars e = \sigma -> mapM_ (f (elsigma (sem e sigma))) lvars
    where
      elsigma (Normal σ) = σ
      elsigma (Abort σ)  = σ
      f s var = putStrLn (var ++ " vale " ++ (show (s var)))
