{-# OPTIONS_GHC -Wall #-}
module MyLispLike where

import Text.ParserCombinators.Parsec
import Data.Char
import System.IO

-- Internal AST for S-expressions
data Sexp
  = Snil
  | Ssym String
  | Snum Int
  | Snode Sexp [Sexp]
  deriving (Show, Eq)

-- Convert S-expression to a printable string
showSexp' :: Sexp -> ShowS
showSexp' Snil       = showString "()"
showSexp' (Snum n)   = shows n
showSexp' (Ssym s)   = showString s
showSexp' (Snode h t) =
  let showTail []     = showChar ')'
      showTail (e:es) = showChar ' ' . showSexp' e . showTail es
  in showChar '(' . showSexp' h . showTail t

showSexp :: Sexp -> String
showSexp e = showSexp' e ""

-- Lexer (handles comments and whitespace)
pChar :: Char -> Parser ()
pChar c = do _ <- char c; return ()

pComment :: Parser ()
pComment = do
  pChar ';'
  _ <- many (satisfy (/= '\n'))
  _ <- (pChar '\n' <|> eof)
  return ()

pSpaces :: Parser ()
pSpaces = do
  _ <- many (space *> pure () <|> pComment)
  return ()

integer :: Parser Int
integer = (do
  c <- digit
  integer' (digitToInt c))
  <|> (do
    _ <- char '-'
    n <- integer
    return (-n))
  where
    integer' n = (do
      c <- digit
      integer' (10 * n + digitToInt c))
      <|> return n

pSymchar :: Parser Char
pSymchar = alphaNum
           <|> satisfy (\c -> not (isAscii c) || c `elem` "!@$%^&*_+-=:|/?<>")

pSymbol :: Parser Sexp
pSymbol = do
  s <- many1 pSymchar
  case parse integer "" s of
    Right n -> return (Snum n)
    _       -> return (Ssym s)

-- Parser (S-expression level)
pQuote :: Parser Sexp
pQuote = do
  pChar '\''
  pSpaces
  e <- pSexp
  return (Snode (Ssym "quote") [e])

pList :: Parser Sexp
pList = do
  pChar '('
  pSpaces
  ses <- pTail
  return $
    case ses of
      []       -> Snil
      (x : xs) -> Snode x xs

pTail :: Parser [Sexp]
pTail = (pChar ')' >> return [])
    <|> (do
          e <- pSexp
          pSpaces
          es <- pTail
          return (e : es))

pAny :: Parser (Maybe Char)
pAny = (Just <$> anyChar) <|> return Nothing

pSexpTop :: Parser Sexp
pSexpTop = do
  pSpaces
  pList <|> pQuote <|> pSymbol
  <|> do
    x <- pAny
    case x of
      Nothing -> pzero
      Just c  -> error ("Unexpected character: " ++ [c])

pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

pSexps :: Parser [Sexp]
pSexps = pSpaces >> many (pSexpTop <* pSpaces)

instance Read Sexp where
  readsPrec _ s =
    case parse pSexp "" s of
      Left _  -> []
      Right e -> [(e, "")]

-- Convert Sexp to an intermediate Lexp
type Var = String

data Type
  = Terror String
  | Tnum
  | Tbool
  | Tfob [Type] Type
  deriving (Show, Eq)

data Lexp
  = Lnum Int
  | Lbool Bool
  | Lvar Var
  | Ltype Lexp Type
  | Ltest Lexp Lexp Lexp
  | Lfob [(Var, Type)] Lexp
  | Lsend Lexp [Lexp]
  | Llet Var Lexp Lexp
  | Lfix [(Var, Lexp)] Lexp
  deriving (Show, Eq)

convertType :: [Sexp] -> Type
convertType [Ssym "Num"]  = Tnum
convertType [Ssym "Bool"] = Tbool
convertType sexps =
  case break (== Ssym "->") sexps of
    (argTypes, _ : retType) ->
      let parsedArgs = map (convertType . toList) argTypes
          parsedRet  = convertType retType
      in Tfob parsedArgs parsedRet
    _ -> error "Unknown type"

toList :: Sexp -> [Sexp]
toList Snil            = []
toList (Ssym x)        = [Ssym x]
toList (Snode f rest)  = f : rest
toList sexp            = error ("Not a list: " ++ showSexp sexp)

symToVar :: Sexp -> Var
symToVar (Ssym v) = v
symToVar sexp     = error ("Not a symbol: " ++ showSexp sexp)

parseArgument :: Sexp -> (Var, Type)
parseArgument (Snode (Ssym varName) [typ]) = (varName, convertType (toList typ))
parseArgument _                            = error "Invalid argument type"

s2l :: Sexp -> Lexp
s2l sexp =
  case sexp of
    Snum n -> Lnum n
    Ssym s -> Lvar s
    Snode (Ssym "if") [cond, thenExp, elseExp] ->
      Ltest (s2l cond) (s2l thenExp) (s2l elseExp)
    Snode (Ssym "fob") [argList, body] ->
      let arguments =
            case argList of
              Snil -> []
              Snode (Snode (Ssym varName) [typ]) rest ->
                (varName, convertType (toList typ))
                : map parseArgument rest
              _ -> error "Invalid argument list"
      in Lfob arguments (s2l body)
    Snode (Ssym "let") [var, valExp, bodyExp] ->
      Llet (symToVar var) (s2l valExp) (s2l bodyExp)
    Snode (Ssym "fix") [declarations, body] ->
      let parseDecl decl =
            case decl of
              Snode (Snode (Ssym f) args) [exp'] ->
                (f, Lfob (map parseArgument args) (s2l exp'))
              Snode (Snode (Ssym f) args) [typ, exp'] ->
                ( f
                , Lfob (map parseArgument args)
                       (Ltype (s2l exp') (convertType (toList typ))) )
              Snode (Ssym varName) [exp'] ->
                (varName, s2l exp')
              Snode (Ssym varName) [typ, exp'] ->
                ( varName
                , Ltype (s2l exp') (convertType (toList typ)) )
              _ -> error "Invalid declaration"
      in Lfix (map parseDecl (toList declarations)) (s2l body)
    Snode (Ssym ":") [exp', typ] ->
      Ltype (s2l exp') (convertType (toList typ))
    Snode func args ->
      Lsend (s2l func) (map s2l args)
    _ -> error ("Unknown expression: " ++ showSexp sexp)

-- Runtime values
data Value
  = Vnum Int
  | Vbool Bool
  | Vbuiltin ([Value] -> Value)
  | Vfob VEnv Int Dexp

instance Show Value where
  showsPrec p (Vnum n)    = showsPrec p n
  showsPrec p (Vbool b)   = showsPrec p b
  showsPrec _ (Vbuiltin _) = showString "<builtin>"
  showsPrec _ (Vfob _ _ _) = showString "<fob>"

type Env = [(Var, Type, Value)]

env0 :: Env
env0 =
  let binop f op =
        Vbuiltin (\vs -> case vs of
          [Vnum n1, Vnum n2] -> f (n1 `op` n2)
          _                  -> error "Invalid arguments")
      intbin  = Tfob [Tnum, Tnum] Tnum
      boolbin = Tfob [Tnum, Tnum] Tbool
  in
  [ ("+", intbin,  binop Vnum (+))
  , ("*", intbin,  binop Vnum (*))
  , ("/", intbin,  binop Vnum div)
  , ("-", intbin,  binop Vnum (-))
  , ("<", boolbin, binop Vbool (<))
  , (">", boolbin, binop Vbool (>))
  , ("≤", boolbin, binop Vbool (<=))
  , ("≥", boolbin, binop Vbool (>=))
  , ("=", boolbin, binop Vbool (==))
  , ("true",  Tbool, Vbool True)
  , ("false", Tbool, Vbool False)
  ]

type TEnv = [(Var, Type)]

-- Type checker
check :: Bool -> TEnv -> Lexp -> Type
check _ _ (Lnum _)  = Tnum
check _ _ (Lbool _) = Tbool
check _ env (Lvar v) =
  case lookup v env of
    Just t  -> t
    Nothing -> Terror ("Unknown variable: " ++ v)

check ctx env (Ltype e t) =
  let t' = check ctx env e
  in if t' == t
     then t
     else Terror ("Type mismatch: " ++ show t' ++ " vs " ++ show t)

check ctx env (Ltest c t f) =
  let ct = check ctx env c
      tt = check ctx env t
      ft = check ctx env f
  in if ct == Tbool
     then if tt == ft
          then tt
          else Terror "Branches have different types"
     else Terror "Condition must be boolean"

check ctx env (Lsend fn args) =
  case check ctx env fn of
    Tfob pts rt ->
      if length pts == length args
      then
        let ats = map (check ctx env) args
            cmp (Terror _) _ = ctx
            cmp _ (Terror _) = ctx
            cmp t1 t2        = t1 == t2
        in if all (uncurry cmp) (zip ats pts)
           then rt
           else Terror "Function argument types mismatch"
      else Terror "Incorrect number of arguments"
    _ -> Terror "Not a function"

check ctx env (Lfob ps b) =
  let env' = env ++ ps
      bt   = check True env' b
  in Tfob (map snd ps) bt

check ctx env (Llet v e1 e2) =
  let t1   = check True env e1
      env' = (v, t1) : env
  in check ctx env' e2

check ctx env (Lfix decls b) =
  let env'      = [(n, Tfob (map snd ps) (Terror "Unknown type")) | (n, Lfob ps _) <- decls] ++ env
      partialTs = [(n, check False env' e) | (n, e) <- decls]
      valEnv    = [(n, t) | (n, t@(Tfob _ _)) <- partialTs] ++ env
      recheck   = [(n, check True valEnv e) | (n, e) <- decls]
      hasErr (Terror _) = True
      hasErr _          = False
  in if any (hasErr . snd) recheck
     then Terror "Error in recursive definitions"
     else check ctx valEnv b

-- De Bruijn transformation
type VarIndex = Int

data Dexp
  = Dnum Int
  | Dbool Bool
  | Dvar VarIndex
  | Dtest Dexp Dexp Dexp
  | Dfob Int Dexp
  | Dsend Dexp [Dexp]
  | Dlet Dexp Dexp
  | Dfix [Dexp] Dexp
  deriving (Show, Eq)

lookupDI :: TEnv -> Var -> Int -> Int
lookupDI ((x1, _):xs) x2 n = if x1 == x2 then n else lookupDI xs x2 (n + 1)
lookupDI [] _ _            = error "Variable not found"

l2d :: TEnv -> Lexp -> Dexp
l2d _   (Lnum n)     = Dnum n
l2d _   (Lbool b)    = Dbool b
l2d env (Lvar v)     = Dvar (lookupDI env v 0)
l2d env (Ltype e _)  = l2d env e
l2d env (Ltest c t f)= Dtest (l2d env c) (l2d env t) (l2d env f)
l2d env (Llet v e1 e2) =
  let d1   = l2d env e1
      env' = (v, undefined) : env
  in Dlet d1 (l2d env' e2)
l2d env (Lfix ds b) =
  let env' = [(v, undefined) | (v, _) <- ds] ++ env
      dsD  = map (l2d env' . snd) ds
  in Dfix dsD (l2d env' b)
l2d env (Lsend f as) =
  Dsend (l2d env f) (map (l2d env) as)
l2d env (Lfob ps b) =
  Dfob (length ps) (l2d (ps ++ env) b)

-- Evaluator
type VEnv = [Value]

eval :: VEnv -> Dexp -> Value
eval _   (Dnum n)     = Vnum n
eval _   (Dbool b)    = Vbool b
eval env (Dvar x)
  | x < length env    = env !! x
  | otherwise         = error "Unbound variable"

eval env (Dtest c b1 b2) =
  case eval env c of
    Vbool True  -> eval env b1
    Vbool False -> eval env b2
    _           -> error "Non-boolean condition"

eval env (Dfob args body) = Vfob env args body

eval env (Dsend fn args) =
  let fnVal   = eval env fn
      argVals = map (eval env) args
  in case fnVal of
       Vbuiltin biFunc       -> biFunc argVals
       Vfob closureEnv ar bE ->
         if length args == ar
         then eval (argVals ++ closureEnv) bE
         else error "Incorrect number of arguments"
       _ -> error "Attempt to call a non-function"

eval env (Dlet x y) =
  eval (eval env x : env) y

eval env (Dfix decls body) =
  let newEnv = map (eval newEnv) decls ++ env
  in eval newEnv body

-- Toplevel
tenv0 :: TEnv
tenv0 = map (\(x,t,_) -> (x,t)) env0

venv0 :: VEnv
venv0 = map (\(_,_,v) -> v) env0

evalSexp :: Sexp -> Value
evalSexp = eval venv0 . l2d tenv0 . s2l

checkSexp :: Sexp -> Type
checkSexp = check True tenv0 . s2l

tevalSexp :: Sexp -> Either (Type, Value) String
tevalSexp se =
  let le = s2l se
  in case check True tenv0 le of
       Terror err -> Right err
       t          -> Left (t, eval venv0 (l2d tenv0 le))

run :: FilePath -> IO ()
run filename = do
  inputHandle <- openFile filename ReadMode
  hSetEncoding inputHandle utf8
  s <- hGetContents inputHandle
  let parsed = case parse pSexps filename s of
                 Left _  -> [Ssym "#<parse-error>"]
                 Right es -> es
  hPutStr stdout (show (map tevalSexp parsed))
  hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf
