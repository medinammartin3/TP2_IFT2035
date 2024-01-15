-- Auteurs:
-- Martin Medina (20235219)
-- Etienne Mitchell-Bouchard (20243430)

-- TP-2  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Librairie d'analyse syntaxique.
import Data.Char        -- Conversion de Chars de/vers Int et autres
-- import Numeric       -- Pour la fonction showInt
import System.IO        -- Pour stdout, hPutStr
-- import Data.Maybe    -- Pour isJust and fromJust

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          | Snode Sexp [Sexp]           -- Une liste non vide
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) ==> Snode (Ssym "+")
--                   [Snum 2, Snum 3]
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Snode (Ssym "/")
--       [Snode (Ssym "*")
--              [Snode (Ssym "-")
--                     [Snum 68, Snum 32],
--               Snum 5],
--        Snum 9]

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Snode (Ssym "quote") [e]) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces;
              ses <- pTail;
                    return (case ses of [] -> Snil
                                        se : ses' -> Snode se ses')
            }
pTail :: Parser [Sexp]
pTail  = do { pChar ')'; return [] }
     -- <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
     --          pChar ')' <|> error ("Missing ')' after: " ++ show e);
     --          return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (e : es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Snode h t) =
    let showTail [] = showChar ')'
        showTail (e : es) =
            showChar ' ' . showSexp' e . showTail es
    in showChar '(' . showSexp' h . showTail t

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire L(ambda)exp(ression)                     --
---------------------------------------------------------------------------

type Var = String

data Type = Tunknown             -- Type utilisé uniquement en cas d'erreur.
          | Tint                 -- Type des nombres entiers.
          | Tbool                -- Type des booléens.
          | Tref Type            -- Type des "ref-cells" renvoyés par `ref!`.
          | Tabs Type Type       -- Type des abstractions: τ₁ → τ₂.
          deriving (Show, Eq)

data Lexp = Llit Int             -- Litéral entier.
          | Lid Var              -- Référence à une variable.
          | Ltype Lexp Type      -- Annotation de type.
          | Labs Var Lexp        -- Fonction anonyme prenant un argument.
          | Lfuncall Lexp [Lexp] -- Appel de fonction, avec arguments "curried".
          | Lmkref Lexp          -- Construire une "ref-cell".
          | Lderef Lexp          -- Chercher la valeur d'une "ref-cell".
          | Lassign Lexp Lexp    -- Changer la valeur d'une "ref-cell".
          | Lite Lexp Lexp Lexp  -- If/then/else.
          | Ldec Var Lexp Lexp   -- Déclaration locale non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lrec [(Var, Lexp)] Lexp
          deriving (Show, Eq)

-- Conversion de Sexp à Lambda --------------------------------------------

s2l :: Sexp -> Lexp
s2l (Snum n) = Llit n
s2l (Ssym s) = Lid s
s2l (Snode (Ssym ":") [e, t]) = Ltype (s2l e) (s2t t) --Implémentation de Ltype
    where s2t (Ssym "Int") = Tint
          s2t (Ssym "Bool") = Tbool
          s2t (Snode (Ssym "Ref") [t2]) = Tref (s2t t2)
          s2t (Snode t1 [Ssym "->" , t2]) = Tabs (s2t t1) (s2t t2)
          s2t (Snode t1 ts) = Tabs (s2t t1) (s2ft ts)
            where s2ft [Ssym "->", se] = s2t se
                  s2ft (se:ses) = Tabs (s2t se) (s2ft ses)
                  s2ft _ = Tunknown
          s2t _ = Tunknown
s2l (Snode (Ssym "λ") (vars:exps)) = s2args [vars] --Sucre syntaxique 3 & 4
    where s2args [] = s2l (Snode (Ssym "begin") exps)
          s2args [Ssym arg] = Labs arg (s2args [])
          s2args ((Ssym arg):args) = Labs arg (s2args args)
          s2args [Snode (Ssym arg) args] = Labs arg (s2args args)
          s2args _ = error "Déclaration de lambda inconnue"
          
s2l (Snode (Ssym "ref!") [e]) = Lmkref (s2l e)
s2l (Snode (Ssym "get!") [e]) = Lderef (s2l e)
s2l (Snode (Ssym "set!") [e1, e2]) = Lassign (s2l e1) (s2l e2)
s2l (Snode (Ssym "if") [e1, e2, e3]) = Lite (s2l e1) (s2l e2) (s2l e3)
s2l (Snode (Ssym "let") ((Ssym x):(ex:es))) = Ldec x (s2l ex) -- S.S. 5
                                         (s2l (Snode (Ssym "begin") es))
s2l (Snode (Ssym "begin") decs) = s2b decs -- Sucre syntaxique 2
    where s2b [d] = s2l d
          s2b (d:ds) = Ldec "_" (s2l d) (s2b ds)
          s2b _ = error "begin malformé"
s2l (Snode (Ssym "letrec") (decls:exps)) = Lrec (s2decs decls) -- S.S. 6
                                           (s2l (Snode (Ssym "begin") exps))
    where s2decs Snil = []
          s2decs (Snode e1 es) = map s2dec (e1 : es)
          s2decs s = error ("Déclaration inconnue: " ++ showSexp s)
          s2dec (Snode (Ssym x) [e2]) = (x, s2l e2)
          s2dec (Snode (Snode (Ssym name) (Snode arg1 [t1]:args)) (rT:lexps)) = 
            (name, s2l (Snode (Ssym ":") [Snode (Ssym "λ") -- S.S λs letrec (7)
            (Snode arg1 (argSexp args):lexps), Snode t1 (typeSexp args)])) 
            where argSexp [] = []
                  argSexp ((Snode a _):as) = a:argSexp as
                  argSexp _ = error "Mauvais nom de args de lambda"
                  typeSexp [] = [Ssym "->", rT]
                  typeSexp ((Snode _ [t]):ts) = t:typeSexp ts
                  typeSexp _ = error "Mauvais type de args de lambda"
                  
          s2dec s = error ("Déclaration inconnue: " ++ showSexp s)
s2l (Snode e es) = Lfuncall (s2l e) (map s2l es)
s2l se = error ("Expression Slip inconnue: " ++ (showSexp se))

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Représentation du "tas" ------------------------------------------------

-- Notre tas est représenté par une arbre binaire de type "trie".
-- La position du nœud qui contient l'info pour l'addresse `p` est
-- déterminée par la séquence de bits dans la représentation binaire de `p`.

data Heap = Hempty | Hnode (Maybe Value) Heap Heap

hlookup :: Heap -> Int -> Maybe Value
hlookup Hempty _ = Nothing
hlookup (Hnode mv _ _) 0 = mv
hlookup _ p | p < 0 = error "hlookup sur une adresse négative"
hlookup (Hnode _ e o) p = hlookup (if p `mod` 2 == 0 then e else o) (p `div` 2)

hinsert :: Heap -> Int -> Value -> Heap
hinsert Hempty p v = hinsert (Hnode Nothing Hempty Hempty) p v
hinsert (Hnode _ e o) 0 v = Hnode (Just v) e o
hinsert _ p _ | p < 0 = error "hinsert sur une adresse négative"
hinsert (Hnode mv e o) p v
        | p `mod` 2 == 0 = Hnode mv (hinsert e (p `div` 2) v) o
        | otherwise = Hnode mv e (hinsert o (p `div` 2) v)

-- Représentation de l'environnement --------------------------------------

-- Type des tables indexées par des `α` et qui contiennent des `β`.
-- Il y a de bien meilleurs choix qu'une liste de paires, mais
-- ça suffit pour notre prototype.
type Map α β = [(α, β)]

-- Transforme une `Map` en une fonctions (qui est aussi une sorte de "Map").
mlookup :: Map Var β -> (Var -> β)
mlookup [] x = error ("Variable inconnue: " ++ show x)
mlookup ((x,v) : xs) x' = if x == x' then v else mlookup xs x'

madd :: Map Var β -> Var -> β -> Map Var β
madd m x v = (x,v) : m

-- On représente l'état de notre mémoire avec non seulement le "tas" mais aussi
-- avec un compteur d'objets de manière a pouvoir créer une "nouvelle" addresse
-- (pour `ref!`) simplement en incrémentant ce compteur.
type LState = (Heap, Int)

-- Type des valeurs manipulée à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vref Int
           | Vfun ((LState, Value) -> (LState, Value))

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _p (Vref p) = (\s -> "ptr<" ++ show p ++ ">" ++ s)
    showsPrec _ _ = showString "<function>"

type Env = [(Var, Value, Type)]
type TEnv = Map Var Type
type VEnv = Map Var Value

-- L'environnement initial qui contient les fonctions prédéfinies.

env0 :: Env
env0 = let binop :: (Value -> Value -> Value) -> Value
           binop op = Vfun (\ (s1, v1)
                            -> (s1, Vfun (\ (s2, v2)
                                         -> (s2, v1 `op` v2))))

           biniiv :: (Int -> Int -> Value) -> Value
           biniiv op = binop (\ v1 v2
                              -> case (v1, v2) of
                                  (Vnum x, Vnum y) -> x `op` y
                                  _ -> error ("Pas des entiers: "
                                             ++ show v1 ++ "," ++ show v2))

           binii wrap f = biniiv (\ x y -> wrap (f x y))

           tiii = (Tabs Tint (Tabs Tint Tint))
           tiib = (Tabs Tint (Tabs Tint Tbool))

       in [("+", binii Vnum (+), tiii),
           ("*", binii Vnum (*), tiii),
           ("/", binii Vnum div, tiii),
           ("-", binii Vnum (-), tiii),
           ("true", Vbool True, Tbool),
           ("false", Vbool False, Tbool),
           ("<", binii Vbool (<), tiib),
           (">", binii Vbool (>), tiib),
           ("=", binii Vbool (==), tiib),
           (">=", binii Vbool (>=), tiib),
           ("<=", binii Vbool (<=), tiib)]

venv0 :: VEnv
venv0 = map (\(x,v,_t) -> (x,v)) env0

tenv0 :: TEnv
tenv0 = map (\(x,_v,t) -> (x,t)) env0

---------------------------------------------------------------------------
-- Vérification des types                                                --
---------------------------------------------------------------------------

type TErrors = [String]

-- `check Γ e τ` vérifie que `e` a type `τ` dans l'environnment `Γ`.
check :: TEnv -> Lexp -> Type -> TErrors
check env (Lite ec et ee) t =
    check env ec Tbool
    ++ check env et t
    ++ check env ee t
-- Début de notre code --
check env (Lid var) t  
    | varT == t = []
    | otherwise = ["Variable \"" ++ var ++ "\" a le type " ++ show varT ++ 
    " qui n'est pas le type attendu " ++ show t ++ "; "]
    where varT = mlookup env var

check env (Labs arg body) (Tabs argT (Tabs t ts)) = 
    check (madd env arg argT) body (Tabs t ts)

check env (Labs arg body) (Tabs argT returnT) 
    | bT == returnT = []
    | otherwise = addErrors ["Mauvaise déclaration de lambda: Le type de retour\ 
                    \ " ++ show bT ++ " n'est pas le même que celui déclaré "++ 
                    show  returnT ++ "; "] bE
    where bT = fst bS
          bE = snd bS
          bS = synth (madd env arg argT) body

-- Fin de notre code

check env e t =
    let (t', errors) = synth env e
    -- Si `synth` renvoie `Tunknown` il doit y avoir des erreurs.
    in if t == t' || t' == Tunknown then errors
       else ("Expression a type " ++ show t'
            ++ " au lieu de " ++ show t ++ "; ") : errors



-- `synth Γ e` synthétise le type `τ` de `e` dans l'environnment `Γ`.
synth :: TEnv -> Lexp -> (Type, TErrors)
synth env (Lid x) = (mlookup env x, []) 
synth env (Ltype e t) = (t, check env e t)
-- Début de notre code --
synth _ (Llit _) = (Tint, [])

synth env (Lmkref e) 
    | null errors = (Tref eT, [])
    | otherwise = (Tunknown, addErrors ["ref! invalide: "] errors)
    where errors = snd eS
          eS = synth env e 
          eT = fst eS

synth env (Lderef e) =
    case eT of
        Tref t -> (t, [])
        _ -> (Tunknown, addErrors ["get! sur un " ++ show eT ++ "; "] errors) 
    where errors = snd eS
          eT = fst eS
          eS = synth env e

synth env (Lassign var e) 
    | null errors = (varT, [])
    | otherwise = (Tunknown, addErrors ["set! invalide: "] errors)
    where errors = addErrors varE eS
          varS = synth env var
          varT = case fst varS of 
                    Tref t -> t
                    _ -> Tunknown
          varE = snd varS 
          eS = check env e varT

synth env (Lite c e1 e2) 
    | null errors = (e1T, [])
    | otherwise = (Tunknown, addErrors ["if invalide: "] errors )
    where errors = addErrors (check env (Lite c e1 e2) e1T) e1E
          e1S = synth env e1
          e1T = fst e1S
          e1E = snd e1S

synth env (Lfuncall func args)
    | null errors = (funcReturnType fT, []) 
    | otherwise = (Tunknown, addErrors ["Mauvais appel de fonction: "] errors)
        where errors = addErrors (checkInputTypes args 
                (funcArgsType fT)) fE
              fT = fst fS
              fE = snd fS
              fS = synth env func
              checkInputTypes (l:ls) (t:ts) = check env l t ++ 
                                              checkInputTypes ls ts
              checkInputTypes _ _ = []
              funcArgsType (Tabs t0 t1) = t0:funcArgsType t1
              funcArgsType _ = []
              funcReturnType (Tabs _ t) = funcReturnType t
              funcReturnType t = t

synth env (Ldec var t e) 
    | null errors = (eT, [])
    | otherwise = (Tunknown, addErrors ["let invalide: "] errors)
    where errors = addErrors varE eE
          eS = synth env' e
          eT = fst eS
          eE = snd eS
          env' = madd env var varT
          varS = synth env t 
          varT = fst varS
          varE = snd varS 

synth env (Lrec decs e) = let (envn, errors) = foldl (\ (envi, er) (x, ex) -> 
                               let (ti, eri) = synth envn ex
                               in (madd envi x ti, addErrors er eri)) 
                               (env, []) decs
    in case errors of 
        [] -> case synth envn e of
            (t, []) -> (t, [])
            (_, es) -> (Tunknown, addErrors ["Expr finale du letrec invalide: "]
                                            es)
        de -> (Tunknown, addErrors ["Déclarations du letrec invalide: "] de)

--Le seul cas non géré est Labs qui retourne un message d'erreur spécial
synth _ _ = (Tunknown, ["Vous ne pouvez pas déclarer de lambdas sans \":\"; "])


addErrors :: TErrors -> TErrors -> TErrors
addErrors [] [] = []
addErrors e1 e2 = [concat e1 ++ concat e2]

-- Fin de notre code --

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

state0 :: LState
state0 = (Hempty, 0)

eval :: LState -> VEnv -> Lexp -> (LState, Value)
eval s _env (Llit n) = (s, Vnum n)
eval s env (Lid x) = (s, mlookup env x)
eval s env (Ltype e _t) = eval s env e
eval s env (Labs x e) = (s, Vfun (\ (s1, v) -> eval s1 (madd env x v) e))
eval s env (Lfuncall e []) = eval s env e
eval s env (Lfuncall f [arg]) =
    let (s1, fv) = eval s env f
        (s2, argv) = eval s1 env arg
    in case fv of
         Vfun op -> op (s2, argv)
         _ -> error ("Pas une fonction: " ++ show fv)
eval s env (Lfuncall f (arg1 : args)) =
    eval s env (Lfuncall (Lfuncall f [arg1]) args)
eval s env (Lmkref e) =
    let ((h, p), v) = eval s env e
    in ((hinsert h p v, p + 1), Vref p)
eval s env (Lderef e) =
    case eval s env e of
      (s1, Vref p) -> case hlookup (fst s1) p of
                       Just v -> (s1, v)
                       _ -> error ("Pointeur fou: " ++ show p)
      (_, v) -> error ("Pas un pointeur: " ++ show v)
eval s env (Lassign e1 e2) =
    case eval s env e1 of
      (s1, Vref p) -> let ((h, pmax), v2) = eval s1 env e2
                     in ((hinsert h p v2, pmax), v2)
      (_, v) -> error ("Pas un pointeur: " ++ show v)
eval s env (Lite e1 e2 e3) =
    case eval s env e1 of
      (s1, Vbool b) -> eval s1 env (if b then e2 else e3)
      (_, v) -> error ("Pas un booléen: " ++ show v)
eval s env (Ldec x e1 e2) =
    let (s1, v1) = eval s env e1
    in eval s1 (madd env x v1) e2
eval s env (Lrec decls body) =
    let (sn, envn) = foldl (\ (si, envi) (x, e)
                            -> let (si1, vi) = eval si envn e
                              in (si1, madd envi x vi))
                           (s, env) decls
    in eval sn envn body


---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = snd . eval state0 venv0 . s2l

tevalSexp :: Sexp -> Either (Value, Type) TErrors
tevalSexp se = let e = s2l se
               in case synth tenv0 e of
                    (t, []) ->
                        let (_, v) = eval state0 venv0 e
                        in Left (v, t)
                    (_t, errors) -> Right errors

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et imprime les valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    let sexps s' = case parse pSexps filename s' of
                     Left _ -> [Ssym "#<parse-error>"]
                     Right es -> es
        printevalone s =
            case tevalSexp s of
              Left (e, t) ->
                  do hPutStr stdout (show e)
                     hPutStr stdout " : "
                     hPutStr stdout (show t)
                     hPutStr stdout "\n"
              Right [] -> hPutStr stdout "ERREUR: PAS D'ERREUR??"
              Right (err1:_) ->
                  do hPutStr stdout "ERREUR: "
                     hPutStr stdout err1
                     hPutStr stdout "\n"
    in do inputHandle <- openFile filename ReadMode 
          hSetEncoding inputHandle utf8
          s <- hGetContents inputHandle
          mapM_ printevalone (sexps s)
          hClose inputHandle
           

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf