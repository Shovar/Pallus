--Implementacion del lenguaje de programación PALLUS


----------------------------------------------------------------------------------- Imports 


{-# LANGUAGE RecordWildCards #-}   

import Data.Char (isUpper)
import Data.List (nub, isInfixOf) 
import Data.List.Split (splitOn)   
import Data.String.Utils (strip)    
import Data.Maybe (mapMaybe, fromMaybe) 


----------------------------------------------------------------------------------- Definiciones de los datos


type Programa = [Regla]

type BaseConeixement = [ Atom ]  --Todos sus Atoms son de tipo ground

type Sustitucio = [ (Term, Term) ]     -- [(variable, constant), (variable, constant), ...]

data Regla = Regla{ _cap::Atom, _cos::[Atom]}

data Atom = Atom{ _nomPredicat::String, _termes::[Term]}
    deriving (Eq,Show)
    
data Term = Var String | Sym String
    deriving (Eq,Show)

-- Muestra las reglas  con la llamada show, lo he utilizado para comparar mi resultado del parsing con el ejemplo del enunciado
instance Show Regla where 
    show (Regla{ _cap = x, _cos = y}) = "Regla { _cap = " ++ (show x) ++ " _cos = " ++ (show y) ++ "}\n"   
-----------------------------------------------------------------------------------


------------------------------------------------------------------------------------ Parsing


-- Traduce la entrada en PALLUS a Haskell
parseText :: String -> Programa
parseText text = init $ map (createRule . lineToAntCons) (textToLines text) -- Uso init por que al dividir por "." al final queda un conjunto vacío que no se usará

-- Separa la entrada en las diferentes lineas que la forman
textToLines :: String -> [String]
textToLines text = map(strip) (splitOn "." text)  

--Divide una linea en los consecuentes y los antecedentes. Nota: Para poder usar splitOn trato el resultado como un [Strings] aun que siemple va a ser de tamaño 1 o 2
lineToAntCons :: String -> [String]
lineToAntCons l = map (strip) ((reverse . splitOn "=>") l)
        
-- Crea una regla para cada linea de la entrada. Si la regla no tiene antecendentes (realmente es un hecho), su cos_ es [] 
createRule :: [String] -> Regla
createRule c = Regla { _cap = ca, _cos = co}
    where 
        ca = createAtom $ words $ head c
        --Si hay hay mas de 1 string en c quiere decir que hay antecedentes y por lo tanto es una regla, si no hay es un hecho
        co =  if(length c > 1) then map (createAtom . words) (chopAnt ( last c))
        else  []
                              
-- Separa los antecesores para poder crear atomos individuales de cada uno
chopAnt :: String -> [String]
chopAnt s = map (strip) (splitOn "&" s)

--Dado el [String] que representa un Atom lo transforma en un objeto de tipo Atom
createAtom :: [String] -> Atom 
createAtom [] = Atom{_nomPredicat = [] ,_termes = []}
createAtom (n:ns) = Atom{_nomPredicat = nom ,_termes = t}
    where
        nom  = n
        t  = map (stringToTerm) ns

--Transforma un String en un Term. Sera de tipo Var si empieza con letra mayuscula o tipo Sym si empieza en minúscula 
stringToTerm :: String -> Term
stringToTerm s 
    | isUpper (head (strip s)) =  Var s -- Comprueva si empieza por mayúscula
    | otherwise = Sym s
    

-----------------------------------------------------------------------------------

    
------------------------------------------------------------------------------------ Evaluar Regla


--Utiliza un programa y los atomos ground ya encontrados para encontrar los que faltan 
consequencia :: Programa -> BaseConeixement -> BaseConeixement
consequencia [] kb = kb
consequencia (p:ps) kb  = nub ((avaluaRegla kb p) ++ (consequencia ps kb))

-- Dado un conjunto de Atoms selecciona aquellos que son Ground por que en la Base de conocimiento no se distinguen los Atomos de los Atomos Ground
eliminaNotGround :: BaseConeixement -> BaseConeixement -> BaseConeixement
eliminaNotGround [] s = s
eliminaNotGround (a:as) s
    | isGround (_termes a) = eliminaNotGround as (s ++ [a])
    |otherwise = eliminaNotGround as s

--Dado un Term devuelve true si este es Ground
isGround :: [Term] -> Bool
isGround [] = True
isGround (t:ts)
    | isVar t = False
    | otherwise = (True && isGround ts)
    
    
-- Da los Atoms ground que pueden generarse a partir de la regla 
avaluaRegla :: BaseConeixement -> Regla -> BaseConeixement
avaluaRegla kb r
    | _cos r == [] = [_cap r] -- La regla es un fet
    | otherwise = eliminaNotGround (nub $ map (sustitueix (_cap r) ) ( posiblesSustitucions (_cos r) kb [sustitucioBuida]) )  []
    
    
    
-- Dado el cuerpo de una regla, una base de conocimiento y un conjunto de posibles susticiones devuelve el conjunto de posibles sustituciones obtenidas de los atomos de esa regla    
posiblesSustitucions :: [Atom] -> BaseConeixement -> [Sustitucio] -> [Sustitucio]
posiblesSustitucions [] kb s = s
posiblesSustitucions (r:rs) kb s =nub (posiblesSustitucions rs kb (avaluaAtom kb r s) ) 


-----------------------------------------------------------------------------------

        
------------------------------------------------------------------------------------ Evaluar Atom   

        
--Coge una sustitucion de la lista, la ejecuta sobre el Atom y el resultado trata de unificarlos con los Atoms de la base de conocimiento 
avaluaAtom :: BaseConeixement -> Atom -> [ Sustitucio ] -> [ Sustitucio ]
avaluaAtom kb atom [] = (mapMaybe (unifica atom)  kb) -- Al ser la primera vez que entra debe generar todas las sustituciones posibles para los demás átomos
avaluaAtom kb atom l = avaluaAtomAux kb atom l --Aquí entrarán los demás átomos de la regla si hay

--Coge una sustitucion de la lista, la ejecuta sobre el Atom y el resultado trata de unificarlos con los Atoms de la base de conocimiento 
avaluaAtomAux :: BaseConeixement -> Atom -> [ Sustitucio ] -> [ Sustitucio ]
avaluaAtomAux kb atom [] = []
avaluaAtomAux kb atom (l:ls) = ( map (l ++) (mapMaybe (unifica $ sustitueix atom l) kb) ++ avaluaAtomAux kb atom ls) 


-----------------------------------------------------------------------------------

    
------------------------------------------------------------------------------------ Sustitucion


sustitucioBuida :: Sustitucio
sustitucioBuida = []

--Sustituye el termino (Var t) por el termino (Sym y) de la sustitucion (x,y) que le corresponde
sustituexTerme :: Sustitucio -> Term -> Term
sustituexTerme [] t = t
sustituexTerme ((x,y):ss) t
    | t == x = y
    | otherwise = sustituexTerme ss t 
                      
--Sustituye un átomo que contiene algún termino Var a su versión con los terminos Var sustituidos por sus correspondientes términos Sym                    
sustitueix :: Atom -> Sustitucio -> Atom
sustitueix at [] = at
sustitueix at s = Atom{_nomPredicat = nom ,_termes = t}
    where 
        t :: [Term]
        nom = _nomPredicat at
        t = map (sustituexTerme s) (_termes at) 

        
-----------------------------------------------------------------------------------

        
------------------------------------------------------------------------------------ Unificacion


--Comprueba que un Var no tenga el valor de 2 constantes diferentes. Si no hay ninguna sustitucion todavía devuelve true
notSameConstDiffVal :: Term -> Term -> Sustitucio -> Bool
notSameConstDiffVal t1 t2 [] = True
notSameConstDiffVal t1 t2 ((x,y):ss)
    | (x == t1 && y /= t2 )= False
    | otherwise = notSameConstDiffVal t1 t2 ss

--Retorna true si el termino a evaluar es un Var
isVar :: Term -> Bool
isVar (Var v) = True
isVar (Sym v) = False
  
--Comprueba que todos los terminos de un atomo sea puedan unificar y devuelve la sustitucion que se saca de estos  
unificable :: [Term] -> [Term] -> Sustitucio -> Maybe Sustitucio
unificable [] [] s = Just s --Cuando no hay más terminos a evaluar devuele las sustituciones que se han conseguido de estos terminos
unificable (t1:ts) (t2:ls) s
    | not(isVar t1) && (t1 == t2) = (unificable ts ls s) --Si el termino de la izq es un Sym, solo será unificable si es el mismo que el de la derecha
    | (isVar t1) && (notSameConstDiffVal t1 t2 s) = ( unificable ts ls (s ++ [(t1,t2)] ) ) --Si el término de la izq es un Var y no tiene el valor de 2 constantes diferentes, entonces (t1,t2) es una sustitución posible
    | otherwise = Nothing

--Retorna una (Just) sustitucion si se pueden unificar atom1 y atom2 o Nothing si no es posible
unifica :: Atom -> Atom -> Maybe Sustitucio
unifica atom1 atom2 
    |(_nomPredicat atom1 == _nomPredicat atom2) = (unificable (_termes atom1) (_termes atom2) sustitucioBuida) --Paso como parámetro sustitucioBuida por que en la función unificable la iré completando con las posibles sustituciones de los terminos de los atomos
    | otherwise = Nothing

    
-----------------------------------------------------------------------------------

    
------------------------------------------------------------------------------------ Auxiliares del Main


-- Unifica la cabeza de la query con la base de conocimiento para obtener las posibles susticiones 
findValues ::  Atom -> Atom -> Maybe Sustitucio
findValues a kb
    | (_nomPredicat kb) == "query" = (unifica a kb)
    | otherwise = Nothing


--Dada una query devuelve true si es de respuesta Booleana
isBoolean :: Atom -> Bool
isBoolean a
    | _termes a == [] = True
    | otherwise = False
    
--Dada una query booleana y una base de conocimiento comprueba si la query pertenece a la base de conocimiento 
isTrue :: [Atom] -> [Atom] -> Bool
isTrue a kb = and(map(\i -> (isInfixOf [i] kb) ) a) -- Busco si el/los atomos de la query estan en la base de conocimiento


--Dadas dos bases de conocimiento kbi y kbf , y un programa(reglas + query) itera hasta encontrar la kbf == kbi. Con esto podré encontrar la base de conocimiento que abarca todo el problema
findKb :: BaseConeixement -> BaseConeixement -> Programa -> BaseConeixement
findKb kbi kbf p
    | and(map (\i -> (isInfixOf [i] kbi) ) kbf) = kbi --Sabiendo que kbf debe contener los elementos de kbi ,ya que es la base de conocimiento sacada de inferir kbi y el programa, si kbf es un subconjunto de kbi entonces kbf es igual a kbi y ya hemos encontrado la base de conocimiento que abarca todo el problema
    | otherwise = findKb kbf (consequencia p kbf) p


--Recorre todas las queries, crea una base de conocimiento para cada una y da una salida para cada una. Esta función es necesaria para poder iterar sobre las queries y generar respuestas individuales para cada una
compute :: Programa -> Programa -> IO ()
compute p [] = return ()
compute p (q:qs) = do
    let kb0 = []
    let kb1 = consequencia (p ++ [q]) kb0 
    let kb  = findKb kb0 kb1 (p ++ [q])
    if (isBoolean (_cap q)) then if(isTrue (_cos q) kb ) then putStrLn $ show "True"
                              else putStrLn $ show "False"
                                          else putStrLn $ show (mapMaybe(findValues (_cap q)) kb)
    compute p qs
         

-----------------------------------------------------------------------------------


------------------------------------------------------------------------------------ Main


main :: IO ()
main = do
    contents <- getContents -- Leo toda la entrada  
    let content = map (strip)(splitOn "end." contents) -- El splitOn genererá 3 arrays dentro de la entrada, en el primero estará el programa, en el segundo las queries y el último será un conjunto vacío
    let programa = parseText $ head content
    let queries = parseText (content !! 1) 
    compute programa queries
