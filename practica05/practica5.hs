import Control.Monad (forM)
data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

data Formula = Atom Var
               |Neg Formula
               |Formula :&: Formula
               |Formula :|: Formula
               |Formula :=>: Formula
               |Formula :<=>: Formula deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

-------------------- EJERCICIO 1 --------------------
variables :: Formula -> [Var]
variables (Atom var) = [var]
variables (Neg f) = variables f
variables (f1 :&: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :|: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :=>: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :<=>: f2) = conjunto (variables f1 ++ variables f2)

conjunto :: Eq a => [a] -> [a]
conjunto []=[]
conjunto (x:xs)=if estaContenido x xs
                then conjunto xs
                else x:conjunto xs

estaContenido :: Eq a => a -> [a] -> Bool
estaContenido elem [] = False
estaContenido elem (x:xs) = if elem == x
                            then True
                            else estaContenido elem xs

------------------------------------------------------------------------- EJERCICIO 2 --------------------
negacion :: Formula -> Formula
negacion (Atom var) = Neg (Atom var)
negacion (Neg f1) = f1
negacion (f1 :&: f2) = negacion f1 :|: negacion f2
negacion (f1 :|: f2) = negacion f1 :&: negacion f2
negacion (f1 :=>: f2) = f1:&:negacion f2
negacion (f1:<=>:f2) = (negacion f1 :&: f2) :|: (f1 :&: negacion f2)

-----------------------------------------------------

-------------------- EJERCICIO 3 --------------------
equivalencia :: Formula -> Formula
equivalencia (Atom var) = Atom var
equivalencia (Neg (Atom var)) = Neg (Atom var)
equivalencia (Neg f1) = equivalencia (negacion f1)
equivalencia (f1 :&: f2) = equivalencia f1 :&: equivalencia f2
equivalencia (f1 :|: f2) = equivalencia f1 :|: equivalencia f2
equivalencia (f1 :=>: f2) = equivalencia (f1 :|: negacion f2)
equivalencia (f1 :<=>: f2) = equivalencia (f1:=>:f2):&:equivalencia (f2:=>:f1)

-----------------------------------------------------

-------------------- EJERCICIO 4 --------------------
-- Función para evaluar la fórmula con la lista de variablesd
interpretaciones :: Formula -> [(Var, Bool)] -> Bool
interpretaciones = undefined


-----------------------------------------------------

-------------------- EJERCICIO 5 --------------------
combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones formula = asignacionLista (variables formula) (listaCombinaciones [[True],[False]] (longitud (variables formula)-1))

combinacionesbooleanas :: [[Bool]] -> Bool -> [[Bool]]
combinacionesbooleanas [listabool] bool2 = [listabool++[bool2]]
combinacionesbooleanas (b:bs) bool2 = combinacionesbooleanas [b] bool2 ++ combinacionesbooleanas bs bool2

listaCombinaciones :: [[Bool]] -> Int -> [[Bool]]
listaCombinaciones booleanos 0 = booleanos
listaCombinaciones booleanos n = listaCombinaciones (combinacionesbooleanas booleanos True ++ combinacionesbooleanas booleanos False)  (n-1)

asignarValor :: [Var] -> [Bool] -> [(Var,Bool)]
asignarValor [] _ = error "Se vaciaron las vairables"
asignarValor _ [] = error "Se vaciaron los booleanos"
asignarValor [var] [bool] = [(var,bool)]
asignarValor (v:vs) (b:bs) = asignarValor [v] [b] ++ asignarValor vs bs

asignacionLista :: [Var] -> [[Bool]] -> [[(Var,Bool)]]
asignacionLista var [bool] = [asignarValor var bool]
asignacionLista var (b:bs) = asignarValor var b : asignacionLista var bs

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) =1+longitud xs 
-----------------------------------------------------

-------------------- EJERCICIO 6 --------------------

tablaDeVerdad :: Formula -> [([(Var,Bool)],Bool)]
tablaDeVerdad _ = undefined
----------------------------------------------------- 