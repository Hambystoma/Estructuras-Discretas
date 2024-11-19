import Distribution.SPDX (LicenseId(MakeIndex))
import Distribution.ModuleName (main)
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
conjunto (x:xs)=if estaContenido x (x:xs)
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
interpretacion :: Formula -> [(Var,Bool)] -> Bool
interpretacion _ [] = error "No todas las variables estan definidas."
interpretacion (Neg (Atom variable)) ((var1, estado1):xs) = if variable == var1
                                                          then not estado1
                                                          else interpretacion (Neg (Atom variable)) xs
interpretacion (Neg f1) lista = not (interpretacion f1 lista)
interpretacion (Atom variable) ((var1, estado1):xs) = if variable == var1
                                                      then estado1
                                                      else interpretacion (Atom variable) xs
interpretacion (f1:&:f2) lista = interpretacion f1 lista && interpretacion f2 lista
interpretacion (f1:|:f2) lista = interpretacion f1 lista || interpretacion f2 lista
interpretacion (f1:=>:f2) lista = interpretacion f1 lista || not (interpretacion f2 lista)
interpretacion (f1:<=>:f2) lista = interpretacion (f1 :=>: f2) lista && interpretacion (f2 :=>: f1) lista

-----------------------------------------------------

-------------------- EJERCICIO 5 --------------------
combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones (Atom var) = [[(var,True),(var, False)]]
combinaciones (Neg f1) = combinaciones f1
combinaciones (f1:&:f2) = undefined

combinacionesbooleanas :: [[Bool]] -> Bool -> [[Bool]]
combinacionesbooleanas [listabool] bool2 = [bool2:listabool]


-----------------------------------------------------

-------------------- EJERCICIO 6 --------------------

tablaDeVerdad :: Formula -> [([(Var,Bool)],Bool)]
tablaDeVerdad _ = undefined
----------------------------------------------------- 