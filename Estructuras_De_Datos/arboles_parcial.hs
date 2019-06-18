module Arbol_parcial where
    data Arbin a = Nodo a (Arbin a) (Arbin a)
                | Vacio
                deriving (Show, Eq)

    obtenerNivel :: Arbin a -> Int -> [a]
    obtenerNivel (Nodo x izquierdo derecho) 1 = [x]
    obtenerNivel Vacio 1 = []
    obtenerNivel Vacio k = []
    obtenerNivel (Nodo key izquierdo derecho) n = obtenerNivel izquierdo (n-1) ++ obtenerNivel derecho (n-1)
