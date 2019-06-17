module Main where

    data Arbol a = H a
            | N a (Arbol a) (Arbol a)
            deriving (Show, Eq)

    nHojas :: Arbol x -> Int
    nHojas (H _) = 1
    nHojas (N x izquierdo derecho) = nHojas derecho + nHojas izquierdo

    preorden :: Arbol a -> [a]
    preorden (H a) = [a]
    preorden (N x izquierdo derecho) = [x] ++ preorden izquierdo ++ preorden derecho

    postorden :: Arbol a -> [a]
    postorden (H a) = [a]
    postorden (N x izquierdo derecho) = postorden izquierdo ++ [x] ++ postorden derecho

    inorden :: Arbol a -> [a]
    inorden (H a) = [a]
    inorden (N x izquierdo derecho) = inorden izquierdo ++ inorden derecho ++ [x]

    nNodos :: Arbol a -> Int
    nNodos (H a) = 0
    nNodos (N x izquierdo derecho) = 1 + nNodos izquierdo + nNodos derecho

    esbinario :: Arbol x -> Bool
    esbinario (N x izquierdo derecho) = if nNodos (N x izquierdo derecho) +1  == nHojas (N x izquierdo derecho) then True else False

    profundidad :: Arbol x -> Int
    profundidad (H _) = 0
    profundidad (N x izquierdo derecho) = 1 + max (profundidad izquierdo) (profundidad derecho)

    --funcion para validar que un arbol es binario partiendo de la profundidad del mismo
    esbinarioa :: Arbol x -> Bool
    esbinarioa x = if nNodos x == 2^(profundidad x) -1 then True else False

    espejoespecular :: Arbol a -> Arbol a
    espejoespecular (H a) = H a
    espejoespecular (N x i d) = N x (espejoespecular d) (espejoespecular i)

    takeArbol :: Int -> Arbol a -> Arbol a
    takeArbol _ (H a) = H a
    takeArbol 0 (N x i d) = H x
    takeArbol n (N x i d) = N x (takeArbol (n-1) i) (takeArbol (n-1) d)

    mapArbol :: (a->a)->Arbol a->Arbol a
    mapArbol funcion (H a) = H (funcion a)
    mapArbol funcion (N x izquierdo derecho) = N (funcion x) (mapArbol funcion izquierdo) (mapArbol funcion derecho)

    nodosxnivel :: Int -> Arbol Int -> [Int]
    nodosxnivel 0 (N x i d) = [x]
    nodosxnivel 0 (N _ i d) = nodosxnivel 0 i ++ nodosxnivel 0 d
    nodosxnivel k (N x i d) = if eshoja (i) && eshoja (d) then nodosxnivel 0 i ++ nodosxnivel 0 d else nodosxnivel (k-1) i ++ nodosxnivel (k-1) d

    
    eshoja :: Arbol a -> Bool
    eshoja (H x) = True
    eshoja (N x i d) = False


    repeatArbol :: a -> Arbol a
    repeatArbol x = N x t t
            where t = repeatArbol x