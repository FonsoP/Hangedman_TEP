import System.IO
import Data.Char
import Data.List

-- Crea la palabra cubierta con guiones bajos
cubrir :: String -> String
cubrir palabra = replicate (length palabra) '_'

-- Actualiza la cubierta con la letra adivinada
descubrir :: (Int, String, Char) -> String
descubrir (indice, cubierta, letra) = take indice cubierta ++ [letra] ++ drop (indice + 1) cubierta

-- Busca el índice de una letra en la palabra
buscar :: (String, Char) -> Maybe Int
buscar (palabra, letra) = elemIndex letra palabra

-- Procesa una letra ingresada por el jugador
procesarLetra :: String -> String -> Char -> IO String
procesarLetra palabra cubierta letra = 
    case buscar (palabra, letra) of
        Just idx -> do
            let nuevaCubierta = descubrir (idx, cubierta, letra)
            putStrLn $ "Cubierta actualizada: " ++ nuevaCubierta
            return nuevaCubierta
        Nothing -> do
            putStrLn "La letra no está en la palabra."
            return cubierta

-- Juego principal
jugarAhorcado :: String -> String -> Int -> IO ()
jugarAhorcado palabra cubierta intentos = do
    if intentos == 0 then do
        putStrLn "¡Has perdido! La palabra era:"
        putStrLn palabra
    else if palabra == cubierta then do
        putStrLn "¡Has ganado! La palabra es:"
        putStrLn palabra
    else do
        putStrLn $ "Intentos restantes: " ++ show intentos
        putStrLn $ "Palabra cubierta: " ++ cubierta
        putStrLn "Dí una letra: "
        letra <- getLine
        let letraChar = toLower (head letra)
        nuevaCubierta <- procesarLetra palabra cubierta letraChar
        if nuevaCubierta == cubierta then
            jugarAhorcado palabra cubierta (intentos - 1)
        else
            jugarAhorcado palabra nuevaCubierta intentos

-- Main
main :: IO ()
main = do
    let palabra = "ahorcado"
    let cubierta = cubrir palabra
    let intentos = 6
    jugarAhorcado palabra cubierta intentos