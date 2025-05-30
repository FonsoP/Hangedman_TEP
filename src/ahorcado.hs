import System.IO
import Data.Char
import Data.List
import System.Directory
import System.Console.ANSI



-- crea la palabra cubierta con guiones bajos
cubrir :: String -> String
cubrir palabra = take (length palabra) (repeat '_')



-- Adivina la letra y devuelve la palabra con la letra descubierta
descubrir :: ([Int], String, Char) -> String
descubrir(indice, cubierta, letra) = descubrir(indice, take indice cubierta ++ [letra] ++ drop (indice + 1) cubierta,letra);
descubrir (0, cubierta, letra) = ""


-- Busca el índice de una letra en la palabra
buscar:: (String, Char) -> [int]
buscar(palabra, letra) =  
    [i | (i, sufijo) <- zip [0..] (tails cadena), subcadena `isPrefixOf` sufijo]
    


encontrarOcurrencias :: String -> String -> [Int]
encontrarOcurrencias subcadena cadena =
    [i | (i, sufijo) <- zip [0..] (tails cadena), subcadena `isPrefixOf` sufijo]

--muestra la letra descubierta
procesarLetra :: String -> String -> Char -> IO ()
procesarLetra palabra cubierta letra = 
    case buscar (palabra, letra) of
        Just idx -> putStrLn $ descubrir (idx, cubierta, letra)
        Nothing  -> putStrLn "La letra no está en la palabra."

mostrarMenu :: IO ()
mostrarMenu = do
    clearScreen
    putStrLn "\n----- EL AHORCADO: -----"
    putStrLn "1. Jugar Partida"
    putStrLn "2. Visualizar Estadísticas"
    putStrLn "3. Salir del Programa"
    putStrLn "Selecciona una opcion (1-3):"

jugarPartida :: IO ()
jugarPartida = do
    clearScreen
    -- lectura del archivo
    handle <- openFile "src/palabras.txt" ReadMode

    -- convertir el contenido a una lista de palabras
    contents <- hGetContents handle
    
    -- crea un arreglo con las lineas del archivo
    let palabrasList = lines  contents
    
    -- mostrar en pantalla la palabra en minusculas
    putStrLn "Dí una letra: "
    
    -- Lee la letra ingresada por el usuario
    letra <- getLine

    -- Muestra todas las palabras del archivo
    putStr $ unlines palabrasList 

    -- Muestra la primera palabra del archivo oculta con guiones bajos
    putStr $ escribir(palabrasList !! 0) ++ "\n"

    -- Muestra la letra que ingresó el usuario
    putStr letra

leerEstadisticas :: IO (Int, Int, Int)
leerEstadisticas = do
    existe <- doesFileExist "src/estadisticas.txt"
    if existe
        then do
            contenido <- readFile "src/estadisticas.txt"
            let lineas = lines contenido
            if length lineas >= 3
                then return (read (lineas !! 0), read (lineas !! 1), read (lineas !! 2))
                else return (0, 0, 0)
        else return (0, 0, 0)

visualizarEstadisticas :: IO ()
visualizarEstadisticas = do
    clearScreen
    putStrLn "=== ESTADÍSTICAS DEL JUEGO ==="
    (ganadas, perdidas, abandonadas) <- leerEstadisticas
    putStrLn $ "Partidas ganadas: " ++ show ganadas
    putStrLn $ "Partidas perdidas: " ++ show perdidas
    putStrLn $ "Partidas abandonadas: " ++ show abandonadas
    putStrLn $ "Total de partidas: " ++ show (ganadas + perdidas + abandonadas)

-- MAIN --
main :: IO ()
main = do

  -- lectura del archivo
    handle <- openFile "src/palabras.txt" ReadMode

    -- convertir el contenido a una lista de palabras
    contents <- hGetContents handle
    
    -- crea un arreglo con las lineas del archivo
    let palabrasList = map (map toLower) (lines contents)
    
    -- pone el arreglo separado por \n
    putStr $ unlines palabrasList 

    let palabraElegida =  palabrasList !! 3

    let cubierta =  cubrir palabraElegida

    let intentos = length palabraElegida

    jugar palabraElegida cubierta intentos








    putStrLn "Palabra cubierta: "
    putStrLn cubierta

    putStrLn "Dí una letra: "
    --leer de pantalla
    letra <- getLine

    procesarLetra palabraElegida cubierta (head letra)






