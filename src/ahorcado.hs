import System.IO
import Data.Char
import Data.List
import System.Directory
import System.Console.ANSI

-- crea la palabra cubierta con guiones bajos
cubrir :: String -> String
cubrir palabra = take (length palabra) (repeat '_')

descubrir :: (Int, String, Char) -> String
descubrir (indice, cubierta, letra) =  take indice cubierta ++ [letra] ++ drop (indice + 1) cubierta;
        

buscar:: (String, Char) -> Maybe Int
buscar(palabra, letra) = elemIndex letra palabra
    

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

visualizarEstadisticas :: IO ()
visualizarEstadisticas = do
    clearScreen
    putStrLn "--- ESTADÍSTICAS ---"

-- MAIN --
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


    let loop = do
        mostrarMenu
        opcion <- getLine
        case opcion of
            "1" -> do
                jugarPartida
                putStrLn "\nPresione Enter para continuar..."
                _ <- getLine
                loop
            "2" -> do
                visualizarEstadisticas
                putStrLn "\nPresione Enter para continuar..."
                _ <- getLine
                loop
            "3" -> putStrLn "Gracias por jugar!"
            _ -> do
                putStrLn "Opción no válida. Por favor seleccione un número del 1 al 3."
                putStrLn "Presione Enter para continuar..."
                _ <- getLine
                loop
    loop



  
