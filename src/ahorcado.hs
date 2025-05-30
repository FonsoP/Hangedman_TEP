import System.IO
import Data.Char
import Data.List
import System.Directory
import System.Console.ANSI

escribir :: String -> String
escribir(palabra) = take (length palabra) (repeat '_')

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

    