import System.IO
import Data.Char
import Data.List
import System.Directory
import System.Process
import System.Info

escribir :: String -> String
escribir palabra = replicate (length palabra) '_'

limpiarPantalla :: IO ()
limpiarPantalla = do
    if os == "mingw32" || os == "win32"
        then do
            _ <- system "cls"
            return ()
        else do
            _ <- system "clear"
            return ()

mostrarMenu :: IO ()
mostrarMenu = do
    limpiarPantalla
    putStrLn "----- EL AHORCADO: -----"
    putStrLn "1. Jugar Partida"
    putStrLn "2. Visualizar Estadísticas"
    putStrLn "3. Salir del Programa"
    putStrLn "Selecciona una opcion (1-3):"

jugarPartida :: IO ()
jugarPartida = do
    limpiarPantalla
    -- lectura del archivo
    handle <- openFile "src/palabras.txt" ReadMode

    -- convertir el contenido a una lista de palabras
    contents <- hGetContents handle
    
    -- crea un arreglo con las lineas del archivo
    let palabrasList = lines contents
    
    -- mostrar en pantalla la palabra en minusculas
    putStrLn "Dí una letra: "
    
    -- Lee la letra ingresada por el usuario
    letra <- getLine

    -- Muestra todas las palabras del archivo
    putStr $ unlines palabrasList 

    -- Muestra la primera palabra del archivo oculta con guiones bajos
    putStr $ escribir(head palabrasList)

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
                then return (read (head lineas), read (lineas !! 1), read (lineas !! 2))
                else return (0, 0, 0)
        else return (0, 0, 0)

visualizarEstadisticas :: IO ()
visualizarEstadisticas = do
    limpiarPantalla
    putStrLn "--- ESTADÍSTICAS DEL JUEGO ---"
    (ganadas, perdidas, abandonadas) <- leerEstadisticas
    putStrLn $ "Partidas ganadas: " ++ show ganadas
    putStrLn $ "Partidas perdidas: " ++ show perdidas
    putStrLn $ "Partidas abandonadas: " ++ show abandonadas
    putStrLn $ "Total de partidas: " ++ show (ganadas + perdidas + abandonadas)

main :: IO ()
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
                "3" -> do
                    limpiarPantalla
                    putStrLn "¡Gracias por jugar!"
                    putStrLn "Presione Enter para salir..."
                    _ <- getLine
                    return ()
                _ -> do
                    putStrLn "\n❌ Opción no válida. Por favor seleccione un número del 1 al 3."
                    putStrLn "Presione Enter para continuar..."
                    _ <- getLine
                    loop
    loop

    