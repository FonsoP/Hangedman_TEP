import System.IO
import Data.Char
import Data.List
import System.Directory
import System.Process
import System.Info

escribir :: String -> String
escribir palabra = replicate (length palabra) '_'

revelarLetra :: String -> String -> Char -> String
revelarLetra palabra actual letra = 
    zipWith (\p a -> if toLower p == toLower letra then p else a) palabra actual

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
    handle <- openFile "palabras.txt" ReadMode
    contents <- hGetContents handle
    let palabrasList = lines contents
    let palabra = head palabrasList
    let palabraOculta = escribir palabra
    let intentos = 6
    let letrasUsadas = []
    
    putStrLn "¡Bienvenido al juego del ahorcado!"
    putStrLn $ "La palabra tiene " ++ show (length palabra) ++ " letras"
    putStrLn $ "Palabra: " ++ palabraOculta
    putStrLn $ "Intentos restantes: " ++ show intentos
    
    jugarTurno palabra palabraOculta intentos letrasUsadas

jugarTurno :: String -> String -> Int -> [Char] -> IO ()
jugarTurno palabra actual intentos letrasUsadas = do
    if intentos <= 0
        then do
            putStrLn "\n¡Te has quedado sin intentos!"
            putStrLn $ "La palabra era: " ++ palabra
            putStrLn "Presione Enter para continuar..."
            _ <- getLine
            return ()
        else if actual == palabra
            then do
                putStrLn "\n¡Felicidades! ¡Has ganado!"
                putStrLn $ "La palabra era: " ++ palabra
                putStrLn "Presione Enter para continuar..."
                _ <- getLine
                return ()
            else do
                putStrLn "\nDí una letra: "
                letra <- getLine
                let letraChar = toLower (head letra)
                
                if letraChar `elem` map toLower letrasUsadas
                    then do
                        putStrLn "¡Ya has usado esa letra!"
                        jugarTurno palabra actual intentos letrasUsadas
                    else if letraChar `elem` map toLower palabra
                        then do
                            let nuevaActual = revelarLetra palabra actual letraChar
                            let nuevasLetrasUsadas = letraChar : letrasUsadas
                            putStrLn $ "¡Correcto! La letra está en la palabra."
                            putStrLn $ "Palabra: " ++ nuevaActual
                            putStrLn $ "Intentos restantes: " ++ show intentos
                            putStrLn $ "Letras usadas: " ++ show nuevasLetrasUsadas
                            jugarTurno palabra nuevaActual intentos nuevasLetrasUsadas
                        else do
                            let nuevasLetrasUsadas = letraChar : letrasUsadas
                            putStrLn "¡Incorrecto! La letra no está en la palabra."
                            putStrLn $ "Palabra: " ++ actual
                            putStrLn $ "Intentos restantes: " ++ show (intentos - 1)
                            putStrLn $ "Letras usadas: " ++ show nuevasLetrasUsadas
                            jugarTurno palabra actual (intentos - 1) nuevasLetrasUsadas

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
                    limpiarPantalla
                    putStrLn "! Opción no válida. Por favor seleccione un número del 1 al 3."
                    putStrLn "Presione Enter para continuar..."
                    _ <- getLine
                    loop
    loop

    