import System.IO
import Data.Char
import Data.List
import System.Directory
import System.Process
import System.Info
import GHC.Show (Show(show))
import Control.Concurrent (threadDelay)

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
    handle <- openFile "app/palabras.txt" ReadMode
    contents <- hGetContents handle
    let palabrasList = lines contents
    indrndm <- randomRIO (0, length palabrasList - 1) :: IO Int

    palabraElegida <- return $ map toUpper (palabrasList !! indrndm)
    jugarConPalabras palabraElegida

jugarConPalabras :: String -> IO ()

jugarConPalabras palabra = do
    let palabraOculta = escribir palabra
    let intentos = 6
    let letrasUsadas = []
    
    putStrLn "¡Bienvenido al juego del ahorcado!"
    putStrLn $ "La palabra tiene " ++ show (length palabra) ++ " letras"
    putStrLn $ "Palabra: " ++ palabraOculta
    putStrLn $ "Intentos restantes: " ++ show intentos
    
    jugarTurno palabra palabraOculta intentos letrasUsadas 

actualizarEstadisticas :: Bool -> IO ()
actualizarEstadisticas esVictoria = do
    (ganadas, perdidas, abandonadas) <- leerEstadisticas
    let (ganadas', perdidas', abandonadas') = if esVictoria 
                                            then (ganadas + 1, perdidas, abandonadas)
                                            else (ganadas, perdidas + 1, abandonadas)
    writeFile "estadisticas.txt" $ unlines $ map show [ganadas', perdidas', abandonadas']

dibujarAhorcado :: Int -> IO ()
dibujarAhorcado intentos = do
    case intentos of
        6 -> do
            putStrLn "\n  +---+"
            putStrLn "  |   |"
            putStrLn "      |"
            putStrLn "      |"
            putStrLn "      |"
            putStrLn "      |"
            putStrLn "========="
        5 -> do
            putStrLn "\n  +---+"
            putStrLn "  |   |"
            putStrLn "  O   |"
            putStrLn "      |"
            putStrLn "      |"
            putStrLn "      |"
            putStrLn "========="
        4 -> do
            putStrLn "\n  +---+"
            putStrLn "  |   |"
            putStrLn "  O   |"
            putStrLn "  |   |"
            putStrLn "      |"
            putStrLn "      |"
            putStrLn "========="
        3 -> do
            putStrLn "\n  +---+"
            putStrLn "  |   |"
            putStrLn "  O   |"
            putStrLn " /|   |"
            putStrLn "      |"
            putStrLn "      |"
            putStrLn "========="
        2 -> do
            putStrLn "\n  +---+"
            putStrLn "  |   |"
            putStrLn "  O   |"
            putStrLn " /|\\  |"
            putStrLn "      |"
            putStrLn "      |"
            putStrLn "========="
        1 -> do
            putStrLn "\n  +---+"
            putStrLn "  |   |"
            putStrLn "  O   |"
            putStrLn " /|\\  |"
            putStrLn " /    |"
            putStrLn "      |"
            putStrLn "========="
        0 -> do
            putStrLn "\n  +---+"
            putStrLn "  |   |"
            putStrLn "  O   |"
            putStrLn " /|\\  |"
            putStrLn " / \\  |"
            putStrLn "      |"
            putStrLn "========="
        _ -> return ()

-- Sonidos por consola
sonidoFallo :: IO ()
sonidoFallo = putStr "\a" >> hFlush stdout

-- Función auxiliar para validar que la entrada sea una sola letra
validarEntrada :: String -> Bool
validarEntrada entrada = length entrada == 1 && isLetter (head entrada)

jugarTurno :: String -> String -> Int -> [Char] -> [String] -> IO ()
jugarTurno palabra actual intentos letrasUsadas restoPalabras = do
    dibujarAhorcado intentos
    if intentos <= 0
        then do
            putStrLn "\n¡Te has quedado sin intentos!"
            putStrLn $ "La palabra era: " ++ palabra
      
            putStrLn "Presione Enter para volver al menú principal..."
            _ <- getLine
            return ()
        else if actual == palabra
            then do
                putStrLn "\n¡Felicidades! ¡Has ganado!"
                putStrLn $ "La palabra era: " ++ palabra
                _ <- getLine
                return ()
                
            else do
                putStrLn "\nDí una letra: "
                letra <- getLine
                if not (validarEntrada letra)
                    then do
                        limpiarPantalla
                        putStrLn "Error: Por favor ingrese UNA SOLA letra válida."
                        putStrLn "No se permiten números, espacios ni caracteres especiales."
                        jugarTurno palabra actual intentos letrasUsadas restoPalabras
                    else do
                        let letraChar = toLower (head letra)
                        if letraChar `elem` map toLower letrasUsadas
                            then do
                                limpiarPantalla
                                putStrLn "¡Ya has usado esa letra!"
                                jugarTurno palabra actual intentos letrasUsadas restoPalabras
                            else if letraChar `elem` map toLower palabra
                                then do
                                    limpiarPantalla
                                    let nuevaActual = revelarLetra palabra actual letraChar
                                    let nuevasLetrasUsadas = letraChar : letrasUsadas
                                    putStrLn $ "¡Correcto! La letra está en la palabra."
                                    putStrLn $ "Palabra: " ++ nuevaActual
                                    putStrLn $ "Intentos restantes: " ++ show intentos
                                    let letrasIncorrectas = filter (\l -> not (l `elem` map toLower palabra)) nuevasLetrasUsadas
                                    putStrLn $ "letras incorrectas: [" ++ intercalate ", " (map (:[]) letrasIncorrectas) ++ "]"
                                    jugarTurno palabra nuevaActual intentos nuevasLetrasUsadas restoPalabras
                                else do
                                    limpiarPantalla
                                    sonidoFallo
                                    let nuevasLetrasUsadas = letraChar : letrasUsadas
                                    putStrLn "¡Incorrecto! La letra no está en la palabra."
                                    putStrLn $ "Palabra: " ++ actual
                                    putStrLn $ "Intentos restantes: " ++ show (intentos - 1)
                                    let letrasIncorrectas = filter (\l -> not (l `elem` map toLower palabra)) nuevasLetrasUsadas
                                    putStrLn $ "letras incorrectas: [" ++ intercalate ", " (map (:[]) letrasIncorrectas) ++ "]"
                                    jugarTurno palabra actual (intentos - 1) nuevasLetrasUsadas restoPalabras

leerEstadisticas :: IO (Int, Int, Int)
leerEstadisticas = do
    existe <- doesFileExist "estadisticas.txt"
    if existe
        then do
            contenido <- readFile "estadisticas.txt"
            let lineas = lines contenido
            if length lineas >= 3
                then return (read (head lineas), read (lineas !! 1), read (lineas !! 2))
                else do
                    writeFile "estadisticas.txt" "0\n0\n0"
                    return (0, 0, 0)
        else do
            writeFile "estadisticas.txt" "0\n0\n0"
            return (0, 0, 0)

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

    