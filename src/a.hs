import System.IO
import Data.Char
import Data.List
import System.Directory

concatenar :: String -> String -> String

concatenar letras nueva = if letras == "" then [head nueva]
    else letras ++  " , " ++ [head nueva] 

-- crea la palabra cubierta con guiones bajos
cubrir :: String -> String
cubrir palabra = take (length palabra) (repeat '_')

-- descubre la letra en la palabra cubierta
descubrir :: (Int, String, Char) -> String
descubrir (indice, cubierta, letra) =  take indice cubierta ++ [letra] ++ drop (indice + 1) cubierta;
        
-- busca la letra en la palabra y devuelve los indices donde se encuentra
buscar :: Char -> String -> [Int]
buscar letra palabra =
    [i | (i, c) <- zip [0..] palabra, c == letra]

procesarLetra :: String -> String -> Char -> String
procesarLetra palabra cubierta letra =
    if null (buscar letra palabra) then
        cubierta
    else
        foldl (\acc idx -> descubrir (idx, acc, letra)) cubierta (buscar letra palabra)
        



jugar :: Int -> String -> String -> String -> IO ()
jugar intentos palabraElegida cubierta letras= do
    putStrLn "Palabra cubierta: "
    putStrLn cubierta

    putStrLn "Dí una letra: "
    -- leer de pantalla
    letra <- getLine

    let nuevaCubierta = procesarLetra palabraElegida cubierta (head letra)
    
    let letras2 = concatenar letras letra

    if nuevaCubierta == cubierta then do
        putStrLn "Letra incorrecta."
    else do
        putStrLn "Letra correcta."

    if nuevaCubierta == palabraElegida then do
        putStrLn "¡Felicidades! Has adivinado la palabra."
    else 

        if intentos > 1 then do
            putStrLn $ "Intentos restantes: " ++ show (intentos - 1)
            putStrLn $ "Letras usadas: " ++ letras2
            jugar (intentos - 1) palabraElegida nuevaCubierta letras2
        else do
            putStrLn "Has perdido, no te quedan intentos."



main ::IO ()
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
    let intentos = length palabraElegida + 2

    jugar intentos palabraElegida cubierta ""

    -- cerrar el archivo
    hClose handle



