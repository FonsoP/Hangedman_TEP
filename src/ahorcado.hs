import System.IO
import Data.Char
import Data.List
import System.Directory


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

