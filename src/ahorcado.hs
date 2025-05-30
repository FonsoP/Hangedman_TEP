import System.IO
import Data.Char
import Data.List
import System.Directory

escribir :: String -> String
escribir(palabra) = take (length palabra) (repeat '_')


main = do
    
    -- lectura del archivo
    handle <- openFile "src/palabras.txt" ReadMode

    -- convertir el contenido a una lista de palabras
    contents <- hGetContents handle
    
    -- crea un arreglo con las lineas del archivo
    let palabrasList = lines  contents
    
    -- mostrar en pantalla la palabra en minusculas
    putStrLn "Dí una letra: "
    
    letra <- getLine

    putStr $ unlines palabrasList 

    putStr $ escribir(palabrasList !! 0) ++ "\n"

    putStr letra

    