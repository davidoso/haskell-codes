{-
HASKELL 2-HOUR (THAT BECAME 4-HOUR) EXAM, UNIT II
    Author:
        Created: June 01, 2018
            by David Osorio & Williams Estrada
        Modified: June 01, 2018 11:48:20
            by David Osorio
    
    Organization:
        Colima Institute of Technology
    
    Career:
        Computer Engineering
    
    Subject:
        Logic and Functional Programming
-}



-------------------------------------------
--Created: June 01, 2018 08:10:06
--Modified: June 01, 2018 11:47:39


--EJERCICIO 1
--complementoUno cadenabinaria → cadenaComplemento

--Genera una lista de dígitos (0 ó 1) pero es complicado concatenar los caracteres de la lista
--y formar una cadena, así que se utilizó la función compUno en vez de esta
--complementoUno n = [ if x == 0 then 1 else 0 | x <- (digits n) ]


--Genera una lista de dígitos [0..9] individuales para un número entero recibido
--Por ejemplo: digits 12345 -> [1, 2, 3, 4, 5]
--digits :: Integral a => a -> [a]
--digits 0 = []
--digits n = digits (div n 10) ++ [rem n 10]


--Imprime el complemento a uno de un número binario (recibe string no número entero)
--Autor: Ricardo Villagrana
compUno :: [Char] -> [Char]
compUno "1" = "0"
compUno "0" = "1"
compUno nbin
    | take 1 nbin == "1"    = "0" ++ compUno(tail nbin)
    | take 1 nbin == "0"    = "1" ++ compUno(tail nbin)
    | otherwise             = error "Error! El número no es binario"


--EJERCICIO 2
--convOctDecHex numbin → (numoct, numdec, numhex)

--Convierte un número en binario (recibe número entero no string) e imprime como strings sus representaciones
--en decimal, octal y hex. Para éstos 2 últimas, utiliza las funciones dto y dth que reciben el número en decimal
convertidor :: (Show a, Integral a) => a -> IO()
convertidor n = putStrLn (show n ++ " en binario es:\n"
                ++ show (bin2dec n) ++ " en decimal\n"
                ++ dto (bin2dec n) ++ " en octal\n"
                ++ dth (bin2dec n) ++ " en hexadecimal")


--Divide un número binario recibido en una lista de dígitos (0 ó 1)
--Un residuo diferente de 0 ó 1 implica que el número NO es binario
digits :: Integral a => a -> [a]
digits 0 = []
digits n
    | rem n 10 > 1  = error "Error! El número no es binario"
    | otherwise     = digits (div n 10) ++ [rem n 10]


--Recibe un número binario y lo transforma a una lista. Luego aplica una función de mapeo para generar la lista
--de base 2 para convertir el número a decimal. Finalmente llama la función de conversión bin2dec'
--Autor: Williams Estrada
bin2dec :: Integral a => a -> a
bin2dec n = bin2dec' a (length a)
    where a = map (2*) (digits n)


--Función que trabaja sobre una lista de potencias base 2 y según su posición, las eleva a ese número
--Autor: Williams Estrada
bin2dec' :: (Eq a, Num a) => [a] -> Int -> a
bin2dec' n 1
    | head n == 2 = 1
    | head n == 0 = 0
bin2dec' n l = head n ^ (length n - 1) + bin2dec' (tail n) (length n - 1)


--Elimina los caracteres \ y " que las funciones auxdto y auxdth generan al concatenar los show que c/u tiene
removeNonDigits :: [Char] -> [Char]
removeNonDigits xs = [ x | x <- xs, not (x `elem` "\\\"") ]


--Convierte de decimal a octal (función principal)
dto :: (Show a, Integral a) => a -> [Char]
dto n = removeNonDigits (auxdto n)


--Decimal a octal (función auxiliar que realiza cálculos recursivamente)
auxdto :: (Integral a, Show a) => a -> [Char]
auxdto n
    | n == 0 = []
    | otherwise = show (auxdto (div n 8)) ++ show (rem n 8)


--Convierte de decimal a hexadecimal (función principal)
dth :: (Show a, Integral a) => a -> [Char]
dth n = removeNonDigits (auxdth n)


--Decimal a hexadecimal (función auxiliar que realiza cálculos recursivamente)
auxdth :: (Integral a, Show a) => a -> [Char]
auxdth n
    | n == 0 = []
    | otherwise = show (auxdth (div n 16)) ++ hexReminder (rem n 16)


--Imprime los módulos hexadecimales con letras de la A-F o número 0-9 en su defecto
hexReminder :: (Eq a, Num a, Show a) => a -> [Char]
hexReminder 10 = "A"
hexReminder 11 = "B"
hexReminder 12 = "C"
hexReminder 13 = "D"
hexReminder 14 = "E"
hexReminder 15 = "F"
hexReminder x = show x


--EJERCICIO 3
--quehay cadena → (numletras, numdigitos, sumotrossim)

--Recibe una cadena y llama la función auxiliar para realizar el conteo de letras, dígitos y otros símbolos
--Los 3 ceros son los parámetros recibidos que servirán de contadores
quehay :: [Char] -> IO()
quehay str = contarSimbolos str str 0 0 0


--Cuenta recursivamente los tipos de caracteres de una cadena. Los 5 parámetros recibidos son:
--strOriginal: guarda la cadena original para mostrar al término de la función
--str: sirve para detectar qué tipo de caracter es el primero (head) de la cadena. Al final queda ""
--l: Contador para letras
--d: Contador para dígitos
--o: Contador para otros símbolos
contarSimbolos :: (Show a1, Show a2, Show a3, Num a1, Num a2, Num a3) =>
    [Char] -> [Char] -> a1 -> a2 -> a3 -> IO()
contarSimbolos strOriginal str l d o
    | str == "" = putStrLn (strOriginal ++ " tiene:\n"
                            ++ show l ++ " letras ASCII [a-zA-Z]\n"
                            ++ show d ++ " dígitos [0-9]\n"
                            ++ show o ++ " otros símbolos")
    | head (str) >= 'a' && head (str) <= 'z' = contarSimbolos strOriginal (tail str) (l + 1) d o
    | head (str) >= 'A' && head (str) <= 'Z' = contarSimbolos strOriginal (tail str) (l + 1) d o
    | head (str) >= '0' && head (str) <= '9' = contarSimbolos strOriginal (tail str) l (d + 1) o
    | otherwise = contarSimbolos strOriginal (tail str) l d (o + 1)