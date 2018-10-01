--Created: December, 2017
--by César Mora
--Modified: June 02, 2018
--by David Osorio

--Función farey llama a la función conteo para realizar los cálculos
farey :: Int -> [String]
farey x
    | x < 0         = error "Fracción inválida! La secuencia de Farey está definida para denominadores > 0"
    | x == 0        = error "División entre 0! La secuencia de Farey está definida para denominadores > 0"
    | otherwise     = conteo x 1 x


--Explicaciones de líneas de código:
--1. Verifica que los números simplificados iguales a 1/2 (2/4, 3/6, 4/8...) no se repitan
--2. Verifica que las fracciones simplificadas no se repitan, por ej. 1/3 (2/6, 3/9... no se guardan)
--3. Misma función pero valida que si z==0 aumenta y en uno, y a z se le agrega a x para que empiece
    --(de esa manera no se cicla)
--4. Verifica que no se ingresen fracciones igual a 1 (2/2, 3/3...) con la condición y==z
--5. Misma función para evitar ciclo
--6. Si y se incrementó al mismo valor que x, la función termina
--7. Valida que no se acepten números mayores a 1, por ej. 3/2 == 1.5, por tanto y > z
--8. Función del ciclo
--9. Si todas las condiciones (guardas) anteriores fueron aceptadas, y si z es diferente de 0,
    --se ingresa la fracción concatenando y ++ z
--10. Si z es igual a 0 no se guarda nada. Se incrementa y, y se suma x a z para iniciar
--11. Si hay otra situacion, no hace n
conteo :: (Integral a, Show a) => a -> a -> a -> [[Char]]
conteo x y z
    | y > 1 && y * 2 == z                                   = conteo x y (z - 1)        --1
    | (rem y 2 == 0) && (rem z 2 == 0) && z /= 0            = conteo x y (z - 1)        --2
    | (rem y 2 == 0) && (rem z 2 == 0) && z == 0            = conteo x (y + 1) (z + x)  --3
    | y == z && z /= 0                                      = conteo x y (z - 1)        --4
    | y == z && z == 0                                      = conteo x (y + 1) (z + x)  --5
    | y == x                                                = ["1/1"]                   --6
    | y > z && z /= 0                                       = conteo x y (z - 1)        --7
    | y > z && z == 0                                       = conteo x (y + 1) (z + x)  --8
    | z /= 0 && x > y                                       = (show(y) ++ "/" ++ show(z)) : conteo x y (z - 1)  --9
    | z == 0 && x > y                                       = conteo x (y + 1) (z + x)  --10
    | otherwise                                             = []                        --11