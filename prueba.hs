

sumaTotalOpt x y = if x == 0 
    then y
    else sumaTotalOpt (x-1) (x+y)

crearLista x y = if x ==  0
        then y
        else 
            crearLista (x-1) ((x-1):y)

crearLista' x y = if == 0
        then y
        else 
            crearLista' z (z:y)
            where z = x - 1


                