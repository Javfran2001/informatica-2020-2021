module Koch exposing (..)


puntoIzquierdo (xinicial, yinicial) (xfinal, yfinal) = 
    let
        x1 = xinicial + (xfinal - xinicial)/3
        y1 = yinicial + (yfinal - yinicial)/3
    in
        (x1,y1) 

puntoderecho (xinicial, yinicial) (xfinal, yfinal) = 
    let
        x2 = xfinal - (xfinal - xinicial)/3
        y2 = yfinal - (yfinal - yinicial)/3

    in
        (x2,y2)
    
puntoarriba (x1,y1) (x2,y2) =
    let
        x3 = (x1 + x2) * cos(pi/3) - (y2 - y1) * sin(pi/3)
        y3 = (y1 + y2) * cos(pi/3) + (x2 - x1) * sin(pi/3)
    in
        (x3, y3)
curvadeVonKoch (xinicial, yinicial) (xfinal, yfinal) n = 
    let
        puntoInicial = (xinicial, yinicial)
        puntoFinal = (xfinal, yfinal)
        izquierda = puntoIzquierdo puntoInicial puntoFinal
        derecha = puntoderecho puntoInicial puntoFinal
        arriba = puntoarriba izquierda derecha
    in
    if  n == 0
    then [puntoInicial, puntoFinal]
    else 
        if n == 1 
            then [puntoInicial, izquierda, derecha, arriba, puntoFinal]
            else 
                curvadeVonKoch puntoInicial izquierda (n - 1) ++ curvadeVonKoch izquierda derecha (n - 1) ++ curvadeVonKoch derecha arriba (n - 1) ++ curvadeVonKoch arriba puntoFinal (n - 1)

snowflake largo n = 
    let
        izquierda = (1,0)
        derecha = (1 + largo, 0)
        arriba = (1 + largo * cos(pi/3), largo * sin(pi/3))
    in
        curvadeVonKoch arriba arriba n ++ curvadeVonKoch arriba derecha n ++ curvadeVonKoch derecha izquierda n 
    