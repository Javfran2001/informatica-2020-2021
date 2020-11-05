module Cantor exposing (..)

-- Fractar de cantor


fcantor largo n = 
    let
        puntoIzquierdo = (largo - largo) 
        puntoIzquierdocentro = (largo * 1/3)
        puntoDerechocentro = (largo * 2/3)
        puntoDerecho = (largo)
    in
        fcantorb n puntoIzquierdo puntoIzquierdocentro puntoDerechocentro puntoDerecho

fcantorb n (puntoIzquierdo, puntoIzquierdocentro) (puntoDerechocentro, puntoDerecho) =
    let
        puntoIzquierdo = (largo - largo)
        nuevopizquierdo = (puntoIzquierdocentro * 1/3)
        nuevopderecho = (puntoDerechocentro * 1/3 )
        puntoDerecho = (largo)
    in
        if n <= 0
        then [(puntoIzquierdo, ountoIzquierdocentro), (puntoDerechocentro,puntoDerecho)]
        else fcantorb (n - 1) (puntoIzquierdo, nuevopizquierdo) (nuevopderecho, puntoDerecho)
