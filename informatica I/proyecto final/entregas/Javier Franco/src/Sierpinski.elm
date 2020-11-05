module Sierpinski exposing (..)


sierpinski largo n =
    let
        p1 = (1, largo * sin (pi/3))
        p2 = (1 + largo, largo * sin (pi/3))
        p3 = (1 + largo * cos (pi/3), 0)
    in
        sierpinskib n p1 p2 p3 



sierpinskib n (x1,y1) (x2,y2) (x3,y3) = 
    let
        puntomediox1x2 = x1 + (x2 - x1)/2
        puntomedioy1y2 = y1 + (y2 - y1)/2
        puntomediox2x3 = x2 + (x3 - x2)/2
        puntomedioy2y3 = y2 + (y3 - y2)/2
        puntomediox1x3 = x1 + (x3 - x1)/2
        puntomedioy1y3 = y1 + (y3 - y1)/2

    in
        if n <= 0
        then [(x1,y1), (x2,y2), (x3,y3)]
        else 
            sierpinskib (n - 1) (x1, y1) (puntomediox1x2, puntomedioy1y2) (puntomediox1x3, puntomedioy1y3) ++ sierpinskib (n - 1) (x2,y2) (puntomediox1x2, puntomedioy1y2) (puntomediox2x3, puntomedioy2y3) ++ sierpinskib (n - 1) (x3,y3) (puntomediox1x3, puntomedioy1y3) (puntomediox2x3, puntomedioy2y3)
