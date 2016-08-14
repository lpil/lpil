module Triangle exposing (..)


triangleKind : number -> number -> number -> Result String String
triangleKind x y z =
    if List.any ((>=) 0) [ x, y, z ] then
        Err "Invalid lengths"
    else if x + y <= z || y + z <= x || z + x <= y then
        Err "Violates inequality"
    else if x == y && y == z then
        Ok "equilateral"
    else if x == y || y == z || z == x then
        Ok "isosceles"
    else
        Ok "scalene"
