let f = fun <!a, b> (x : !a) -> fun (y : b) -o x in
let g = f [| 1, 2, 3 |] in
(g 1, g 2)