let f = fun <!a, b> (x : !a) -> fun (y : b) -o x in
(f [| 1, 2, 3 |] 4)