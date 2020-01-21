type !t = Lin (int, ![|int|])

let x = !Lin (1, [| 1, 2, 3 |]) in
let y = !Lin (1, [| 1, 2, 3 |]) in
let !Lin (_, a) = x in
let !Lin (b, _) = y in
(x, y)