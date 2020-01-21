let head = fun <a> (h::t : [a]) -> h in
let undefined = head [] in
if False && undefined then 0
else if True || undefined == 0 then 
  let arr = arr_from_elem 10 undefined |> update (fun (_ : int) -> undefined) 3 in
  let p = (undefined, undefined) in
  let l = [1, undefined] in
  let (x, y) = p in
  drop arr; head l
else 0