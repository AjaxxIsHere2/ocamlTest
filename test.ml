(*let divides k n =
  n mod k = 0 ;;


let rec noDivisorsFrom m n =
  if m = n-1
    then not(divides m n) else
      not (divides m n) && noDivisorsFrom(m+1) n;;

let value = noDivisorsFrom 23 4;;

(*Function to find the exponent from a number*)
let rec find_exponent n m =
  if m = 0 then
    1
  else 
    n * find_exponent n (m - 1);;

    find_exponent 42 5;; *)


(*let x = 4 in x + 7;;

let x = 5 in let x = 9 in x;;

let x = 4 in 
(let x = 8 in x) + x;;

(true, ("nested", "pair") , 10, "pair");;

let max2 p =
  let (x, y) = p
in if x > y then x else y;;

let result = max2 (3, 7);;

let sum t =
  let (x , y , z) = t
in x +. y +. z;;

sum (57., 7.62 , 898.2);;

let rec naivesum n = 
  if n = 1
    then 1
else n + naivesum (n-1);;

naivesum 1000;;

let rec fib n =
  if n < 2
  then n
else fib(n-1) + fib(n-2);;

fib 10;;

let matrix_multiply ((a, b), (c, d)) ((e, f), (g, h)) =
  let open Int64 in
  let ae = add (mul a e) (mul b g) in
  let bf = add (mul b f) (mul d h) in
  let cg = add (mul c e) (mul d g) in
  let dh = add (mul c f) (mul d h) in
  ((ae, bf), (cg, dh))

let rec matrix_power m n =
  if n = 1L then m
  else if Int64.rem n 2L = 0L then
    let half_pow = matrix_power m (Int64.div n 2L) in
    matrix_multiply half_pow half_pow
  else
    matrix_multiply m (matrix_power m (Int64.sub n 1L))

let fib_matrix n =
  let fib_matrix = ((1L, 1L), (1L, 0L)) in
  let result_matrix = matrix_power fib_matrix n in
  match result_matrix with
  | ((result, _), _) -> result

let () =
  let n = 50L in (* Change this to the desired Fibonacci number you want to compute *)
  let result = fib_matrix n in
  Printf.printf "Fibonacci(%Ld) = %Ld\n" n result*)

let rec naivesum n =
  if n = 1
  then 1
  else n + naivesum (n-1);;

let fastfib k =
  let minifib p = let (a, b) = p in (b, a+b) in
  let rec auxfib n =
  if n = 0
    then (0,1)
  else minifib (auxfib (n-1))
    in
    let (x,_) = auxfib k in x;


auxfib 1;;