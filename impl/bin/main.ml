open Winksel.Imp_grammar

let () = print_endline "Hello, World!"

let four = Addition(3, 1)
let fls = Not(True)
let ite = IfThenElse(fls, Skip, Assign("hello", four))

let () = print_endline("cmpAExp: " ^ string_of_bool (cmpAExp (Addition(3, 5)) (Addition(3, 5))))