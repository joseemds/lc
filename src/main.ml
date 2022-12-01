module Context = Map.Make(String)


type var = string

type term =
  | Var of var
  | Abs of var * term
  | App of term * term

let rec pp = function
    | Var v -> Format.sprintf "Var(%s)" v
    | Abs (param, body) -> Format.sprintf "Abs(\\%s -> %s )" param (pp body)
    | App (f, arg) -> Format.sprintf "App (%s) %s" (pp f) (pp arg)

let id = Abs ("x", Var("x"))

let app_id = App (id, Var("y"))


let () = Format.printf "%s\n" (pp id)
let () = Format.printf "%s\n" (pp app_id)

