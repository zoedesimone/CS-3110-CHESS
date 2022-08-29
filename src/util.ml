(** Explode and implode taken from
    https://caml.inria.fr/pub/old_caml_site/Examples/oc/basics/explode.ml*)
let explode s =
  let rec expl i l = if i < 0 then l else expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []

let implode l =
  let result = Bytes.create (List.length l) in
  let rec imp i = function
    | [] -> result
    | c :: l ->
        result.[i] <- c;
        imp (i + 1) l
  in
  imp 0 l

let rec build_list length elm =
  if length > 0 then elm :: build_list (length - 1) elm else []
