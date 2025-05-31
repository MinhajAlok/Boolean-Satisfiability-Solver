let validVarNames = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"];;

let partition (input : string list) (bound : string) : string list list =
  let rec aux acc curr depth tokens =
    match tokens with
    | [] -> List.rev ((List.rev curr) :: acc)
    | tok :: rest ->
        if tok = bound && depth = 0 then
          aux ((List.rev curr) :: acc) [] depth rest
        else
          let depth' =
            if tok = "(" then depth + 1
            else if tok = ")" then depth - 1
            else depth
          in
          aux acc (tok :: curr) depth' rest
  in
  if input = [] then [] else aux [] [] 0 input
;;

let getVariables (input : string list) : string list =
  let rec aux seen order tokens =
    match tokens with
    | [] -> List.rev order
    | tok :: rest ->
        if List.mem tok validVarNames && not (List.mem tok seen) then
          aux (tok :: seen) (tok :: order) rest
        else
          aux seen order rest
  in
  aux [] [] input
;;


let rec generateDefaultAssignments (varList : string list)
    : (string * bool) list =
  match varList with
  | [] -> []
  | v :: rest -> (v, false) :: generateDefaultAssignments rest
;;

let generateNextAssignments
    (assignList : (string * bool) list)
    : (string * bool) list * bool =
  let rec step lst =
    match lst with
    | [] -> ([], true)              
    | (v, b) :: rest ->
        if not b then
          ((v, true) :: rest, false)      
        else
          let (tail, carry) = step rest in
          ((v, false) :: tail, carry)         
  in
  let reversed, carry = step (List.rev assignList) in
  (List.rev reversed, carry)
;;

let rec lookupVar (assignList : (string * bool) list) (str : string) : bool =
  match assignList with
  | [] -> raise Not_found
  | (v, b) :: rest -> if v = str then b else lookupVar rest str
;;


let buildCNF (input : string list) : (string * string) list list =
  let strip_parens lst =
    match lst with
    | "(" :: rest ->
        (match List.rev rest with
         | ")" :: inner_rev -> List.rev inner_rev
         | _ -> lst)
    | _ -> lst
  in
  let build_clause tokens =
    let lit_tokens = partition (strip_parens tokens) "OR" in
    let build_lit lit =
      let lit = List.filter (fun t -> t <> "(" && t <> ")") lit in
      match lit with
      | "NOT" :: v :: _ -> (v, "NOT")
      | v :: _ -> (v, "")
      | _ -> failwith "Malformed literal"
    in
    List.map build_lit lit_tokens
  in
  let clauses = partition input "AND" in
  List.map build_clause clauses
;;

let evaluateCNF
    (cnf : (string * string) list list)
    (assignList : (string * bool) list) : bool =
  let eval_literal (v, flag) =
    let value = lookupVar assignList v in
    if flag = "NOT" then not value else value
  in
  let eval_clause clause =
    List.exists eval_literal clause
  in
  List.for_all eval_clause cnf
;;

let satisfy (input : string list) : (string * bool) list =
  let cnf = buildCNF input in
  let vars = getVariables input in
  let rec search assignment =
    if evaluateCNF cnf assignment then
      assignment
    else
      let next, wrapped = generateNextAssignments assignment in
      if wrapped then
        [ ("error", true) ]
      else
        search next
  in
  search (generateDefaultAssignments vars)
;;
