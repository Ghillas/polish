(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Signes des variables *)
type sign = Neg | Zero | Pos | Error

(** Expressions arithmétiques *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block


(**une ligne est un numero de ligne(dans le programme polish) un nombre d'indentation et une chaine de caractere correspondant au contenue de la ligne*)
type ligne =
  |Ligne of position * int * string

          
(***********************************************************************)
    

(*fonction qui calcule le nombre d'indentation d'une ligne(sa profondeur dans le block)*)
let rec indentation str= 
  if str.[0] = ' ' && str.[1] = ' ' then 
    1 + indentation(String.sub str 2 (String.length str - 2)) else 0
;;

(*fonction qui transforme un fichier lu en liste de ligne*)
let read_file (filename: string): ligne list =

  let rec read_file_rec (ic: in_channel) (lines_acc: ligne list) (pos: int) =
    try
      let line = input_line ic in
      read_file_rec ic (lines_acc @ [Ligne(pos, indentation line, line)])(pos+1)
    with _ -> close_in ic; lines_acc in

  let ic = open_in filename in
  read_file_rec ic [] 1
;;


let rec print_file fic =
  match fic with
  |[] -> print_string "bonjour frerot\n"
  |s :: t -> (match s with
              |(c,i) -> print_string ("; " ^ c ^ ","); print_int i ;print_file t;
             )
;;

(*fonction qui tranforme une chaine de caractere est en liste*)
let string_to_list str = 
  let liste = String.split_on_char ' ' str in 
  List.filter (fun x -> x <> "") liste
;;

(*fonction qui verifie si un caractere est un operateur*)
let est_op c = 
  if(c = "+" || c = "*" || c = "-" || c = "/" || c = "%") then true else false
;;

(*fonction qui verifie si un caractere est un comparateur*)
let est_comp c =
  if(c = "=" || c = "<=" || c = "<" || c = "<>" || c = ">=" || c = ">") then true else false
;;

(*fonction qui attribut a chaque operateur son type Op correspondant*)
let string_to_op c = 
  match c with
  |"+" -> Add
  |"-" -> Sub
  |"*" -> Mul
  |"/" -> Div
  |"%" -> Mod 
  |_ -> failwith "expression incorrect"
;;

(*fonction qui associe les symbole de comparaison a leur type*)
let string_to_comp c = 
  match c with
  |"=" -> Eq
  |"<>" -> Ne
  |"<" -> Lt
  |"<=" -> Le
  |">" -> Gt
  |">=" -> Ge
  |_ -> failwith "expression incorrect"
;;


(*fonction qui renvoie l'indice du comparateur present dans une liste, -1 s'il n'est pas present*)
let rec recherche_comp liste i = 
  match liste with
  |[] -> -1
  |s :: t -> if est_comp s then i else recherche_comp t (i+1)
;;

(*fonction qui cree une liste avec les elements qui ont une position dans l'intervalle que l'on souhaite*)
let rec add_list liste tot pos fin = 
  match liste with
  |[] -> tot
  |s :: t -> if pos < fin then add_list liste (tot @ [List.nth liste pos]) (pos+1) fin else tot
;;

(*fonction qui transforme une chaine de caractere en type expr*)
let rec expr_of_list liste = 
  match liste with
  |[] -> failwith "on ne tombe jamais dans ce cas"
  |[x] -> (try let y = int_of_string x in Num(y) with Failure _ -> Var(x))
  |s :: t -> if est_op s then Op(string_to_op s,expr_of_list([List.hd t]),expr_of_list(List.tl t))
             else (try let y = int_of_string s in Num(y) with Failure _ -> Var(s))
;;

(*fonction qui crée les condition pour les if et while*)
let create_cond liste =
  let com = (recherche_comp liste 0) in
  if com > 0 then         (*ont cree 2 liste avec les intervalle [0,indice du comparateur -1] et [indice du comparateur + 1, fin]*)
    (expr_of_list(add_list liste [] 0 com),string_to_comp(List.nth liste com),expr_of_list(add_list liste [] (com+1) (List.length liste ))) 
  else failwith "expression incorrect" 
;;

let rec ajout liste tot i =
  match liste with
  |[] -> tot
  |s :: t -> (match s with
              |Ligne(x,y,str) -> if y < i then tot else ajout t (tot @ [s]) i
             )
;;

(*fonction qui renvoie la liste des lignes contenue dans un ELSE*)
let rec selection liste i =
  match liste with
  |[] -> []
  |s :: t -> (match s with
              |Ligne(x,y,str) -> let contenue = string_to_list str in
                                 if  y = i && (List.hd contenue = "ELSE") then ajout t [] (i+1) else
                                   if y = i && (List.hd contenue <> "ELSE") then [] else
                                     if y > i then selection t i else []
             )
;;





(*fonction qui transforme une liste de type ligne en liste de type block(program)*)
let rec instr_of_list liste tot indent =
  match liste with
  |[] -> tot
  |s :: t -> (match s with  (*le if permet de faire en sorte que la fonction passe a la prochaine ligne sans la traiter si l'indentation est plus grande que celle demander 
  (un autre appelle a la fonction gerera le cas contraire), traite la ligne en question si l'indentation demandé est la bonne enfin la fonction retourne le resultat si l'indentation 
  est plus petite que celle demandé (car nous sortons d'un block)*)
              |Ligne(x,y,str) -> if y > indent then instr_of_list(t)(tot)(indent) else if y < indent then tot else
                                   let contenue  = string_to_list str in 
                                   if (List.length contenue > 1) && (List.hd (List.tl contenue) = ":=")
                                   then instr_of_list(t)(tot @ [x,Set(List.nth contenue 0, expr_of_list(List.tl(List.tl contenue)))])(indent) else 
                                     (match List.hd contenue with
                                      | "IF" -> instr_of_list(t)(tot @ [x,If(create_cond (List.tl contenue),instr_of_list(t)([])(y+1),else_block t y)])(y)
                                      | "READ" -> instr_of_list(t)(tot @ [x,Read(List.nth contenue 1)])(y)
                                      | "PRINT" -> instr_of_list(t)(tot @ [x,Print(expr_of_list(List.tl contenue))])(y)
                                      | "WHILE" -> instr_of_list(t)(tot @ [x,While(create_cond (List.tl contenue),instr_of_list(t)([])(y+1))])(y)
                                      |_ -> instr_of_list(t)(tot)(indent)
                                   )
             )
           
(*fonction qui gere un block ELSE pour la fonction instr_of_list*)            
and else_block listes i =
  instr_of_list(selection listes i)([])(i+1)
;;




let read_polish (filename:string) : program =
  let liste_temp = read_file filename in
  instr_of_list (liste_temp) ([]) (0)
;;

let rec print_expr exp =
  match exp with
  |Num(x) -> print_int x ; print_string " "
  |Var(x) -> print_string x ; print_string " "
  |Op(o,e1,e2) -> (match o with
      |Add -> print_string "+ " 
      |Sub -> print_string "- "
      |Mul -> print_string "* "
      |Div -> print_string "/ "
      |Mod -> print_string "% "
    ) ; print_expr e1 ; print_expr e2;
;;

let print_cond cond =
  match cond with
  |(e1,c,e2) -> print_expr e1;
                (match c with
                 | Eq -> print_string "= "
                 | Ne -> print_string "<> "
                 | Lt -> print_string "< "
                 | Le -> print_string "<= "
                 | Gt -> print_string "> "
                 | Ge -> print_string ">= "
                );
                print_expr e2;
;;

let rec print_indentation n = if n=0 then "" else "  " ^ print_indentation (n-1);;


let rec print_block prog indent =
  match prog with
  |[] -> print_string ""
  |s :: t -> (match s with
              |(p,ins) -> print_instr ins indent;
             );
             print_block t indent;
             

and print_instr inst indent =
  match inst with
  |Set(n,e) -> print_string(print_indentation indent); print_string n; print_string " := "; print_expr e; print_newline();
  |Read(n) -> print_string(print_indentation indent); print_string "READ "; print_string n; print_newline();
  |Print(e) -> print_string(print_indentation indent); print_string "PRINT "; print_expr e; print_newline();
  |If(c,b1,b2) -> print_string(print_indentation indent); print_string "IF "; print_cond c; print_newline();
                  print_block (b1) (indent+1);
                  if b2 = [] then print_string "" else (print_string(print_indentation indent); print_string "ELSE\n"; print_block (b2) (indent+1);)
  |While(c,b) -> print_string(print_indentation indent); print_string "WHILE "; print_cond c; print_newline(); print_block (b) (indent+1);
;;


                  
let print_polish (p:program) : unit =
  print_block(p)(0); ();
;;
    
let rec eval_expr env exp = match exp with 
  | Num(x) -> x
  | Var(s) -> List.assoc s env
  | Op(o,e1,e2) -> (match o with
      | Add -> eval_expr(env) (e1) + eval_expr(env) (e2)
      | Sub -> eval_expr(env) (e1) - eval_expr(env) (e2)
      | Mul -> eval_expr(env) (e1) * eval_expr(env) (e2)
      | Div -> eval_expr(env) (e1) / eval_expr(env) (e2)
      | Mod -> eval_expr(env) (e1) mod eval_expr(env) (e2)
    )
;;

let eval_cond env cond =
  match cond with
  |(e1,c,e2) -> (match c with
                 |Eq -> if eval_expr(env)(e1) = eval_expr(env)(e2) then true else false
                 |Ne -> if eval_expr(env)(e1) <> eval_expr(env)(e2) then true else false
                 |Lt -> if eval_expr(env)(e1) < eval_expr(env)(e2) then true else false
                 |Le -> if eval_expr(env)(e1) <= eval_expr(env)(e2) then true else false
                 |Gt -> if eval_expr(env)(e1) > eval_expr(env)(e2) then true else false
                 |Ge -> if eval_expr(env)(e1) >= eval_expr(env)(e2) then true else false
                )
;;


let modif_list liste c i =
  if List.mem_assoc c liste then let res = List.remove_assoc c liste in [(c,i)] @ res
  else let res = [(c,i)] @ liste in res
;;


let rec eval_block env b =
  match b with
  |[] -> env
  |s :: t -> (match s with
      |(p,ins) -> let environnement = eval_instr(env)(ins) in eval_block(environnement)(t)
             ) 
    
and eval_instr env inst = 
  match inst with
  |Set(x,ins) -> let res = modif_list(env)(x)(eval_expr env ins) in res
  |Read(r) -> print_string "entrer un entier x :"; let x = read_int() in (r,x) :: env
  |Print(ex) -> print_int (eval_expr env ex); print_newline(); env 
  |If(c, b1, b2) -> if eval_cond env c then eval_block(env)(b1) else eval_block(env)(b2)
  |While(c, b) -> if eval_cond env c then let environnement = eval_block(env) (b) in
                                          eval_instr(environnement) (inst) else env
  
;;

let eval_polish (p:program) : unit =
  let _ = eval_block [] p in ()
;;

let rec simpl_expr e = match e with 
  | Op(Add, v, Num(0)) -> simpl_expr v
  | Op(Add, Num(0), v) -> simpl_expr v
  | Op(Mul, v, Num(0)) -> Num(0)
  | Op(Mul, v, Num(1)) -> simpl_expr v
  | Op(Mul, Num(0), v) -> Num(0)
  | Op(Mul, Num(1), v) -> simpl_expr v
  | Op(Sub, v, Num(0)) -> simpl_expr v 
  | Op(Div, Num(0), v) -> Num(0)
  | Op(Div, v, Num(1)) -> simpl_expr v
  | Op(Mod, v, Num(1)) -> simpl_expr v
  | Op(Mod, Num(0), v) -> Num(0)
  | _ -> e
;; 

let simpl_cond cond = match cond with 
    | (a,comp,b) -> (simpl_expr a, comp, simpl_expr b)
;;

let is_cond_constant cond = let c_simpl = simpl_cond cond in match c_simpl with
    | (Num(x),comp,Num(y)) -> true
    | _ -> false
;;

let eval_constante e = match e with
  | Num(x) -> x
  | _ -> failwith "ce n'est pas une constante"
;;

let eval_cond_constante cond = let c = simpl_cond cond in
  match c with
  |(e1,Eq,e2) -> if eval_constante e1 = eval_constante e2 then true else false
  |(e1,Ne,e2) -> if eval_constante e1 <> eval_constante e2 then true else false
  |(e1,Lt,e2) -> if eval_constante e1 < eval_constante e2 then true else false
  |(e1,Le,e2) -> if eval_constante e1 <= eval_constante e2 then true else false
  |(e1,Gt,e2) -> if eval_constante e1 > eval_constante e2 then true else false
  |(e1,Ge,e2) -> if eval_constante e1 >= eval_constante e2 then true else false
;;

let rec simpl_instr i = match i with 
  |(n,If(c,b1,b2)) -> if is_cond_constant c then if eval_cond_constante c then simpl_block b1 else simpl_block b2 else [(n,If(c,b1,b2))]
  |(n,While(c,b)) -> if is_cond_constant c then if not(eval_cond_constante c) then [] else [(n,While(c,b))] else [(n,While(c,b))]
  |(n,Print(e)) -> [(n,Print(simpl_expr e))]
  |(n,Set(s,e)) -> [(n,Set(s, simpl_expr e))]
  |(n,Read(s)) -> [(n,Read(s))] 
  
and simpl_block b = match b with
  |[] -> []
  |x :: xs -> simpl_instr x @ simpl_block xs
;;

let simpl_polish p = simpl_block p;;

let rec acc_var varlist b = match b with 
  | [] -> varlist
  | x :: xs -> let variables = acc_instr varlist x in  acc_var variables xs

and acc_instr varlist i = match i with 
  | (n,Set(s,e)) -> if List.mem s varlist then varlist else varlist @ [s] 
  | (n,Read(s)) -> if List.mem s varlist then varlist else varlist @ [s] 
  | (n,If(c,b1,b2)) -> let variable = acc_var varlist b1 in acc_var variable b2
  | (n,While(c,b)) -> acc_var varlist b
  | _ -> varlist
;; 

let print_variables p = let rec print_list_var l = match l with 
    | [] -> ""
    | x :: xs -> x ^ " " ^ print_list_var xs
  in print_string (print_list_var (acc_var [] p)); print_newline();;

let sign_variables p = print_string "à venir";;

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "option -eval permet d'evaluer le programme \n -reprint permet d'afficher le programme en Ocaml \n";
  print_string "option -simpl permet de simplifier le programme \n -vars présente les variables du programme \n -sign montre les signes possibles pour les variables à la fin de l'execution \n"
;; 

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | [|_;"-simpl";file|] -> print_polish (simpl_polish (read_polish file))
  | [|_;"-vars";file|] -> print_variables (read_polish file)
  | [|_;"-sign";file|] -> sign_variables (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
