
(** 
    Cette partie du programme NE DOIT EN AUCUN CAS être modifiée sous peine
    de voir votre code ne pas passer les tests 
*)
type instruction = (* représentation d'une instruction reçu *)
  | Left (* Déplacement du curseur vers la gauche *)
  | Right (* Déplacement du curseur vers la droite *)
  | Write of char (* Écriture du caractère sur le ruban *)
  | Repeat of (int * instruction list) (* Repeter n fois la liste d'instructions *)
  | Caesar of int (* Caesar n à partir de la phase 3Aiens *)
  | Delete of char (* Delete(a) suppression du caractère a du message à partir de la phase 3Aiens *)
  | Invert (* Invert : retournement du message  à partir de la phase 3Aiens *)
  
type program = instruction list (* Un programme est simplement une liste d'instruction *)

(**
   VOUS N'AVEZ PAS BESOIN DE LIRE OU DE COMPRENDRE CETTE PARTIE DU CODE (LES SIX FONCTIONS SUIVANTES). 
   ELLE EST VOLONTAIREMENT NON COMMENTÉE ET RÉDIGÉE DE MANIÈRE PEU CLAIRE 
 *)
let rec analyse_comment fd =
  let c = Scanf.bscanf fd "%c" (fun c -> c) in
  if c = '\n'
  then ()
  else analyse_comment fd
             
let rec analyse_program_aux =
  fun fd lvl  ->
  try 
    let c = Scanf.bscanf fd "%c" (fun a  -> a) in
    if c = '#'
    then
      let _ = analyse_comment fd in
      analyse_program_aux fd lvl
    else if c = ';' || c = '\n' || c = ' '
    then analyse_program_aux fd lvl
    else if c = ']'
    then
      if lvl > 0
      then []
      else raise (Invalid_argument "Error on char ]")
    else if c = 'W'
      then
        let i= Write(Scanf.bscanf fd "(%c)" (fun a -> a)) in
        let li = analyse_program_aux fd lvl in
        i::li
    else if c = 'C'
      then
        let i= Caesar(Scanf.bscanf fd "(%d)" (fun a -> a)) in
        let li = analyse_program_aux fd lvl in
        i::li
    else if c = 'D'
    then
      let a = Scanf.bscanf fd "(%c)" (fun a -> a) in 
      let i= Delete(a) in
      let li = analyse_program_aux fd lvl in
      i::li
    else if c = 'R'
    then let li = analyse_program_aux fd lvl in
         Right::li
    else if c = 'I'
    then Invert::analyse_program_aux fd lvl
    else if c = 'L'
    then Left::analyse_program_aux fd lvl
    else if c = 'F'
    then
      let n = Scanf.bscanf fd "(%d,[" (fun n -> n) in
      let l = analyse_program_aux fd (lvl + 1) in
      let c = Scanf.bscanf fd "%c" (fun a -> a) in
      if c <> ')' then raise (Invalid_argument ("Error found '"^String.make 1 c^"' instead of ')'"))
      else
        let li = analyse_program_aux fd lvl in
        Repeat(n,l)::li
    else
      let _ = Format.printf  "ERROR '%c' (%d)@." c (Char.code c) in
      assert false
  with End_of_file -> []

let rec read_file_aux =
  fun acc fd ->
  try
    let c = Scanf.bscanf fd "%c" (fun x -> x) in
    read_file_aux (c::acc) fd
  with End_of_file -> acc

let read_file file =
  try
    if Sys.is_directory file
    then raise (Invalid_argument "is a directory")
    else
      let fd = Scanf.Scanning.open_in file in 
      List.rev (read_file_aux [] fd)
  with exc ->
    Format.fprintf Format.err_formatter "Problème à l'ouverture du fichier %s (exception %s)@." file (Printexc.to_string exc);
    exit 127

                    
let rec fprintf_instruction fmt i =
  match i with
  | Write c -> Format.fprintf fmt "W(%c)" c
  | Right -> Format.fprintf fmt "R"
  | Left -> Format.fprintf fmt "L"
  | Repeat(n,li) ->
     Format.fprintf fmt "F(%d,[%a])" n (fun fmt li -> List.iter (fun i -> Format.fprintf fmt "%a;" fprintf_instruction i) li) li
  | Caesar n -> Format.fprintf fmt "C(%d)" n
  | Delete(a) -> Format.fprintf fmt "D(%c)" a
  | Invert -> Format.fprintf fmt "I"
            
let fprintf_program fmt l =
  List.iter (fun i -> Format.fprintf fmt "%a;" fprintf_instruction i) l

(*** 
     Retour au côté clair
*)


(* 
   [print_program] : program -> unit 
   affiche un [program] sur la sortie standard de l'executable
 *)  
let print_program p = Format.printf "%a" fprintf_program p

(*
  [analyse_program] : unit -> program
  Lit un programme 1Aien ou 2Aien, l'analyse et le retourne sous forme d'une valeur de type [program]
 *)                    
let analyse_program file =
  try
    if Sys.is_directory file
    then raise (Invalid_argument "is a directory")
    else
      let fd = Scanf.Scanning.open_in file in 
      analyse_program_aux fd 0
  with exc ->
    Format.fprintf Format.err_formatter "Problème à l'ouverture du fichier %s (exception %s)@." file (Printexc.to_string exc);
    exit 127









    
(** Votre code doit commencer à partir de ce point.

    NE MODIFIER RIEN AU DESSUS DE CE POINT 
*)

type ruban = { left: char list; right: char list; };; (* zipper de char *)
let nruban = {left = []; right = []};; (* ruban vide *)

(* ------ Fonctions sur les caractères ------ *)
(* Ces fonctions sont utilisées dans les map *)
let caesar off char =
  (* int -> char -> char *)
  (* Effectue un codage de César de pas off sur le caractère char *)
  (* Le codage est appliqué sur le regex [a-zA-Z] *)
  let ascii_span = 26 (* Taille de l'alphabet *)
  and char_pos_min = Char.code char - Char.code 'a' (* Position de char dans l'intervalle [a - z] *)
  and char_pos_maj = Char.code char - Char.code 'A' (* Position de char dans l'intervalle [A - Z] *)
  and ccode = Char.code char
  in
  if ccode >= Char.code 'A' && ccode <= Char.code 'Z'
  then (* Majuscules *)
    Char.chr ((char_pos_maj + off) mod ascii_span + Char.code 'A')
  else (* Minuscules *)
    Char.chr ((char_pos_min + off) mod ascii_span + Char.code 'a');; 
  
let del_char comp src =
  (* char -> char -> char *)
  (* Supprime src si src et comp sont les mêmes caractères *)
  if src = comp then (Char.chr 0) else src;; (* char NULL simule la suppression d'un maillon *)


(* ------ Fonctions de lecture du ruban ------ *)
let lshift zp = match zp.left with
  (* ruban -> ruban *)
  (* Déplacement du curseur vers la gauche *)
  |[] -> {left = []; right = ' '::zp.right}
  |hd::tl -> {left = tl; right = hd::zp.right};;

let rshift zp = match zp.right with
  (* ruban -> ruban *)
  (* Déplacement du curseur vers la droite *)
  |[] -> {left = ' '::zp.left; right = []}
  |hd::tl -> {left = (hd::zp.left); right = tl};;

let rec rewind r =
  (* ruban -> ruban *)
  (* Rembobinage d'un ruban *)
  if r.left = [] then r else rewind (lshift r);;             


  

(* ------ Fonctions d'édition directe du ruban ------ *)
(* L'écriture se fait à droite *)
(* On crée un nouveau maillon lorsqu'on est tout à droite *)
let fold_ruban f v0 r =
  (* ('a -> char -> 'a) -> 'a -> ruban -> 'a *)
  (* Application d'une fonction sur un ruban (fold) *)
  let rev_r = rewind r in
  let rec li_parser f v0 r =
    (* -AUXILIAR- Application d'une fonction sur une liste *)
    List.fold_left f v0 r
  in li_parser f v0 rev_r.right;;

let map_ruban f r =
  (* (char -> char) -> ruban -> ruban *)
  (* Application d'une fonction sur un ruban (map) *)
  {left = List.map f r.left; right = List.map f r.right};;

let invert_ruban r =
  (* ruban -> ruban *)
  (* Inversion d'un ruban *)
  {left = r.right; right = r.left};;

let push ch zp = match zp.right with
  (* char -> ruban -> ruban *)
  (* Écriture d'un caractère sur la tête du ruban *)
  |[] -> {left = zp.left; right = [ch]}
  |hd::tl -> {left = zp.left; right = ch::tl};;

let rec parse_instr q ins =
  (* ruban -> instruction -> ruban *)
  (* Analyse d'une instruction unique et application de son effet sur le ruban *)
  match ins with 
    |Left -> lshift q
    |Right -> rshift q
    |Write(ch) -> push ch q
    |Repeat(n,li) ->
      if (n > 0)
      then begin
        match li with
        |[] -> q
        |hd::tl -> parse_instr (parse_instr (parse_instr q hd) (Repeat(1,tl))) (Repeat(n-1,li)); end
      else q
    |Caesar(k) -> map_ruban (caesar k) q
    |Delete(ch) -> map_ruban (del_char ch) q
    |Invert -> invert_ruban q;;
                   

let rec adv_prog zp prog =
  (* ruban -> instruction list -> ruban *)
  (* Analyse d'une liste d'instructions *)
  match prog with
  |[] -> zp
  |hd::tl -> adv_prog (parse_instr zp hd) tl;;
           
let execute_program p =
  (* instruction list -> ruban *)
  (* Exécution d'un programme *)
  adv_prog nruban p;;




let generate_program msg =
  (* char list -> instruction list *)
  (* Conversion d'un message (liste de char) en programme 2A *)
  let rec construct_program msg prog =
    (* -AUXILIAR- Construction d'un programme 2A non optimisé *)
    match msg with
    |[] -> prog
    |hd::tl -> construct_program tl (Left::Write(hd)::prog)
  in
  construct_program msg []
  ;;
                      
(** Votre code doit s'arreter à partir de ce point.

    NE MODIFIER RIEN EN DESSOUS DE CE POINT 
 *)











                         
                         
let die i = 
  let _ = Format.fprintf Format.err_formatter "Usage: %s <1|2|3> <file>@." Sys.argv.(0) in
  exit i
  
let main phase file =
  if phase = "1" || phase = "2"
  then
    let li = analyse_program file in
    let rub = execute_program li in
    let _ = fold_ruban (fun _ c -> Format.printf "%c" c) () rub in
    Format.printf "@."
  else if phase = "3"
  then
    let msg = read_file file in
    let p = generate_program msg in
    print_program p
  else die 1


let _ =
  if Array.length Sys.argv = 3 
  then
    main Sys.argv.(1) Sys.argv.(2)
  else die 2
