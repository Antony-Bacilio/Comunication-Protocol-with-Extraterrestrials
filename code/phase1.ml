(* ========================================================================= *)
(* ============================== phase1.ml ================================ *)
(* ========================================================================= *)


(****************************** F O N C T I O N S ***********************************)

 (**
  * Retourne la liste de caractères correspondant à la chaîne [s] .
  * @requires
  * @ensures
  * @raises
  *)
 let split_string s =
  let rec aux i n =
    if i >= n then []
    else (String.get s i)::aux (i+1) n in 
      aux 0 (String.length s)
 ;;

 (**
  * Longueur d'une liste 
  * @requires
  * @ensures
  * @raises
  *)
 let rec longueur_liste l = 
   if l = [] then 0
   else 1 + longueur_liste (List.tl l)
 ;;


 (**
  * Fonction qui lit deux lignes sur l'entrée standard du programme. La première doit comporter un entier, la seconde une chaîne de caractères. 
  * Retourne un couple composé de l'entier de la première ligne et de la liste des caractères de la seconde.
  * Lève l'exception [Failure "int_of_string"] si la première ligne ne représente pas un entier.
  * @requires
  * @ensures
  * @raises
  *)
 let parse_input () =
   let nb_antennas = int_of_string (read_line ()) in 
    let phrase = read_line () in 
      nb_antennas,(*split_string*) phrase
 ;;


 (**
  * Fonction qui calcule le temps d'éxecution (avec MATCH).
  * Changement lettre -> 3 secondes (N || P);;
  * Emission lettre -> 5 secondes (E);;
  * @requires
  * @ensures
  * @raises
  *)
 let cal_tmp cmd = 
   match cmd with
    |'N' |'P' -> 3
    |'E' -> 5
    |_-> 1
 ;;
  

 (**
  * Calcule la somme d'entiers d'une liste.
  * @requires
  * @ensures
  * @raises
  *)
 let rec somme_liste l =
  match l with
  |[] -> 0
  |e::l' -> e + somme_liste l'
 ;;
    
 (**
  * Affichage Commande 
  * @requires
  * @ensures
  * @raises
  *)
 let rec print_cmd nb cmd = 
  let i= ref 1 in 
  while !i<=nb do
    print_char cmd; print_char ' ';
    incr i;
  done
 ;;

 (**
  * Afficher chars d'une liste 
  * @requires
  * @ensures 
  * @raises
  *)
 let rec print_liste_char l = 
  match l with
  |[] -> ()
  |e::l' -> print_char e; print_char ' '; print_liste_char l'
 ;;

 (**
  * Ecriture Commande dans une liste
  * @requires
  * @ensures 
  * @raises
  *)
 let rec create_l_cmd nb cmd = 
  if nb==0 then ['E']
  else [cmd]@(create_l_cmd (nb-1) cmd);
 ;;


 (**
  * Fonction qui renvoie la position d'une lettre dans l'alphabet
  * ASCII : 65<=Upper<=90 :: espace=32 :: 97<=Lower<=122
  * @requires
  * @ensures 
  * @raises
  *)
 let char_position (c:char) =
    if (97<=(Char.code c) && (Char.code c)<=122) then ((Char.code c) - (Char.code 'a') + 1)
    else (
      if (Char.code c=32) then 0
      else (Char.code c) - (Char.code 'A') + 1
    )
 ;;

 (**
  * 
  * @requires
  * @ensures
  * @raises
  *)
 let distance_hor p_next p_curr =
    if p_next <= p_curr then p_curr - p_next
    else 27 + p_curr - p_next
 ;;



(*************************** P R O G R A M M E ***********************************)

(* Bienvenue *)
 Printf.printf "\n\t====================================================\n";;
 Printf.printf "\n\t\tBIENVENUE A LA COMMUNICATION DE BASE\n";;
 Printf.printf "\n\t====================================================\n\n";;


(* MESSAGE D'ENTREE *)
 Printf.printf "\n- Vous avez toujours 1 seule antenne (n = 1)\n\n";;
 Printf.printf "\n- Saisir votre message à émettre :\n\n";;
 let message = read_line();;


(* SUITES DE COMMANDES *)
let l_message = split_string message;;
let taille_msg = longueur_liste l_message;;
let v_message = Array.of_list l_message;;


let l_lettres = [' ' ; 'A' ; 'B' ; 'C' ; 'D' ; 'E'; 'F' ; 'G' ; 'H' ; 'I' ; 'J' ; 'K' ; 'L' ; 'M' ; 'N' ; 'O' ; 'P' ; 'Q' ; 'R' ; 'S' ; 'T' ; 'U' ; 'V' ; 'W' ; 'X' ; 'Y' ; 'Z'];;
let l_lettres_uplet = [(' ',0,0); ('A',1,26); ('B',2,25); ('C',3,24); ('D',4,23); ('E',5,22); ('F',6,21) ; ('G',7,20) ; ('H',8,19) ; ('I',9,18) ; ('J',10,17) ; ('K',11,16) ; ('L',12,15) ; ('M',13,14) ; ('N',14,13) ; ('O',15,12) ; ('P',16,11) ; ('Q',17,10) ; ('R',18,9) ; ('S',19,8) ; ('T',20,7) ; ('U',21,6) ; ('V',22,5) ; ('W',23,4) ; ('X',24,3) ; ('Y',25,2) ; ('Z',26,1)];;
let v_lettres_uplet = Array.of_list l_lettres_uplet;;

Printf.printf "\n- Les commandes permettant l'émission sont : \n\n" ;;
let courant = ref v_lettres_uplet.(0) in
  let cmds = ref [] in
    for i = 0 to taille_msg - 1 do
      let pos = char_position v_message.(i) in
        let _, p_hor, p_antihor = v_lettres_uplet.(pos) in
          let _, p_hor_0, p_antihor_0 = !courant in
            courant := v_lettres_uplet.(pos);
            let depl_hor = distance_hor p_antihor p_antihor_0 in
              let depl_antihor = distance_hor p_hor p_hor_0 in
                if depl_hor < depl_antihor then cmds := (create_l_cmd depl_hor 'N') @ !cmds
                else cmds := (create_l_cmd depl_antihor 'P') @ !cmds
    done;
    print_liste_char !cmds ;
    print_string "\n\n- Temps d'éxecution : " ;
    let tmp = somme_liste (List.map cal_tmp !cmds) in
      print_int tmp;
      print_string "\n\n";
;;

(* Fermeture *)
Printf.printf "\n\n\t====================================================\n";;
Printf.printf "\n\t\tFIN DE COMMUNICATION DE BASE\n";;
Printf.printf "\n\t====================================================\n\n";;

(*for i=0 to taille_msg-1 do
  let j = ref 0 in
    while !j<=26 do (
      let (l,n1,n2)= v_lettres_uplet.(!j) in
        if v_message.(i) == l then ( 
          if (n1<n2) then (
            let cmds = create_l_cmd n1 'N' in
              print_string "\n\n";
              print_liste_char cmds; 
              print_string "\n\n- Temps d'éxecution : "; 
              print_int (somme_liste (List.map cal_tmp cmds));
          )
          else (
            let cmds  = create_l_cmd n2 'P' in
              print_string "\n\n";
              print_liste_char cmds;
              print_string "\n\n- Temps d'éxecution : "; 
              print_int (somme_liste (List.map cal_tmp cmds));
          );
          j := !j;
        );
      j := !j+1;
    )
    done;
done;
;;*)