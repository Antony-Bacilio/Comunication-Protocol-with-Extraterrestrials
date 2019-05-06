(* ========================================================================= *)
(* ============================== phase1.ml ================================ *)
(* ========================================================================= *)


(****************************** F O N C T I O N S ***********************************)

 (**
  * @requires : une chaîne de caractères [s].
  * @ensures : Retourne une Liste de caractères correspondant à la chaîne [s]. 
  *)
  let split_string s =
    let rec aux i n =
      if i >= n then []
      else (String.get s i)::aux (i+1) n 
    in aux 0 (String.length s)
 ;;

 (**
  * @requires : une Liste.
  * @ensures : Longueur d'une liste.
  *)
 let rec longueur_liste l = 
   if l = [] then 0
   else 1 + longueur_liste (List.tl l)
 ;;


 (**
  * @requires : aucun argument pour lancer cette fonction.
  * @ensures : Lecture 2 lignes sur l'entrée standard (un entier et chaîne de caractères) et 
  *            Retourne un couple (entier de la première ligne, Liste de caractères de la seconde).
  * @raises : Exception [Failure "int_of_string"] si la première ligne ne représente pas un entier.
  *)
 let parse_input () =
   let nb_antennas = int_of_string (read_line ()) in 
      let phrase = read_line () in 
        nb_antennas,(*split_string*) phrase
 ;;


 (**
  * @requires : un caractère (une commande).
  * @ensures : Retourne un entier (temps d'éxecution) en fonction de la commande :
  *)
 let cal_tmp cmd = 
   match cmd with
    |'N' |'P' -> 3 (*Changement de lettre*)
    |'E' -> 5   (*Emission lettre*)
    |_-> 0
 ;;
  

 (**
  * Calcule la somme d'entiers d'une liste.
  * @requires : une Liste d'entiers.
  * @ensures : Somme d'élements de la Liste.
  *)
 let rec somme_liste l =
  match l with
  |[] -> 0
  |e::l' -> e + somme_liste l'
 ;;
    

 (**
  * @requires : une Liste de caractères.
  * @ensures : Affiche les élements de la Liste.
  *)
 let rec print_liste_char l = 
  match l with
  |[] -> ()
  |e::l' -> print_char e; print_char ' '; print_liste_char l'
 ;;


 (**
  * @requires : une lettre.
  * @ensures : Renvoie la position horaire de la lettre grâce au code ASCII (65<=Upper<=90 :: espace=32 :: 97<=Lower<=122).
  *)
 let char_position (c:char) =
    if (97<=(Char.code c) && (Char.code c)<=122) then ((Char.code c) - (Char.code 'a') + 1)
    else (
      if (Char.code c=32) then 0
      else (Char.code c) - (Char.code 'A') + 1
    )
 ;;


 (** 
  * @requires : Position de la lettre suivante et Position de lettre courante.
  * @ensures : Compare les distances entre les 2 positions dans les 2 sens (horaire et antihoraire) et Retourne la distance minimale.
  *)
 let distance_hor p_next p_curr =
    if p_next <= p_curr then p_curr - p_next
    else 27 + p_curr - p_next
 ;;


 (**
  * @requires : [l_cmds] Liste de commandes, [cmd] commande N ou P, [n] distance.
  * @ensures : Remplissage de la Liste de commandes commençant par le début.
  *)
 let rec ajoute_debut l_cmds cmd n =
    if n = 0 then 'E'::l_cmds
    else ajoute_debut (cmd::l_cmds) cmd (n-1)
 ;;


 (**
  * @requires : [message] Liste de caractères à traiter, [taille_msg] sa taille et [l_alph] Liste de 3-uplets (lettre alphabet, position horaire, position antihoraire).
  * @ensures : Traitement du message et Retourne une Liste avec les commandes 'N', 'E' ou 'P'.
  *)
 let traite_msg_1 message taille_msg l_alph =

    (**
     * @requires : [message] Liste de caractères, [courant] position courante (3-uplet), [cmds] Liste de commandes.
     * @ensures :  Parcourir chaque lettre du message gardant sa position et comparant avec la lettre précedente. Retourne une Liste de commandes.
     *) 
    let rec loop message courant cmds =
     match message with
      (* Après avoir parcouru le message, on inverse la Liste de commandes [cmds] ajoutée progresivement au header de la Liste [ajoute_debut] *)
      |[] -> List.rev cmds
      |lettre :: message ->
          (* pos : position d'une lettre du message*)
          let pos = char_position lettre in

          (* Récuperation d'un 3-uplet *)
          let _, p_hor_0, p_antihor_0 = courant in

          (* courant : prend l'uplet ("position") de la dernière lettre evaluée dans la Liste d'alphabet *)
          let courant = List.nth l_alph pos in
          let _, p_hor, p_antihor = courant in

          (* depl_hor : prend la distance dans le sens horaire à partir de la dernière lettre evaluée*)
          let depl_hor = distance_hor p_antihor p_antihor_0 in 

          (* depl_antihor : prend la distance dans le sens anti-horaire à partir de la dernière lettre evaluée*)
          let depl_antihor = distance_hor p_hor p_hor_0 in 

          (* Comparer les distances pour savoir si on ajoute 'N' ou 'P'*)
          if depl_hor < depl_antihor then loop message courant (ajoute_debut cmds 'N' depl_hor)
          else loop message courant (ajoute_debut cmds 'P' depl_antihor)

    in (loop message (List.hd l_alph) [])

 ;;



(*************************** P R O G R A M M E ***********************************)

(* BIENVENUE *)

Printf.printf "\n\t====================================================\n";;
Printf.printf "\n\t\tBIENVENUE A LA COMMUNICATION DE BASE (Phase 1)\n";;
Printf.printf "\n\t====================================================\n\n";;


(* TRAITEMENTS *)

Printf.printf "\n- Pour cette phase vous avez toujours 1 seule antenne (n = 1)\n\n";;
Printf.printf "\n- Saisir votre message à émettre :\n\n";;

(* Structure l'alphabet d'une roue : Liste de 3-uplets (lettre, position horaire, position antihoraire)  *)
let l_alphabet = [(' ',0,0); ('A',1,26); ('B',2,25); ('C',3,24); ('D',4,23); ('E',5,22); ('F',6,21) ; ('G',7,20) ; ('H',8,19) ; ('I',9,18) ; ('J',10,17) ; ('K',11,16) ; ('L',12,15) ; ('M',13,14) ; ('N',14,13) ; ('O',15,12) ; ('P',16,11) ; ('Q',17,10) ; ('R',18,9) ; ('S',19,8) ; ('T',20,7) ; ('U',21,6) ; ('V',22,5) ; ('W',23,4) ; ('X',24,3) ; ('Y',25,2) ; ('Z',26,1)];;

(* Traitement message *)
let message = read_line();;
let l_message = split_string message;;
let taille_msg = longueur_liste l_message;;

(* Suite de commandes en fonction du message saisi *)
let liste_cmds = traite_msg_1 l_message taille_msg l_alphabet in

  (* Impresion Liste de commandes *)
  Printf.printf "\n- Les commandes permettant l'émission de votre message ('%s') sont : \n\n" message;
  print_liste_char liste_cmds;

  (* Impresion Temps d'éxecution à partir de la Liste de commandes*)
  print_string "\n\n- Temps d'éxecution : " ;
  let tmp = somme_liste (List.map cal_tmp liste_cmds) in 
    print_int tmp;
    print_string " secondes.\n\n"
;;


(* FERMETURE *)

Printf.printf "\n\n\t====================================================\n";;
Printf.printf "\n\t\tFIN DE COMMUNICATION DE BASE\n";;
Printf.printf "\n\t====================================================\n\n";;
