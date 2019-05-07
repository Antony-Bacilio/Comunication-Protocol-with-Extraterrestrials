
(* ========================================================================= *)
(* ============================== phase2.ml ================================ *)
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
   |'E' -> 5    (*Emission lettre*)
   |'S' -> 1   (*Changement de roue*)
   |_ -> 0
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
  * @requires : Nombre d'antennes.
  * ensures : Traitement nombre d'antennes et Retourne des messages en fonction du nombre d'antennes.
  *)
 let traite_anten nb =
  match nb with
  |0 -> print_string "\nSaisir nombre d'antennes >= 1 svp !\n\n" ; exit(1);
  |1 -> print_string "\n\n ==== Phase 1 ====\n" ;
  |_ -> print_string "\n- Vous avez " ; print_int nb ; print_string " antennes\n";
 ;;


 (**
  * @requires : [l_cmds] Liste de commandes, [cmd] commande N, P ou Sn, [n] distance.
  * @ensures : Remplissage de la Liste de commandes commençant par le début.
  *)
 let rec ajoute_debut l_cmds cmd n =
   if n = 0 then 'E'::l_cmds
   else ajoute_debut (cmd::l_cmds) cmd (n-1)
 ;;
 

 (**
  * @requires : [message] Liste de caractères à traiter, [taille_msg] sa taille et [l_alph] Liste de 3-uplets (lettre alphabet, position horaire, position antihoraire).
  * @ensures : Traitement du message avec une seule roue et Retourne une Liste avec les commandes 'N', 'P' et 'E'.
  *)
 let traite_msg_1 message taille_msg l_alph =

   (**
     * @requires : [message] Liste de caractères, [courant] position courante (3-uplet), [cmds] Liste de commandes.
     * @ensures :  Parcourir chaque lettre du message gardant sa position et comparant avec la lettre précedente. Retourne une Liste de commandes.
     *)
   let rec loop message courant cmds =
     match message with
      |[] -> List.rev cmds
      |lettre :: message ->
          (* pos : position d'une lettre du message*)
          let pos = char_position lettre in

          (* Récuperation d'un 3-uplet *)
          let _, p_hor_0, p_antihor_0 = courant in

          (* courant : prend l'uplet ("position") de la dernière lettre evaluée dans la Liste d'alphabet*)
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


 (*
  * @requires : [l_cmds] Liste de commandes et [l_num_roue] Liste des roues.
  * @ensures : Rajoute à la Liste de commandes le numéro de roue à utiliser (notamment après le 'S' implémenté dans le 'traite_msg_n')
  *)
 let rec ajoute_change_roue l_cmds l_num_roue =
   match l_num_roue with
    |[] -> l_cmds
    |n :: l_num_roue -> ajoute_change_roue (n::l_cmds) l_num_roue
 ;;


 (**
  * @requires : [liste_roues] Liste avec numéros de roues, [ind_roue] indice d'une roue et [new_contenu]
  * @ensures : Remplace la roue et Retourne une liste.
  *)
 let remplace_roue liste_roues ind_roue new_contenu =
   let rec loop liste_roues ind acc =
     match liste_roues with
      |[] -> List.rev acc
      |old_contenu :: liste_roues ->
        loop liste_roues (ind + 1) ((if ind = ind_roue then new_contenu else old_contenu)::acc )
   in loop liste_roues 0 []
 ;;


 (**
  * @requires : [message] Liste de caractères à traiter, 
  *             [taille_msg] sa taille, [l_alph] Liste de 3-uplets (lettre alphabet, position horaire, position antihoraire) et
  *             [nb_anten] nombre d'antennes (roues) à utiliser.
  * @ensures : Traitement du message avec plusieurs roues et Retourne une Liste avec les commandes 'N', 'P', 'E' et 'Sn'.
  *)
 let traite_msg_n message taille_msg l_alph nb_anten =
    (**
      * @requires : [lettre_but] lettre à viser, [max_roue] nombre de roues, 
      *             [ant_courante] anciene position courante, [liste_roues] Liste avec num de roues,
      *             [ind_roues] indice d'une roue, [min_dep] distance minimal,
      *             [min_sens] commande du sens N ou P, [min_roue] indice minimum de roue
      * @ensures : Retourne un 3-uplet (numéro roue minimum, distance minimale, commande 'N' ou 'P' répresentant le sens)
      *)
    let rec trouve_meilleure_roue lettre_but max_roue ant_courante liste_roues ind_roue min_dep min_sens min_roue =
      if ind_roue >= max_roue then min_roue, min_dep, min_sens
      else (
        match liste_roues with 
        |[]-> failwith "Liste vide!"
        |courant::liste_roues -> 
              let _, p_hor_0, p_antihor_0 = courant in
              let _, p_hor, p_antihor = lettre_but in

              (* depl_hor : prend la distance dans le sens horaire à partir de la dernière lettre evaluée *)
              let depl_hor = distance_hor p_antihor p_antihor_0 in 

              (* depl_antihor : prend la distance dans le sens anti-horaire à partir de la dernière lettre evaluée *)
              let depl_antihor = distance_hor p_hor p_hor_0 in
              
              (* (dep, sens) : Récupère un couple (déplacement, commande qui indique le sens) *)
              let dep, sens =
                if depl_hor < depl_antihor then depl_hor, 'N'
                else depl_antihor, 'P'
              in
              if (dep < min_dep) || (dep = min_dep && ant_courante = ind_roue) then (
                trouve_meilleure_roue lettre_but max_roue ant_courante liste_roues (ind_roue + 1) dep sens ind_roue
              )
              else (
                trouve_meilleure_roue lettre_but max_roue ant_courante liste_roues (ind_roue + 1) min_dep min_sens min_roue
              )
      )   
    in

    let rec loop message liste_roues cmds max_roue ant_courante =
      match message with
        |[] -> List.rev cmds
        |lettre :: message ->
              (* lettre_but : tuple corrrespondant à la lettre courante du message*)
              let lettre_but = List.nth l_alph (char_position lettre) in
              let roue, dep, sens = trouve_meilleure_roue lettre_but max_roue ant_courante liste_roues 0 28 '_' (-1) in
              let max_roue = 
                if (max_roue - roue = 1) && (max_roue < nb_anten) then max_roue+1
                else max_roue
              in
              let cmds =
                if roue = ant_courante then cmds
                else let liste_chars = split_string (string_of_int roue) in 
                  ajoute_change_roue ('S'::cmds) liste_chars
              in
              let liste_roues = remplace_roue liste_roues roue lettre_but 
    in loop message liste_roues (ajoute_debut cmds sens dep) max_roue roue in

    (**
      * @requires : Nombre d'antennes, tuplet (lettre, positio horaire, pos antihoraire), Liste.
      * @ensures :  Initialiser la Liste avec des roues.
      *)
    let rec init_liste_roues nb_anten elem acc =
      if nb_anten = 0 then acc
      else init_liste_roues (nb_anten - 1) elem (elem::acc) 
    in
      let liste_roues = init_liste_roues nb_anten (List.hd l_alph) [] 
    
    in (* let liste_roues = List.init nb_anten (fun _ -> List.hd l_alph) in *)(loop message liste_roues [] 1 0) (* <*)

 ;;




(*************************** P R O G R A M M E ***********************************)

(* BIENVENUE *)

Printf.printf "\n\t====================================================================\n";;
Printf.printf "\n\t\tBIENVENUE A LA COMMUNICATION AVEC PLUSIEURS ANTENNES (Phase 2)\n";;
Printf.printf "\n\t====================================================================\n\n";;


(* TRAITEMENTS *)

print_string "\n\n\n- Saisir 2 lignes :\n  | Nombre d'antennes à utiliser (roues).\n  | Le message à trasmettre.\n\n";;
let couple = parse_input ();;


(* Traitement de valeurs saisies *)
let l_message = split_string (snd couple);;
let taille_msg = longueur_liste l_message;;

(* Structure l'alphabet d'une roue : Liste de 3-uplets (lettre, position horaire, position antihoraire)  *)
let l_alphabet = [(' ',0,0); ('A',1,26); ('B',2,25); ('C',3,24); ('D',4,23); ('E',5,22); ('F',6,21) ; ('G',7,20) ; ('H',8,19) ; ('I',9,18) ; ('J',10,17) ; ('K',11,16) ; ('L',12,15) ; ('M',13,14) ; ('N',14,13) ; ('O',15,12) ; ('P',16,11) ; ('Q',17,10) ; ('R',18,9) ; ('S',19,8) ; ('T',20,7) ; ('U',21,6) ; ('V',22,5) ; ('W',23,4) ; ('X',24,3) ; ('Y',25,2) ; ('Z',26,1)];;

(* Traitement de message en fonction d'antennes saisies *)
let nb_anten = fst couple in
  traite_anten nb_anten;

  if (nb_anten=1) then (

    (* Suite de commandes pour 1 antenne*)
    let liste_cmds = traite_msg_1 l_message taille_msg l_alphabet in

      (* Impresion Liste de commandes *)
      Printf.printf "\n- Les commandes permettant l'émission de votre message ('%s') sont : \n\n" (snd couple);
      print_liste_char liste_cmds;
      
      (* Impresion Temps d'éxecution à partir de la Liste de commandes*)
      print_string "\n\n- Temps d'éxecution : " ;
      let tmp = somme_liste (List.map cal_tmp liste_cmds) in 
        print_int tmp;
        print_string " secondes.\n\n"
  )
  else(

    (* Suite de commandes pour 'n' antennes *)
    let liste_cmds = traite_msg_n l_message taille_msg l_alphabet nb_anten in

      (* Impresion Liste de commandes *)
      Printf.printf "\n- Les commandes permettant l'émission de votre message ('%s') sont : \n\n" (snd couple);
      print_liste_char liste_cmds;

      (* Impresion Temps d'éxecution à partir de la Liste de commandes*)
      print_string "\n\n- Temps d'éxecution : " ;
      let tmp = somme_liste (List.map cal_tmp liste_cmds) in 
        print_int tmp;
        print_string " secondes.\n\n"     
  )
;;


(* FERMETURE *)

Printf.printf "\n\n\t====================================================================\n";;
Printf.printf "\n\t\tFIN DE COMMUNICATION AVEC PLUSIEURS ANTENNES\n";;
Printf.printf "\n\t====================================================================\n\n";;
