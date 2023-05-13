open Game

type prep_hand = block list
type counter = (block * int) list


(*Same combine function but supports kanchans*)
let combine_with_kanchan (b : block) (t : tile) : block =
  match b with
  | Triple _ -> Invalid
  | Sequence (a, b, c) -> Invalid
  | Ryanmen (a, b) ->
      if tile_suit a = tile_suit t then
        let (Integer x) = tile_value a in
        let (Integer y) = tile_value t in
        if x - y = 1 then Sequence (t, a, b)
        else if y - x = 2 then Sequence (a, b, t)
        else Invalid
      else Invalid
  | Kanchan (a, b) ->
      if tile_suit a = tile_suit t then
        let (Integer x) = tile_value a in
        let (Integer y) = tile_value t in
        if y - x = 1 then Sequence (a, t, b) else Invalid
      else Invalid
  | Single a ->
      if compare_tile a t = 0 then Pair a
      else if tile_suit a = tile_suit t then
        match tile_suit a with
        | Pin | Man | Sou ->
            let (Integer x) = tile_value a in
            let (Integer y) = tile_value t in
            if x - y = 1 then Ryanmen (t, a)
            else if y - x = 1 then Ryanmen (a, t)
            else if x + 2 = y then Kanchan (a, t)
            else if y + 2 = x then Kanchan (t, a)
            else Invalid
        | _ -> Invalid
      else Invalid
  | Pair a -> if compare_tile a t = 0 then Triple a else Invalid
  | _ -> Invalid

let rec generate_subsets_helper (h : tile) (b : block list) (pre : block list)
    (subsets : block list list) : block list list =
  match b with
  | [] -> (create_single h :: pre) :: subsets
  | hb :: t ->
      let nb = combine hb h in
      if nb = invalid_block then
        generate_subsets_helper h t (pre @ [ hb ]) subsets
      else
        generate_subsets_helper h t (pre @ [ hb ])
          ((pre @ [ nb ] @ t) :: subsets)

let generate_subsets (h : tile) (b : block list) =
  generate_subsets_helper h b [] []

(** If you want to check if a hand is winning, set n = 5 and check that
    prep_hand_helper returns anything*)
let rec prep_hand_helper (h : tile list) (b : block list list) (n : int) :
    block list list =
  match h with
  | [] -> b
  | ht :: t ->
      prep_hand_helper t
        (List.filter
           (fun x -> List.length x <= n)
           (List.fold_right (fun x y -> generate_subsets ht x @ y) b []))
        n

let complete (h : hand) : block list option =
  try
    Some
      (List.nth
         (prep_hand_helper
            ((drawn_tile h :: open_hand_tiles h) @ closed_hand_tiles h)
            [ [] ] 5)
         0)
  with
  | Failure _ -> None
  | _ -> None

let complete_closed (h : hand) : block list option =
  try
    Some
      (List.nth
         (prep_hand_helper
            (drawn_tile h :: closed_hand_tiles h)
            [ [] ]
            (((List.length (closed_hand_tiles h) - 2) / 3) + 1))
         0)
  with
  | Failure _ -> None
  | _ -> None

(**
    ((pre @ [ nb ] @ t) :: subsets))
    
    (fun x -> List.length x > 0)**)

(* Helper functions for checking the types of blocks*)

(*Scuffed code reuse, TODO: add to game.mli, remove*)
let triple_fst (x, y, z) = x
let triple_snd (x, y, z) = y
let triple_third (x, y, z) = z

let get_block_suit (b : block) : suit =
  match b with
  | Triple t1
  | Sequence (t1, _, _)
  | Ryanmen (t1, _)
  | Kanchan (t1, _)
  | Penchan (t1, _)
  | Single t1
  | Pair t1 -> tile_suit t1
  | Invalid -> raise (Failure "invalid block")

(* Requires: block does not contain honor tiles*)
let compare_block_values (ba : block) (bb : block) : bool =
  match (ba, bb) with
  | Triple t1, Triple t2 -> tile_value t1 = tile_value t2
  | Sequence (t1a, _, t3a), Sequence (t1b, _, t3b) ->
      tile_value t1a = tile_value t1b && tile_value t3a = tile_value t3b
  | Pair t1, Pair t2 -> tile_value t1 = tile_value t2
  | _ -> false

(*Checks if non-honor blocks are equal but ignores whether dora or not*)
let compare_block_equal (ba : block) (bb : block) : bool =
  match (ba, bb) with
  | Triple t1, Triple t2 ->
      tile_value t1 = tile_value t2 && tile_suit t1 = tile_suit t2
  | Sequence (t1a, _, t3a), Sequence (t1b, _, t3b) ->
      tile_value t1a = tile_value t1b
      && tile_value t3a = tile_value t3b
      && tile_suit t1a = tile_suit t1b
  | Pair t1, Pair t2 ->
      tile_value t1 = tile_value t2 && tile_suit t1 = tile_suit t2
  | _ -> false

(*All functions past this line will be towards calculating yaku*)

let count_honors (b : block list) : int =
  List.fold_left (fun count x -> count + count_block_honors x) 0 b

let count_terminals (b : block list) : int =
  List.fold_left (fun count x -> count + count_block_terminals x) 0 b

let check_tanyao (b : block list) : bool =
  if count_honors b = 0 && count_terminals b = 0 then true else false

let check_triplet_dragons (b : block list) : int =
  List.fold_left (fun count x -> count + if dragon_triplet x then 1 else 0) 0 b

let check_triplet_winds (b : block list) : int =
  List.fold_left (fun count x -> count + if wind_triplet x then 1 else 0) 0 b

let check_tsumo (h : hand) : bool = if h.melds = [] then true else false

(*Assoc list of blocks to number of times they appear in the hand*)
let rec assoc_populate (b : block list) (b_assoc : counter) : counter =
  match b with
  | [] -> b_assoc
  | h :: t ->
      if List.mem_assoc h b_assoc then
        assoc_populate t ((h, List.assoc h b_assoc + 1) :: b_assoc)
      else assoc_populate t ((h, 1) :: b_assoc)

(**Note: Menzenchin only*)
let check_iipeikou (b : block list) : int =
  let b_assoc = assoc_populate b [] in
  List.fold_left
    (fun count x ->
      count
      +
      if is_sequence x then
        if List.assoc x b_assoc = 4 then 2
        else if List.assoc x b_assoc >= 2 then 1
        else 0
      else 0)
    0 b

(* Checks if a block list has sanshoku for a block pattern (a block with value
   only). If the returning triple has all bools, that means sanshoku is
   fulfilled.*)
let rec check_sanshoku (b : block list) (b_p : block)
    (suits : bool * bool * bool) : bool * bool * bool =
  match b with
  | [] -> suits
  | h :: t ->
      if compare_block_values h b_p then
        match get_block_suit h with
        | Pin ->
            check_sanshoku t b_p (true, triple_snd suits, triple_third suits)
        | Man ->
            check_sanshoku t b_p (triple_fst suits, true, triple_third suits)
        | Sou -> check_sanshoku t b_p (triple_fst suits, triple_snd suits, true)
        | _ -> check_sanshoku t b_p suits
      else check_sanshoku t b_p suits

let rec check_triple_triplets_helper (b : block list) (original_b : block list)
    : bool =
  match b with
  | [] -> false
  | h :: t ->
      if
        is_triple h
        && check_sanshoku original_b h (false, false, false) = (true, true, true)
      then true
      else check_triple_triplets_helper t original_b

let check_triple_triplets (b : block list) : bool =
  check_triple_triplets_helper b b

let rec check_all_triplets_helper (b : block list) (triplets : int) (pair : int)
    : int * int =
  match b with
  | [] -> (triplets, pair)
  | h :: t ->
      if is_triple h then check_all_triplets_helper t (triplets + 1) pair
      else if is_pair h then check_all_triplets_helper t triplets (pair + 1)
      else check_all_triplets_helper t triplets pair

let check_all_triplets (b : block list) : bool =
  if check_all_triplets_helper b 0 0 = (4, 1) then true else false

let check_all_concealed_triplets (h : hand) : bool =
  let sb = complete_closed h in
  match sb with
  | Some b -> fst (check_all_triplets_helper b 0 0) = 3
  | None -> raise (Failure "invalid hand")

let rec has_pair_suit (b : block list) (s : suit) : bool =
  match b with
  | [] -> false
  | h :: t ->
      if is_pair h && get_block_suit h = s then true else has_pair_suit t s

let check_shousangen (b : block list) : bool =
  if check_triplet_dragons b = 2 && has_pair_suit b Dragon then true else false

let check_honroutou (b : block list) : bool =
  if count_honors b + count_terminals b = 14 then true else false

let rec check_chiitoitsu (b : block list) : bool =
  match b with
  | [] -> true
  | h :: t -> if is_pair h then check_chiitoitsu t else false

let rec check_chanta (b : block list) : bool =
  match b with
  | [] -> true
  | h :: t ->
      if count_block_honors h > 0 || count_block_terminals h > 0 then
        check_chanta b
      else false

(* Yeah im hardcoding this *)

let pin_111 : block =
  Sequence
    ((Pin, Integer 1, false), (Pin, Integer 1, false), (Pin, Integer 1, false))

let pin_19 : block list =
  [
    Sequence
      ((Pin, Integer 1, false), (Pin, Integer 2, false), (Pin, Integer 3, false));
    Sequence
      ((Pin, Integer 4, false), (Pin, Integer 5, false), (Pin, Integer 6, false));
    Sequence
      ((Pin, Integer 7, false), (Pin, Integer 8, false), (Pin, Integer 9, false));
  ]

let man_19 : block list =
  [
    Sequence
      ((Man, Integer 1, false), (Man, Integer 2, false), (Man, Integer 3, false));
    Sequence
      ((Man, Integer 4, false), (Man, Integer 5, false), (Man, Integer 6, false));
    Sequence
      ((Man, Integer 7, false), (Man, Integer 8, false), (Man, Integer 9, false));
  ]

let sou_19 : block list =
  [
    Sequence
      ((Sou, Integer 1, false), (Sou, Integer 2, false), (Sou, Integer 3, false));
    Sequence
      ((Sou, Integer 4, false), (Sou, Integer 5, false), (Sou, Integer 6, false));
    Sequence
      ((Sou, Integer 7, false), (Sou, Integer 8, false), (Sou, Integer 9, false));
  ]

(* checks if block list contains block b*)
let rec contains_block (bl : block list) (b : block) : bool =
  match bl with
  | [] -> false
  | h :: t -> if compare_block_equal h b then true else contains_block t b

(* checks if complete block list bl contains sub block list b*)
let rec contains_block_list (bl : block list) (b : block list) : bool =
  match b with
  | [] -> true
  | h :: t -> if contains_block bl h then contains_block_list bl t else false

let check_ittsu (b : block list) : bool =
  (* Lets check if pin 1 - 9 works!*)
  if contains_block_list b pin_19 then true
  else if contains_block_list b man_19 then true
  else if contains_block_list b sou_19 then true
  else false

(* nooooo i hate code duplication!!!!*)
let rec check_sanshiki_helper (b : block list) (original_b : block list) : bool
    =
  match b with
  | [] -> false
  | h :: t ->
      if
        is_sequence h
        && check_sanshoku original_b h (false, false, false) = (true, true, true)
      then true
      else check_sanshiki_helper t original_b

let check_sanshiki (b : block list) : bool = check_sanshiki_helper b b
