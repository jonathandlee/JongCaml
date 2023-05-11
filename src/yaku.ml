open Game

type prep_hand = block list

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

(**
    ((pre @ [ nb ] @ t) :: subsets))
    
    (fun x -> List.length x > 0)**)

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
