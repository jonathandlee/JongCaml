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

let complete (h : hand) : bool =
  List.length
    (prep_hand_helper (open_hand_tiles h @ closed_hand_tiles h) [ [] ] 5)
  >= 1

(**
    ((pre @ [ nb ] @ t) :: subsets))
    
    (fun x -> List.length x > 0)*)
