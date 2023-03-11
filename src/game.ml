open Random

type direction =
  | North
  | South
  | East
  | West

type color =
  | Red
  | Green
  | White

type value =
  | Integer of int
  | Direction of direction
  | Color of color

type suit =
  | Pin
  | Man
  | Sou
  | Wind
  | Dragon

type tile = suit * value * bool
(** the bool is for when the tile is a red five.*)

(** Sequences are always sorted in ascending order*)
type meld =
  | Chi of (tile * tile * tile)
  | Pon of (tile * tile * tile)
  | Kan of (tile * tile * tile * tile)

type hand = {
  tiles : tile list;
  melds : meld list;
}

type wall = {
  tiles : tile list;
  position : int;
}

type player = {
  hand : hand;
  points : int;
  riichi : bool;
}

type state = {
  wall : wall;
  players : player * player * player * player;
  round : int;
  wind : suit;
}

exception OutOfTiles

let generate_all_numbers suit : tile list =
  List.map (fun x -> (suit, Integer x, false)) [ 1; 2; 3; 4; 6; 7; 8; 9 ]

let generate_fives suit : tile list =
  List.map (fun x -> (suit, Integer 5, false)) [ 1; 2; 3 ]

let generate_red_fives suit : tile list =
  List.map (fun x -> (x, Integer 5, true)) suit

let generate_all_dragons colors : tile list =
  List.map (fun x -> (Dragon, Color x, false)) colors

let generate_all_winds directions : tile list =
  List.map (fun x -> (Wind, Direction x, false)) directions

let quadruple f vals : tile list =
  List.fold_right (fun a b -> f vals @ b) [ 1; 2; 3; 4 ] []

(** Create an ordered list of every Mahjong Tile. Yes, I decided to go with this
    solution.*)
let all_tiles : tile list =
  quadruple generate_all_numbers Pin
  @ quadruple generate_all_numbers Man
  @ quadruple generate_all_numbers Sou
  @ quadruple generate_all_dragons [ Red; Green; White ]
  @ quadruple generate_all_winds [ East; South; West; North ]
  @ generate_fives Pin @ generate_fives Man @ generate_fives Sou
  @ generate_red_fives [ Pin; Man; Sou ]

(** Swap elements at position a and b. *)
let rec swap_helper original lst a b pos new_list =
  match lst with
  | [] -> new_list
  | h :: t ->
      if pos <> a && pos <> b then
        swap_helper original t a b (pos + 1) (new_list @ [ h ])
      else if pos = a then
        swap_helper original t a b (pos + 1) (new_list @ [ List.nth original b ])
      else
        swap_helper original t a b (pos + 1) (new_list @ [ List.nth original a ])

let swap lst a b = swap_helper lst lst a b 0 []

(** Shuffle tiles (this creates the wall)*)

let rec create_wall wall bound =
  if bound = 0 then { tiles = wall; position = 0 }
  else
    let x = Random.int bound in
    create_wall (swap wall x (bound - 1)) (bound - 1)

let wall_draw wall =
  match wall.tiles with
  | [] -> raise OutOfTiles
  | h :: t -> h

let wall_pop wall =
  match wall.tiles with
  | [] -> raise OutOfTiles
  | h :: t -> { tiles = t; position = wall.position + 1 }

let triple_fst (x, y, z) = x
let triple_snd (x, y, z) = y
let triple_third (x, y, z) = z

let string_of_tile tile : string =
  (if triple_third tile then "Red " else "")
  ^ (match triple_fst tile with
    | Pin -> "Pin"
    | Man -> "Man"
    | Sou -> "Sou"
    | Wind -> "Wind"
    | Dragon -> "Dragon")
  ^ " "
  ^
  match triple_snd tile with
  | Integer a -> string_of_int a
  | Direction dir -> (
      match dir with
      | North -> "North"
      | South -> "South"
      | East -> "East"
      | West -> "West")
  | Color c -> (
      match c with
      | Red -> "Red"
      | Green -> "Green"
      | White -> "White")
