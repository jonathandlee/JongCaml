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
  wind : direction;
  discards : tile list;
}

type updated_player_wall_data = {
  player : player;
  wall : wall;
}

type state = {
  wall : wall;
  dead_wall : tile list;
  dora : tile list;
  hidden_dora : tile list;
  players : player * player * player * player;
  round : int;
  wind : direction;
}

exception OutOfTiles

(** Helper Functions*)

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

(** Manipulating triples (tiles)*)
let triple_fst (x, y, z) = x

let triple_snd (x, y, z) = y
let triple_third (x, y, z) = z
let swap lst a b = swap_helper lst lst a b 0 []

(** Maniupulating quads (players) *)
let quad_fst (w, x, y, z) = w

let quad_snd (w, x, y, z) = x
let quad_trd (w, x, y, z) = y
let quad_frth (w, x, y, z) = z

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

(** Shuffle tiles (this creates the wall)*)

(* n log(n) time *)
let shuffle tiles =
  let x = List.map (fun y -> (Random.bits (), y)) tiles in
  let z = List.sort compare x in
  List.map snd z

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

let compare_suit (a : suit) (b : suit) : int =
  let x =
    match a with
    | Pin -> 1
    | Man -> 2
    | Sou -> 3
    | Wind -> 4
    | Dragon -> 5
  in
  let y =
    match b with
    | Pin -> 1
    | Man -> 2
    | Sou -> 3
    | Wind -> 4
    | Dragon -> 5
  in
  x - y

let compare_direction (a : direction) (b : direction) : int =
  let x =
    match a with
    | East -> 1
    | South -> 2
    | West -> 3
    | North -> 4
  in
  let y =
    match b with
    | East -> 1
    | South -> 2
    | West -> 3
    | North -> 4
  in
  x - y

let compare_color (a : color) (b : color) : int =
  let x =
    match a with
    | White -> 1
    | Green -> 2
    | Red -> 3
  in
  let y =
    match b with
    | White -> 1
    | Green -> 2
    | Red -> 3
  in
  x - y

(** Comparison function for tiles. Trust me this function is safe*)

[@@@warning "-8"]

let compare_tile (a : tile) (b : tile) : int =
  let x = compare_suit (triple_fst a) (triple_fst b) in
  if x = 0 then
    match triple_snd a with
    | Integer x ->
        compare x
          (let (Integer y) = triple_snd b in
           y)
    | Direction d ->
        compare_direction d
          (let (Direction y) = triple_snd b in
           y)
    | Color c ->
        compare_color c
          (let (Color y) = triple_snd b in
           y)
  else x

[@@@warning "+8"]

let sort_tiles tiles = List.stable_sort compare_tile tiles

let rec sublist start_pos end_pos list =
  match list with
  | [] -> raise OutOfTiles
  | hd :: tl ->
      let tail =
        if end_pos = 0 then [] else sublist (start_pos - 1) (end_pos - 1) tl
      in
      if start_pos > 0 then tail else hd :: tail

let setup_game tiles =
  let dead_wall = sublist 0 13 tiles in
  let player_1 =
    {
      hand = { tiles = sort_tiles (sublist 14 26 tiles); melds = [] };
      points = 25000;
      riichi = false;
      wind = East;
      discards = [];
    }
  in
  let player_2 =
    {
      hand = { tiles = sort_tiles (sublist 27 39 tiles); melds = [] };
      points = 25000;
      riichi = false;
      wind = South;
      discards = [];
    }
  in
  let player_3 =
    {
      hand = { tiles = sort_tiles (sublist 40 52 tiles); melds = [] };
      points = 25000;
      riichi = false;
      wind = West;
      discards = [];
    }
  in
  let player_4 =
    {
      hand = { tiles = sort_tiles (sublist 53 65 tiles); melds = [] };
      points = 25000;
      riichi = false;
      wind = North;
      discards = [];
    }
  in
  let dora = sublist 0 4 dead_wall in
  let hidden_dora = sublist 5 9 dead_wall in
  let wall = { tiles = sublist 66 135 tiles; position = 0 } in
  {
    wall;
    dead_wall = sublist 10 13 dead_wall;
    dora;
    hidden_dora;
    players = (player_1, player_2, player_3, player_4);
    round = 4;
    wind = East;
  }

let update_players (players : player * player * player * player)
    (updated_player : player) =
  match players with
  | a, b, c, d ->
      if a.wind = updated_player.wind then (updated_player, b, c, d)
      else if b.wind = updated_player.wind then (a, updated_player, c, d)
      else (a, b, c, updated_player)

(* TODO: This does not take into account drawing from dead wall *)
let draw_tile board wind =
  let player_to_draw =
    match board.players with
    | a, b, c, d ->
        if a.wind = wind then a
        else if b.wind = wind then b
        else if c.wind = wind then c
        else d
  in
  let updated_player_wall =
    match board.wall.tiles with
    | [] -> raise OutOfTiles
    | hd :: tl ->
        let new_hand =
          {
            tiles = hd :: player_to_draw.hand.tiles;
            melds = player_to_draw.hand.melds;
          }
        in
        let new_wall = tl in
        let updated_data =
          {
            player =
              {
                hand = new_hand;
                points = player_to_draw.points;
                riichi = player_to_draw.riichi;
                wind = player_to_draw.wind;
                discards = player_to_draw.discards;
              };
            wall = { tiles = new_wall; position = board.wall.position + 1 };
          }
        in
        updated_data
  in
  {
    wall = updated_player_wall.wall;
    dead_wall = board.dead_wall;
    dora = board.dora;
    hidden_dora = board.hidden_dora;
    players = update_players board.players updated_player_wall.player;
    round = board.round;
    wind = board.wind;
  }
(* TODO: Deal with discarding tile from hand here *)

(* does not deal with melds *)
let rec string_of_hand tiles =
  match tiles with
  | [] -> ""
  | hd :: tl ->
      if tl = [] then string_of_tile hd
      else string_of_tile hd ^ ", " ^ string_of_hand tl

(* let discard_tile hand = *)
