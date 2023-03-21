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
  draw : tile option;
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

type block =
  | Triple of tile
  | Sequence of tile * tile * tile
  | Ryanmen of tile * tile
  | Kanchan of tile * tile
  | Penchan of tile * tile
  | Single of tile
  | Pair of tile
  | Invalid

exception EmptyHand
exception OutOfTiles
exception InvalidSuit of string

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
let closed_hand_tiles h = h.tiles
let open_hand_tiles h = h.melds
let invalid_block = Invalid
let create_single t = Single t

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

let shuffle tiles =
  let x = List.map (fun y -> (Random.bits (), y)) tiles in
  let z = List.sort compare x in
  List.map snd z

let tile_suit t = triple_fst t
let tile_value t = triple_snd t

let string_of_tile (tile : tile) : string =
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

(* #load "str.cma";; *)
let string_has_int input = Str.string_match (Str.regexp "[0-9]+$") input 0

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

let value_of_string (str : string) : value =
  if string_has_int str then Integer (int_of_string str)
  else
    match str with
    | "East" -> Direction East
    | "South" -> Direction South
    | "West" -> Direction West
    | "North" -> Direction North
    | "Red" -> Color Red
    | "Green" -> Color Green
    | "White" -> Color White

let tile_of_string_red list : tile =
  let suit =
    match List.nth list 1 with
    | "Pin" -> Pin
    | "Man" -> Man
    | "Sou" -> Sou
    | "Wind" -> Wind
    | "Dragon" -> Dragon
  in
  let (value : value) = value_of_string (List.nth list 2) in
  (suit, value, true)

let tile_of_string_not_red list : tile =
  let suit =
    match List.nth list 0 with
    | "Pin" -> Pin
    | "Man" -> Man
    | "Sou" -> Sou
    | "Wind" -> Wind
    | "Dragon" -> Dragon
  in
  let (value : value) = value_of_string (List.nth list 1) in
  (suit, value, false)

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

let combine (b : block) (t : tile) : block =
  match b with
  | Triple _ -> Invalid
  | Sequence (a, b, c) -> Invalid
  | Ryanmen (a, b) ->
      if tile_suit a = tile_suit t then
        let (Integer x) = tile_value a in
        let (Integer y) = tile_value t in
        if x - y = 1 || y - x = 2 then Sequence (a, b, t) else Invalid
      else Invalid
  | Single a ->
      if compare_tile a t = 0 then Pair a
      else if tile_suit a = tile_suit t then
        match tile_suit a with
        | Wind | Dragon ->
            if compare_suit (tile_suit a) (tile_suit t) = 0 then Pair a
            else Invalid
        | Pin | Man | Sou ->
            let (Integer x) = tile_value a in
            let (Integer y) = tile_value t in
            if x - y = 1 || y - x = 1 then Ryanmen (a, t) else Invalid
      else Invalid
  | Pair a -> if compare_tile a t = 0 then Triple a else Invalid
  | _ -> Invalid

[@@@warning "+8"]

let tile_of_string (input : string) : tile =
  let subs = String.split_on_char ' ' input in
  if List.nth subs 0 = "Red" then tile_of_string_red subs
  else tile_of_string_not_red subs

let sort_tiles tiles = List.stable_sort compare_tile tiles

let rec sublist start_pos end_pos list =
  match list with
  | [] -> raise OutOfTiles
  | hd :: tl ->
      let tail =
        if end_pos = 0 then [] else sublist (start_pos - 1) (end_pos - 1) tl
      in
      if start_pos > 0 then tail else hd :: tail

let determine_player (players : player * player * player * player) wind =
  match players with
  | a, b, c, d ->
      if a.wind = wind then a
      else if b.wind = wind then b
      else if c.wind = wind then c
      else d

let update_players (players : player * player * player * player)
    (updated_player : player) =
  match players with
  | a, b, c, d ->
      if a.wind = updated_player.wind then (updated_player, b, c, d)
      else if b.wind = updated_player.wind then (a, updated_player, c, d)
      else (a, b, c, updated_player)

let rec string_of_hand (tiles : tile list) =
  match tiles with
  | [] -> ""
  | hd :: tl ->
      if tl = [] then string_of_tile hd
      else string_of_tile hd ^ ", " ^ string_of_hand tl

let print_hand hand = print_endline ("Your hand is: " ^ hand)

let rec remove_from_list (list : tile list) (new_list : tile list) (item : tile)
    =
  let new_list = if new_list = [] then [] else new_list in
  match list with
  | [] -> raise EmptyHand
  | hd :: tl ->
      if hd = item then tl @ new_list
      else remove_from_list tl (hd :: new_list) item

let discard_tile_helper hand drawn_tile =
  let drawn_tile =
    match drawn_tile with
    | Some drawn_tile -> [ drawn_tile ]
    | None -> []
  in
  let hand = sort_tiles (drawn_tile @ hand) in
  let curr_hand = string_of_hand hand in
  let _ = print_hand curr_hand in
  let _ =
    print_endline ("Your Draw: " ^ string_of_tile (List.nth drawn_tile 0))
  in
  let _ = print_endline "Please choose a tile to discard." in
  let user_input = read_line () in
  let user_tile = tile_of_string user_input in
  sort_tiles (remove_from_list hand [] user_tile)

let tiles_of_melds_helper meld =
  match meld with
  | Chi (x, y, z) -> [ x; y; z ]
  | Kan (x, _, _, _) -> [ x; x; x; x ]
  | Pon (x, _, _) -> [ x; x; x ]

let rec tiles_of_melds (melds : meld list) =
  match melds with
  | [] -> []
  | hd :: tl -> tiles_of_melds_helper hd @ tiles_of_melds tl

let rec string_of_list_helper list =
  match list with
  | [] -> ""
  | hd :: tl -> hd ^ "; " ^ string_of_list_helper tl

let string_of_list list =
  let str_lst = List.map string_of_tile list in
  let str = string_of_list_helper str_lst in
  "[" ^ str ^ "]"

let string_list_of_tile list = List.map string_of_tile list
let hand_of_player (p : player) = p.hand

(* Main Functions: *)
let setup_game =
  let tiles =
    shuffle
      (quadruple generate_all_numbers Pin
      @ quadruple generate_all_numbers Man
      @ quadruple generate_all_numbers Sou
      @ quadruple generate_all_dragons [ Red; Green; White ]
      @ quadruple generate_all_winds [ East; South; West; North ]
      @ generate_fives Pin @ generate_fives Man @ generate_fives Sou
      @ generate_red_fives [ Pin; Man; Sou ])
  in
  let dead_wall = sublist 0 13 tiles in
  let player_1 =
    {
      hand =
        { draw = None; tiles = sort_tiles (sublist 14 26 tiles); melds = [] };
      points = 25000;
      riichi = false;
      wind = East;
      discards = [];
    }
  in
  let player_2 =
    {
      hand =
        { draw = None; tiles = sort_tiles (sublist 27 39 tiles); melds = [] };
      points = 25000;
      riichi = false;
      wind = South;
      discards = [];
    }
  in
  let player_3 =
    {
      hand =
        { draw = None; tiles = sort_tiles (sublist 40 52 tiles); melds = [] };
      points = 25000;
      riichi = false;
      wind = West;
      discards = [];
    }
  in
  let player_4 =
    {
      hand =
        { draw = None; tiles = sort_tiles (sublist 53 65 tiles); melds = [] };
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

(* does not properly work with dead wall *)
let draw_tile board wind from_dead =
  let player_to_draw = determine_player board.players wind in
  let updated_player_wall =
    let wall_to_draw =
      if from_dead then board.dead_wall else board.wall.tiles
    in
    match wall_to_draw with
    | [] -> raise OutOfTiles
    | hd :: tl ->
        let new_hand =
          {
            draw = Some hd;
            tiles = player_to_draw.hand.tiles;
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

let discard_tile board wind =
  let player_to_discard = determine_player board.players wind in
  let player_hand = player_to_discard.hand in
  let new_hand =
    {
      draw = None;
      tiles = discard_tile_helper player_hand.tiles player_hand.draw;
      melds = player_to_discard.hand.melds;
    }
  in
  let new_player =
    {
      hand = new_hand;
      points = player_to_discard.points;
      riichi = player_to_discard.riichi;
      wind = player_to_discard.wind;
      discards = player_to_discard.discards;
    }
  in
  {
    wall = board.wall;
    dead_wall = board.dead_wall;
    dora = board.dora;
    hidden_dora = board.hidden_dora;
    players = update_players board.players new_player;
    round = board.round;
    wind = board.wind;
  }

let round_wind board = board.wind
let round_number board = board.round
let get_player board wind = determine_player board.players wind
let tile_dora tile = if triple_third tile = true then 1 else 0
let tiles_left wall = List.length wall.tiles
let closed_hand_tiles (hand : hand) = hand.tiles
let open_hand_tiles hand = tiles_of_melds hand.melds

let string_of_list list =
  let str_lst = List.map string_of_tile list in
  let str = string_of_list_helper str_lst in
  "[" ^ str ^ "]"

let drawn_tile hand =
  match hand.draw with
  | Some x -> x
  | None -> raise EmptyHand

(* TEST *)
let test =
  discard_tile
    (draw_tile
       (discard_tile
          (draw_tile
             (discard_tile
                (draw_tile
                   (discard_tile
                      (draw_tile
                         (discard_tile
                            (draw_tile
                               (discard_tile
                                  (draw_tile
                                     (discard_tile
                                        (draw_tile
                                           (discard_tile
                                              (draw_tile setup_game East false)
                                              East)
                                           East false)
                                        East)
                                     East false)
                                  East)
                               East false)
                            East)
                         East false)
                      East)
                   East false)
                East)
             East false)
          East)
       East false)
    East
