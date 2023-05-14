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
        if x - y = 1 then Sequence (t, a, b)
        else if y - x = 2 then Sequence (a, b, t)
        else Invalid
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
            else Invalid
        | _ -> Invalid
      else Invalid
  | Pair a -> if compare_tile a t = 0 then Triple a else Invalid
  | _ -> Invalid

let count_block_honors (b : block) : int =
  match b with
  | Triple t1 -> if tile_suit t1 = Wind || tile_suit t1 = Dragon then 3 else 0
  | Pair t1 -> if tile_suit t1 = Wind || tile_suit t1 = Dragon then 2 else 0
  | Single t1 -> if tile_suit t1 = Wind || tile_suit t1 = Dragon then 1 else 0
  | _ -> 0

let count_block_terminals (b : block) : int =
  match b with
  | Triple t1 ->
      if tile_value t1 = Integer 1 || tile_value t1 = Integer 9 then 3 else 0
  | Pair t1 ->
      if tile_value t1 = Integer 1 || tile_value t1 = Integer 9 then 2 else 0
  | Single t1 ->
      if tile_value t1 = Integer 1 || tile_value t1 = Integer 9 then 1 else 0
  | Sequence (t1, t2, t3) ->
      if tile_value t1 = Integer 1 || tile_value t3 = Integer 9 then 1 else 0
  | Ryanmen (t1, t2) ->
      if tile_value t1 = Integer 1 || tile_value t2 = Integer 9 then 1 else 0
  | _ -> 0

let dragon_triplet (b : block) : bool =
  match b with
  | Triple t1 -> if tile_suit t1 = Dragon then true else false
  | _ -> false

let wind_triplet (b : block) : bool =
  match b with
  | Triple t1 -> if tile_suit t1 = Wind then true else false
  | _ -> false

let prevalent_wind_triplet (b : block) (w : direction) : bool =
  match b with
  | Triple t1 ->
      if tile_suit t1 = Wind && tile_value t1 = Direction w then true else false
  | _ -> false

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
      else if c.wind = updated_player.wind then (a, b, updated_player, d)
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
  let drawn_string =
    match drawn_tile with
    | [] -> ""
    | _ -> string_of_tile (List.nth drawn_tile 0)
  in
  let _ = print_endline ("Your Draw: " ^ drawn_string) in
  let _ = print_endline "Please choose a tile to discard." in
  let user_input = read_line () in
  let user_tile = tile_of_string user_input in
  (sort_tiles (remove_from_list hand [] user_tile), user_tile)

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
let setup_game () =
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

let string_of_wind (d : direction) : string =
  match d with
  | East -> "East"
  | West -> "West"
  | North -> "North"
  | South -> "South"

(* does not properly work with dead wall *)
let draw_tile board wind from_dead =
  let player_to_draw = determine_player board.players wind in
  let updated_player_wall =
    let wall_to_draw =
      if from_dead then board.dead_wall else board.wall.tiles
    in
    match wall_to_draw with
    | [] ->
        print_endline "You ran out of tiles! Nobody wins";
        raise OutOfTiles
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
  let discard_result = discard_tile_helper player_hand.tiles player_hand.draw in
  let new_hand =
    {
      draw = None;
      tiles = fst discard_result;
      melds = player_to_discard.hand.melds;
    }
  in
  let new_player =
    {
      hand = new_hand;
      points = player_to_discard.points;
      riichi = player_to_discard.riichi;
      wind = player_to_discard.wind;
      discards = snd discard_result :: player_to_discard.discards;
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
let get_wall state = state.wall
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

let discard_tile_helper_2 (inp : string) hand drawn_tile =
  let drawn_tile =
    match drawn_tile with
    | Some drawn_tile -> [ drawn_tile ]
    | None -> []
  in
  let hand = sort_tiles (drawn_tile @ hand) in
  let curr_hand = string_of_hand hand in
  let _ = print_hand curr_hand in
  let drawn_string =
    match drawn_tile with
    | [] -> ""
    | _ -> string_of_tile (List.nth drawn_tile 0)
  in
  let _ = print_endline ("Your Draw: " ^ drawn_string) in
  let _ = print_endline "Please choose a tile to discard." in
  let user_tile = tile_of_string inp in
  (sort_tiles (remove_from_list hand [] user_tile), user_tile)

let discard_tile_gui inp_tile board wind =
  let player_to_discard = determine_player board.players wind in
  let player_hand = player_to_discard.hand in
  let discard_result =
    discard_tile_helper_2 inp_tile player_hand.tiles player_hand.draw
  in
  let new_hand =
    {
      draw = None;
      tiles = fst discard_result;
      melds = player_to_discard.hand.melds;
    }
  in
  let new_player =
    {
      hand = new_hand;
      points = player_to_discard.points;
      riichi = player_to_discard.riichi;
      wind = player_to_discard.wind;
      discards = snd discard_result :: player_to_discard.discards;
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

let is_sequence (b : block) : bool =
  match b with
  | Sequence _ -> true
  | _ -> false

let is_triple (b : block) : bool =
  match b with
  | Triple _ -> true
  | _ -> false

let is_pair (b : block) : bool =
  match b with
  | Pair _ -> true
  | _ -> false

[@@@warning "-8"]

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

[@@@warning "+8"]

(*Combines tile t with all blocks in b and returns a list of any newly combined
  blocks*)
let rec combine_block_list (t : tile) (b : block list) (nbl : block list) :
    block list =
  match b with
  | [] -> nbl
  | h :: tl ->
      let nb = combine_with_kanchan h t in
      if nb = invalid_block then combine_block_list t tl nbl
      else combine_block_list t tl (nb :: nbl)

(*Generates a set of all blocks present in a tile list*)
let rec generate_blocks (h : tile list) (ans : block list) : block list =
  match h with
  | [] -> ans
  | hb :: t ->
      let c = combine_block_list hb ans [] in
      generate_blocks t (create_single hb :: (c @ ans))

let rec generate_melds_helper (t : tile) (bl : block list)
    (meld_list : block list) : block list =
  match bl with
  | [] -> meld_list
  | h :: tl -> (
      let meld = combine_with_kanchan h t in
      match meld with
      | Sequence _ | Triple _ -> generate_melds_helper t tl (meld :: meld_list)
      | _ -> generate_melds_helper t tl meld_list)

let generate_melds (t : tile) (h : tile list) : block list =
  let bl = generate_blocks h [] in
  generate_melds_helper t bl []

let rec check_chii melds =
  match melds with
  | [] -> false
  | hd :: tl -> (
      match hd with
      | Sequence _ -> true
      | _ -> check_chii tl)

let rec check_pon melds =
  match melds with
  | [] -> false
  | hd :: tl -> (
      match hd with
      | Triple _ -> true
      | _ -> check_pon tl)

let rec small_remove_from_list lst itm acc =
  match lst with
  | [] -> acc
  | hd :: tl ->
      if hd = itm then tl @ acc else small_remove_from_list tl itm (hd :: acc)

let rec remove_list_from_list hnd mld acc =
  match hnd with
  | [] -> acc
  | hd :: tl ->
      if List.mem hd mld then
        let new_mld = small_remove_from_list mld hd [] in
        remove_list_from_list tl new_mld acc
      else remove_list_from_list tl mld (hd :: acc)

[@@@warning "-8"]

let meld_of_block b =
  match b with
  | Sequence (x, y, z) -> Chi (x, y, z)
  | Triple x -> Pon (x, x, x)

[@@@warning "+8"]

let rec meld_chii melds (hand : hand) =
  let possible_meld, rest =
    match melds with
    | [] -> (Invalid, [])
    | hd :: tl -> (hd, tl)
  in
  if possible_meld = Invalid then hand
  else
    let item =
      match possible_meld with
      | Sequence (x, y, z) -> [ x; y; z ]
      | _ -> []
    in
    if List.length item = 0 then meld_chii rest hand
    else
      let _ =
        print_endline ("Make the sequence, " ^ string_of_list item ^ "? [y/n]")
      in
      let user_input = read_line () in
      if user_input = "y" then
        let new_hand = remove_list_from_list (closed_hand_tiles hand) item [] in
        {
          draw = None;
          tiles = new_hand;
          melds = meld_of_block possible_meld :: hand.melds;
        }
      else meld_chii rest hand

let rec meld_pon melds (hand : hand) =
  let possible_meld, rest =
    match melds with
    | [] -> (Invalid, [])
    | hd :: tl -> (hd, tl)
  in
  if possible_meld = Invalid then hand
  else
    let item =
      match possible_meld with
      | Triple x -> [ x; x; x ]
      | _ -> []
    in
    if List.length item = 0 then meld_pon rest hand
    else
      let _ =
        print_endline ("Make the triple, " ^ string_of_list item ^ "? [y/n]")
      in
      let user_input = read_line () in
      if user_input = "y" then
        let new_hand = remove_list_from_list (closed_hand_tiles hand) item [] in
        {
          draw = None;
          tiles = new_hand;
          melds = meld_of_block possible_meld :: hand.melds;
        }
      else meld_pon rest hand

let run_chii melds hand discarded_tile wind =
  let _ =
    print_endline
      ("Would you like to chii on tile: "
      ^ string_of_tile discarded_tile
      ^ "? [y/n]")
  in
  let user_input = read_line () in
  if user_input = "y" then (true, wind, meld_chii melds hand)
  else (false, wind, hand)

let run_pon melds hand discarded_tile wind =
  let _ =
    print_endline
      ("Would you like to pon on tile: "
      ^ string_of_tile discarded_tile
      ^ "? [y/n]")
  in
  let user_input = read_line () in
  if user_input = "y" then (true, wind, meld_pon melds hand)
  else (false, wind, hand)

let run_one_pon player3 wind3 melds3 discarded_tile =
  match check_pon melds3 with
  | true -> (
      let pon_result = run_pon melds3 player3 discarded_tile wind3 in
      match triple_fst pon_result with
      | true -> pon_result
      | false -> (false, wind3, player3))
  | false -> (false, wind3, player3)

let run_two_pon (player2, player3) (wind2, wind3) (melds2, melds3)
    discarded_tile =
  match check_pon melds2 with
  | true -> (
      let pon_result = run_pon melds2 player2 discarded_tile wind2 in
      match triple_fst pon_result with
      | true -> pon_result
      | false -> run_one_pon player3 wind3 melds3 discarded_tile)
  | false -> run_one_pon player3 wind3 melds3 discarded_tile

let run_three_pon (player1, player2, player3) (wind1, wind2, wind3)
    (melds1, melds2, melds3) discarded_tile =
  match check_pon melds1 with
  | true -> (
      let pon_result = run_pon melds1 player1 discarded_tile wind1 in
      match triple_fst pon_result with
      | true -> pon_result
      | false ->
          run_two_pon (player2, player3) (wind2, wind3) (melds2, melds3)
            discarded_tile)
  | false ->
      run_two_pon (player2, player3) (wind2, wind3) (melds2, melds3)
        discarded_tile

let run_chii_or_pon (player1, player2, player3) (wind1, wind2, wind3)
    (melds1, melds2, melds3) discarded_tile =
  match check_chii melds1 with
  | true -> (
      let chii_result = run_chii melds1 player1 discarded_tile wind1 in
      match triple_fst chii_result with
      | true -> chii_result
      | false ->
          run_three_pon
            (player1, player2, player3)
            (wind1, wind2, wind3) (melds1, melds2, melds3) discarded_tile)
  | false ->
      run_three_pon
        (player1, player2, player3)
        (wind1, wind2, wind3) (melds1, melds2, melds3) discarded_tile

let melding_helper board discarded_tile wind =
  let winds =
    match wind with
    | East -> (South, West, North)
    | South -> (West, North, East)
    | North -> (East, South, West)
    | West -> (North, East, South)
  in
  let first_player = (get_player board (triple_fst winds)).hand in
  let first_player_melds =
    generate_melds discarded_tile (closed_hand_tiles first_player)
  in
  let second_player = (get_player board (triple_snd winds)).hand in
  let second_player_melds =
    generate_melds discarded_tile (closed_hand_tiles second_player)
  in
  let third_player = (get_player board (triple_third winds)).hand in
  let third_player_melds =
    generate_melds discarded_tile (closed_hand_tiles third_player)
  in
  let meld_result =
    run_chii_or_pon
      (first_player, second_player, third_player)
      winds
      (first_player_melds, second_player_melds, third_player_melds)
      discarded_tile
  in
  if triple_fst meld_result = false then
    {
      wall = board.wall;
      dead_wall = board.dead_wall;
      dora = board.dora;
      hidden_dora = board.hidden_dora;
      players = board.players;
      round = board.round;
      wind = board.wind;
    }
  else
    let meld_wind, new_hand =
      match meld_result with
      | _, y, z -> (y, z)
    in
    let old_player = get_player board meld_wind in
    let new_player =
      {
        hand = new_hand;
        points = old_player.points;
        riichi = old_player.riichi;
        wind = old_player.wind;
        discards = old_player.discards;
      }
    in
    let old_original_player = get_player board wind in
    let updated_discards =
      match old_original_player.discards with
      | [] -> []
      | hd :: tl -> tl
    in
    let updated_player =
      {
        hand = old_original_player.hand;
        points = old_original_player.points;
        riichi = old_original_player.riichi;
        wind = old_original_player.wind;
        discards = updated_discards;
      }
    in
    let temp_board_state =
      {
        wall = board.wall;
        dead_wall = board.dead_wall;
        dora = board.dora;
        hidden_dora = board.hidden_dora;
        players =
          update_players
            (update_players board.players updated_player)
            new_player;
        round = board.round;
        wind = board.wind;
      }
    in
    discard_tile temp_board_state meld_wind

(* FOR GUI SUPPORT, THIS IS THE FUNCTION THAT IS CALLED, SO I THINK THIS IS THE
   ONE THAT NEEDS THE STRING INPUT, THEN PASS IT INTO THE HELPER FUNCTION, WHICH
   CAN THEN USE IN FOR THE GUI SUPPORTED DISCARDING *)
let melding board wind =
  let curr_player = get_player board wind in
  let discarded_tile = List.nth curr_player.discards 0 in
  melding_helper board discarded_tile wind
