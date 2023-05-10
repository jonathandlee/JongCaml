open Mahjong
open Game

let init_state = Mahjong.Game.setup_game ()
let wind = Mahjong.Game.round_wind init_state
let p = Mahjong.Game.get_player init_state wind
let hand = Mahjong.Game.hand_of_player p

let next_player (p : direction) =
  match p with
  | East -> West
  | North -> West
  | West -> South
  | South -> East

(* let gui = Mahjong.Gui.render init_state *)

let curr_state = ref init_state
(* let main () = Mahjong.Gui.render init_state *)

let rec build_board (game : state) wind =
  let game = draw_tile game wind false in
  build_board (Gui.create_board_2 game false wind) (next_player wind)

let () = curr_state := build_board init_state wind

(* let rec simgame (game : state) wind = Mahjong.Gui.render game true East; let
   b = discard_tile (draw_tile game wind false) wind in simgame b wind

   let () = simgame init_state wind *)
