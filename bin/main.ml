open Mahjong
open Game

let init_state = Mahjong.Game.setup_game
let wind = Mahjong.Game.round_wind init_state
let p = Mahjong.Game.get_player init_state wind
let hand = Mahjong.Game.hand_of_player p

(* let gui = Mahjong.Gui.render init_state *)
let init_state = setup_game
let wind = round_wind init_state
let p = get_player init_state wind
(* let main () = Mahjong.Gui.render init_state *)

let rec simgame (game : state) wind =
  Mahjong.Gui.render game;
  let b = discard_tile (draw_tile game wind false) wind in
  simgame b wind

let () = simgame init_state wind
