open Mahjong
open Game

let init_state = Mahjong.Game.setup_game
let wind = Mahjong.Game.round_wind init_state
let p = Mahjong.Game.get_player init_state wind
let hand = Mahjong.Game.hand_of_player p
let gui = Mahjong.Gui.render init_state
let w = Mahjong.Gui.wipe
let init_state = setup_game
let wind = round_wind init_state
let p = get_player init_state wind
let hand = hand_of_player p
let ns = draw_tile init_state (round_wind init_state) false
let nz = discard_tile ns
let main () = discard_tile (draw_tile init_state (round_wind init_state) false)
