open Mahjong
open Game

let init_state = Mahjong.Game.setup_game
let wind = Mahjong.Game.round_wind init_state
let p = Mahjong.Game.get_player init_state wind
let hand = Mahjong.Game.hand_of_player p
let gui = Mahjong.Gui.render (Mahjong.Game.closed_hand_tiles hand)
