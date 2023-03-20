open Mahjong

let init_state = Mahjong.Game.setup_game
let wind = Mahjong.Game.round_wind init_state
let gui = Mahjong.Gui.render
