open Bogue
open Main
open Tsdl
open Game
module W = Widget
module L = Layout
module T = Trigger

let thick_grey_line =
  Style.mk_line ~color:Draw.(opaque grey) ~width:3 ~style:Solid ()

let round_blue_box =
  let open Style in
  let border = mk_border ~radius:25 thick_grey_line in
  create ~border
    ~background:(color_bg Draw.(opaque @@ find_color "lightblue"))
    ()

let shadow_box =
  let open Style in
  create ~background:(opaque_bg Draw.blue)
    ~shadow:(mk_shadow ~width:400 ~radius:400 ())
    ()

let string_of_tile (x : tile) = tile_suit x
let dir = Sys.getcwd ()
let bg = dir ^ "/" ^ "newtiles" ^ "/" ^ "tony.jpg"
let hand_to_widget (h : string) = W.image (dir ^ "/" ^ h ^ ".png")
let hand_to_widgets (h : string list) : W.t list = List.map hand_to_widget h
