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
let image_file = dir^"/"^"tony.jpg"
let bg = dir ^ "/" ^ "newtiles" ^ "/" ^ "tony.jpg"
let hand_to_widget (h : string) = W.image (dir ^ "/newtiles/" ^ h ^ ".png")
let bamboo = dir^"/"^"bamboo1.png"

let hand_to_widgets (h : string list) : W.t list = List.map hand_to_widget h
let render tl = let string_tiles = (string_list_of_tile tl) in let style = Style.(of_shadow (mk_shadow ~width:800 ~radius:640 ())) in
let create_menu_button x y = let l = W.label "click" in 
let r = L.tower ~name:"tile button" ~margins:0 [L.resident ~w:30 ~h:20 l] in L.setx r x; L.sety r y; r in
(**let image = W.image ~w:880 ~h:800 image_file in*)
let menu = let open Menu in [{label = Layout (create_menu_button 200 200); content = Action (fun _ -> print_endline "START")}] in
let box = W.box ~w:720 ~h:600 ~style () in 
let n = Array.init 13 (fun _ -> W.check_box ()) in
let ns = L.flat_of_w (Array.to_list n) in
(** let s = Array.init 13 (fun _ -> W.button ~kind:Button.Switch ~label_on:(Label.icon bamboo) ~label_off:(Label.icon bamboo) "竹" ) in *)
(* let s = Array.init 13 (fun _ -> W.image bamboo) in
let ss = L.flat_of_w (Array.to_list s) in *)
let ss = L.flat_of_w (hand_to_widgets string_tiles) in
let e = Array.init 13 (fun _ -> W.check_box ()) in
let es = L.tower_of_w (Array.to_list e) in
let w = Array.init 13 (fun _ -> W.check_box ()) in
let ws = L.tower_of_w (Array.to_list w) in
let layout = L.tower( [L.flat [ws;(L.tower [ns;L.tower_of_w [box]] );es];ss]) in 
let _ = Menu.create (Menu.Custom menu) in
(* let layout = L.superpose [image;layout] in *)
L.animate_x layout (Avar.fromto 0 0);
L.animate_y layout (Avar.fromto 0 0);
L.setx ns 240;
L.setx ss 190;
L.sety ss 700;
L.sety ws 240;
L.sety es 240;
L.fade_in ~duration:200 layout;
let board = Bogue.of_layout layout in
Bogue.run board

