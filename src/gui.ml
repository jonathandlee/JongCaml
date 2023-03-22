open Bogue
open Main
open Tsdl
open Game
module W = Widget
module L = Layout
module T = Trigger




let string_of_tile (x : tile) = tile_suit x
let dir = Sys.getcwd ()
let image_file = dir^"/src/newtiles/"^"tony.jpg"
let hand_to_widget (h : string) = W.image (dir ^ "/src/newtiles/" ^ h ^ ".png")
let bamboo = dir^"/"^"bamboo1.png"
let create_button (x:int) = let l = W.label "Drop" in 

let r = L.tower ~name:"tile button" ~margins:0 [L.resident ~w:30 ~h:20 l] in L.setx r x; L.sety r 700; r

let wtf () = print_endline "whatthefuck"




let hand_to_widgets (h : string list) : W.t list = List.map hand_to_widget h




let rec render (game:state) = 
  
  let wind = round_wind game in
  let game = draw_tile game wind false in
let p = get_player game wind in
let hand = hand_of_player p in
let tl = closed_hand_tiles hand in
  let tl = (try (drawn_tile hand)::tl with EmptyHand -> tl ) in
  let string_tiles = (string_list_of_tile tl) in let style = Style.(of_shadow (mk_shadow ~width:800 ~radius:640 ())) in



let image = W.image ~w:880 ~h:800 image_file in
let image = L.flat [L.resident image] in
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
let layout = L.superpose [image;layout] in 
(** Create Menu*)
let create_menu_buttonn (x:int) = let open Menu in {label = Layout (create_button x); content = Action (fun _ ->  print_endline (string_of_list tl);
 render (discard_tile game wind); 
 T.push_quit ())} in
let create_menu_buttons = List.map create_menu_buttonn [200;240;280;320;360;400;440;480;520;560;600;640;680;720] in
let menu = create_menu_buttons in
let _ = Menu.create ~dst:layout (Menu.Custom menu)  in
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

