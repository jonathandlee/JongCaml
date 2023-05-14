open Bogue
open Main
open Tsdl
open Game
open Yaku
module W = Widget
module L = Layout
module T = Trigger

(* String_of_tile x is the string representation of tile x*)
let string_of_tile (x : tile) = tile_suit x
let dir = Sys.getcwd ()

(* Rest in peace tony, we commemorate you in 3110 mahjong. *)
let image_file = dir ^ "/src/newtiles/" ^ "tony.jpg"
let hand_to_widget (h : string) = W.image (dir ^ "/src/newtiles/" ^ h ^ ".png")
let bamboo = dir ^ "/" ^ "bamboo1.png"
let line = Style.mk_line ~color:Draw.(opaque grey) ~width:5 ~style:Solid ()

let create_quit_button () =
  let style = Style.(create ~border:(mk_border line) ()) in
  let l = W.label "quit" in
  let l2 = L.resident ~background:(L.style_bg style) l in
  L.setx l2 160;
  L.sety l2 200;
  let open Menu in
  { label = Layout l2; content = Action (fun _ -> Bogue.quit ()) }

let create_button (x : int) =
  let l = W.label "Drop" in
  let r = L.tower ~name:"tile button" ~margins:0 [ L.resident ~w:30 ~h:20 l ] in
  L.setx r x;
  L.sety r 700;
  r

let hand_to_widgets (h : string list) : W.t list = List.map hand_to_widget h
let get_wind game = round_wind game

let generate_tile_gui (tiles : string list) =
  L.flat_of_w (hand_to_widgets tiles)

let generate_vertical_tile_gui (tiles : string list) =
  L.tower_of_w (hand_to_widgets tiles)

(* player_hand [game] [player] is a list representing [tile1,tile2,...tilen],
   where tiles are drawn from player [player]'s hand*)
let player_hand (player : player) : tile list =
  player |> hand_of_player |> closed_hand_tiles

(* Set_background creates a layout of type t with a specific background image*)

let set_background (image : string) : L.t =
  [ W.image ~w:880 ~h:800 image |> L.resident ] |> L.flat

(* let next_wind (wind:direction) = match wind with | East -> South | South ->
   West | West -> North | North -> East *)

let next_wind (wind : direction) =
  match wind with
  | East -> North
  | North -> West
  | West -> South
  | South -> East

let complete_helper (b : block list option) : bool =
  match b with
  | Some z -> true
  | None -> false

let create_board_2 (game : state) (first_round : bool)
    (current_player : direction) =
  let endgame_bool = ref true in
  let game = draw_tile game current_player false in
  let new_p = player_hand (get_player game (next_wind current_player)) in
  let p = get_player game current_player in
  let hand = hand_of_player p in
  if complete_helper (complete hand) then Bogue.quit ();
  let tl = closed_hand_tiles (hand_of_player p) in
  let tl = try drawn_tile hand :: tl with EmptyHand -> failwith "fuck" in
  let string_tiles = string_list_of_tile tl in
  let style = Style.(of_shadow (mk_shadow ~width:800 ~radius:640 ())) in
  let newnewstate = ref game in
  let image = set_background image_file in
  let box = W.box ~w:720 ~h:600 ~style () in
  let ns = generate_tile_gui (string_list_of_tile new_p) in
  let ss = generate_tile_gui string_tiles in
  let layout =
    L.tower [ L.flat [ L.tower [ ns; L.tower_of_w [ box ] ] ]; ss ]
  in
  let layout = L.superpose [ image; layout ] in
  let create_menu_buttonn x =
    let open Menu in
    {
      label = Layout (create_button (fst x));
      content =
        Action
          (fun _ ->
            T.push_quit ();
            endgame_bool := false;
            newnewstate :=
              discard_tile_gui
                (List.nth string_tiles (snd x))
                game current_player);
    }
  in
  let create_menu_buttons =
    List.map create_menu_buttonn
      [
        (200, 0);
        (240, 1);
        (280, 2);
        (320, 3);
        (360, 4);
        (400, 5);
        (440, 6);
        (480, 7);
        (520, 8);
        (560, 9);
        (600, 10);
        (640, 11);
        (680, 12);
        (720, 13);
      ]
  in
  let menu = create_quit_button () :: create_menu_buttons in
  let _ = Menu.create ~dst:layout (Menu.Custom menu) in
  L.animate_x layout (Avar.fromto 0 0);
  L.animate_y layout (Avar.fromto 0 0);
  L.setx ns 165;
  L.setx ss 190;
  L.sety ss 700;
  L.fade_in ~duration:200 layout;
  let board = Bogue.of_layout layout in
  let finalstate = !newnewstate in
  Bogue.run board;
  T.push_quit ();
  (finalstate, !endgame_bool)
