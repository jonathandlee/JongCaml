open Bogue
open Main
open Tsdl
open Game
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

let create_button (x : int) =
  let l = W.label "Drop" in

  let r = L.tower ~name:"tile button" ~margins:0 [ L.resident ~w:30 ~h:20 l ] in
  L.setx r x;
  L.sety r 700;
  r

let hand_to_widgets (h : string list) : W.t list = List.map hand_to_widget h
let get_wind game = round_wind game
let p1 game = get_player game Game.East
let p2 game = get_player game Game.South
let p3 game = get_player game Game.West
let p4 game = get_player game Game.North

let generate_tile_gui (tiles : string list) =
  L.flat_of_w (hand_to_widgets tiles)

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
  | East -> West
  | West -> East
  | _ -> failwith "dang"

let rec render (game : state) (first_round : bool) (current_player : direction)
    =
  let wind = round_wind game in
  let game = draw_tile game wind false in
  let p1_hand = player_hand (p1 game) in
  let p3_hand = player_hand (p3 game) in

  let p = get_player game current_player in
  let hand = hand_of_player p in
  let tl = p1_hand in
  let tl = try drawn_tile hand :: tl with EmptyHand -> tl in
  let string_tiles = string_list_of_tile tl in
  let style = Style.(of_shadow (mk_shadow ~width:800 ~radius:640 ())) in

  let image = set_background image_file in
  let box = W.box ~w:720 ~h:600 ~style () in
  (* let n = Array.init 13 (fun _ -> W.check_box ()) in *)
  let ns = generate_tile_gui (string_list_of_tile p3_hand) in
  (* let ns = L.flat_of_w (Array.to_list n) in *)
  (* let s = Array.init 13 (fun _ -> W.button ~kind:Button.Switch
     ~label_on:(Label.icon bamboo) ~label_off:(Label.icon bamboo) "竹" ) in *)
  (* let s = Array.init 13 (fun _ -> W.image bamboo) in let ss = L.flat_of_w
     (Array.to_list s) in *)
  let ss = generate_tile_gui string_tiles in
  let e = Array.init 13 (fun _ -> W.check_box ()) in
  let es = L.tower_of_w (Array.to_list e) in
  let w = Array.init 13 (fun _ -> W.check_box ()) in
  let ws = L.tower_of_w (Array.to_list w) in
  let layout =
    L.tower [ L.flat [ ws; L.tower [ ns; L.tower_of_w [ box ] ]; es ]; ss ]
  in
  let layout = L.superpose [ image; layout ] in
  (* Create Menu*)
  let create_menu_buttonn x =
    let open Menu in
    {
      label = Layout (create_button (fst x));
      content =
        Action
          (fun _ ->
            print_endline (string_of_list tl);
            render
              (discard_tile_gui (List.nth string_tiles (snd x)) game wind)
              false current_player;
            T.push_quit ());
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
  let menu = create_menu_buttons in
  let _ = Menu.create ~dst:layout (Menu.Custom menu) in
  L.animate_x layout (Avar.fromto 0 0);
  L.animate_y layout (Avar.fromto 0 0);
  L.setx ns 165;
  L.setx ss 190;
  L.sety ss 700;
  L.sety ws 240;
  L.sety es 240;
  L.fade_in ~duration:200 layout;
  let board = Bogue.of_layout layout in
  Bogue.run board

let create_board_2 (game : state) (first_round : bool)
    (current_player : direction) =
  let game = draw_tile game current_player false in
  (* let p1_hand = player_hand (p1 game) in *)
  let p3_hand = player_hand (p3 game) in

  let p = get_player game current_player in
  let hand = hand_of_player p in
  let tl = closed_hand_tiles (hand_of_player p) in
  let tl = try drawn_tile hand :: tl with EmptyHand -> failwith "fuck" in
  let string_tiles = string_list_of_tile tl in
  let style = Style.(of_shadow (mk_shadow ~width:800 ~radius:640 ())) in

  let newnewstate = ref game in
  let image = set_background image_file in
  let box = W.box ~w:720 ~h:600 ~style () in
  (* let n = Array.init 13 (fun _ -> W.check_box ()) in *)
  let ns = generate_tile_gui (string_list_of_tile p3_hand) in
  (* let ns = L.flat_of_w (Array.to_list n) in *)
  (* let s = Array.init 13 (fun _ -> W.button ~kind:Button.Switch
     ~label_on:(Label.icon bamboo) ~label_off:(Label.icon bamboo) "竹" ) in *)
  (* let s = Array.init 13 (fun _ -> W.image bamboo) in let ss = L.flat_of_w
     (Array.to_list s) in *)
  let ss = generate_tile_gui string_tiles in
  let e = Array.init 13 (fun _ -> W.check_box ()) in
  let es = L.tower_of_w (Array.to_list e) in
  let w = Array.init 13 (fun _ -> W.check_box ()) in
  let ws = L.tower_of_w (Array.to_list w) in
  let layout =
    L.tower [ L.flat [ ws; L.tower [ ns; L.tower_of_w [ box ] ]; es ]; ss ]
  in
  let layout = L.superpose [ image; layout ] in
  (* Create Menu*)
  let create_menu_buttonn x =
    let open Menu in
    {
      label = Layout (create_button (fst x));
      content =
        Action
          (fun _ ->
            newnewstate :=
              discard_tile_gui
                (List.nth string_tiles (snd x))
                game current_player;
            T.push_quit ());
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
  let menu = create_menu_buttons in
  let _ = Menu.create ~dst:layout (Menu.Custom menu) in
  L.animate_x layout (Avar.fromto 0 0);
  L.animate_y layout (Avar.fromto 0 0);
  L.setx ns 165;
  L.setx ss 190;
  L.sety ss 700;
  L.sety ws 240;
  L.sety es 240;
  L.fade_in ~duration:200 layout;
  let board = Bogue.of_layout layout in
  Bogue.run board;
  let finalstate = !newnewstate in
  finalstate
