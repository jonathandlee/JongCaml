open Bogue
open Main
open Tsdl
open Game
open Yaku
module W = Widget
module L = Layout
module T = Trigger

let string_of_tile (x : tile) = tile_suit x
let dir = Sys.getcwd ()
let image_file = dir ^ "/src/newtiles/" ^ "greenbg.png"
let hand_to_widget (h : string) = W.image (dir ^ "/src/newtiles/" ^ h ^ ".png")

let create_quit_button () =
  let l = W.label ~size:40 "QUIT" in
  let l2 = L.resident l in
  L.setx l2 40;
  L.sety l2 40;
  let open Menu in
  { label = Layout l2; content = Action (fun _ -> Bogue.quit ()) }

let create_text (t : string) (x : int) (y : int) =
  let ll =
    W.rich_text ~size:40 Text_display.(page [ para ("            " ^ t) ])
  in
  (* let l = W.text_display ~w:50 ~h:50 p in *)
  let l2 = L.tower_of_w [ ll ] in
  L.setx l2 x;
  L.sety l2 y;
  l2

let create_button (x : int) =
  let l = W.label "Drop" in
  let r = L.tower ~name:"tile button" ~margins:0 [ L.resident ~w:30 ~h:20 l ] in
  L.setx r x;
  L.sety r 690;
  r

let hand_to_widgets (h : string list) : W.t list = List.map hand_to_widget h

let generate_tile_gui (tiles : string list) =
  L.flat_of_w (hand_to_widgets tiles)

(* player_hand [game] [player] is a list representing [tile1,tile2,...tilen],
   where tiles are drawn from player [player]'s hand*)
let player_hand (player : player) : tile list =
  player |> hand_of_player |> closed_hand_tiles

(* Set_background creates a layout of type t with a specific background image*)

let set_background (image : string) : L.t =
  [ W.image ~w:900 ~h:960 image |> L.resident ] |> L.flat

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

let render_lose_screen (winner : string) =
  (* let style = Style.(of_shadow (mk_shadow ~width:800 ~radius:640 ())) in *)
  let image = set_background image_file in
  let text = W.text_display "You ran out of tiles, nobody wins!" in
  let layout = L.tower [ L.flat_of_w [ text ] ] in
  let layout = L.superpose [ image; layout ] in
  let board = Bogue.of_layout layout in
  Bogue.run board

let render_win_screen (h : string) =
  (* let style = Style.(of_shadow (mk_shadow ~width:800 ~radius:640 ())) in *)
  let image = set_background image_file in
  let text = W.text_display "Congrats, player" in
  let text2 = W.text_display ("You won with hand" ^ h) in
  let layout = L.tower [ L.tower_of_w [ text; text2 ] ] in
  let layout = L.superpose [ image; layout ] in
  let board = Bogue.of_layout layout in
  Bogue.run board

let create_board_2 (game : state) (first_round : bool)
    (current_player : direction) =
  let endgame_bool = ref true in
  let new_p = player_hand (get_player game (next_wind current_player)) in
  let p = get_player game current_player in
  let hand = hand_of_player p in
  let _ = print_endline (hand_to_string hand) in
  if complete_helper (complete hand) then
    render_win_screen (hand_to_string hand);
  let tl = closed_hand_tiles (hand_of_player p) in
  let tl =
    try drawn_tile hand :: tl with EmptyHand -> failwith "Draw Failure"
  in
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
  let layout =
    L.superpose
      [
        image;
        layout;
        create_text (string_of_wind current_player) 640 40;
        create_text
          (string_of_int (get_points (get_player game current_player)))
          630 100;
      ]
  in

  let create_menu_buttonn x =
    let open Menu in
    {
      label = Layout (create_button (fst x));
      content =
        Action
          (fun _ ->
            T.push_quit ();
            endgame_bool := false;
            print_endline "argh!";
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
  let board = Bogue.of_layout layout in
  Bogue.run board;
  T.push_quit ();
  (!newnewstate, !endgame_bool)
