open OUnit2
open Mahjong
open Game

(* Create some basic inputs for testing*)
let init_game = setup_game ()

(* [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

let rec progress_game (n : int) (game : state) (wind : direction) : state =
  let output_game =
    let post_draw_state = draw_tile game wind false in
    let new_tile =
      drawn_tile (hand_of_player (get_player post_draw_state wind))
    in
    discard_tile_gui (string_of_tile new_tile) post_draw_state wind
  in
  match n - 1 with
  | 0 | -1 -> output_game
  | _ -> progress_game (n - 1) output_game wind
(* progress_game allows for easy progression of game states, in order to help
   with testing wall, dead wall, etc.contents For my own sanity: works by taking
   in a state, drawing a tile, and then discarding said tile*)

let get_all_tiles (h : hand) =
  let g =
    let hands = closed_hand_tiles h @ open_hand_tiles h in
    try drawn_tile h :: hands with EmptyHand -> hands
  in
  g |> string_list_of_tile |> List.sort String.compare

let rec check_elts_in_list (inp_elts : 'a list) (check_elts : 'a list) =
  match inp_elts with
  | [] -> true
  | hd :: tl ->
      if List.mem hd check_elts then check_elts_in_list tl check_elts else false

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)

let pp_string (s : string) = s

let pp_list lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_string h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_string h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(* You should not be testing any helper functions here. Test only the functions
   exposed in the [.mli] files. Do not expose your helper functions. See the
   handout for an explanation. *)

(* let generate_all_tiles_test name = name >:: fun _ -> assert_equal
   ~printer:pp_string (from_json file |> f) result *)

let test_draw_wind (name : string) (game : state) (wind : direction)
    (expected : int) : test =
  name >:: fun _ ->
  let phand =
    hand_of_player (get_player (draw_tile init_game East false) East)
  in
  assert_equal ~printer:string_of_int
    (List.length
       (string_list_of_tile (drawn_tile phand :: closed_hand_tiles phand)))
    expected

let test_draw_wind_raises (name : string) (game : state) (wind : direction)
    (expected : string list) : test =
  name >:: fun _ ->
  assert_equal
    (drawn_tile
       (hand_of_player (get_player (draw_tile init_game wind false) wind)))
    (drawn_tile
       (hand_of_player (get_player (draw_tile init_game wind false) wind)))

let test_draw_consistent (name : string) (game : state) (wind : direction)
    (expected : bool) : test =
  name >:: fun _ ->
  let original_hand =
    get_player game wind |> hand_of_player |> closed_hand_tiles
    |> string_list_of_tile
  in
  assert_equal
    (check_elts_in_list original_hand
       (get_player (draw_tile game wind false) wind
       |> hand_of_player |> closed_hand_tiles |> string_list_of_tile))
    expected

let test_draw_discard (name : string) (game : state) (wind : direction)
    (expected : bool) : test =
  name >:: fun _ ->
  let drawn_state = draw_tile game wind false in
  let drawn_tile = drawn_tile (hand_of_player (get_player drawn_state wind)) in
  assert_equal ~printer:pp_list
    (get_player game wind |> hand_of_player |> closed_hand_tiles
   |> string_list_of_tile |> List.sort String.compare)
    (get_all_tiles
       (hand_of_player
          (get_player
             (discard_tile_gui (string_of_tile drawn_tile) drawn_state wind)
             wind)))

let string_of_list_test (name : string) (game : state) (wind : direction)
    (expected : string) : test =
  name >:: fun _ ->
  assert_equal expected ~printer:pp_string
    (string_of_list (closed_hand_tiles (hand_of_player (get_player game wind))))
(* (drawn_tile (hand_of_player (get_player (drawn_state east)))) *)

let round_wind_test (name : string) (game : state) (expected_wind : direction) :
    test =
  name >:: fun _ -> assert_equal (round_wind game) expected_wind

let count_tiles_left (name : string) (game : state) (expected : int) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int expected (tiles_left (get_wall game))

let method_tests =
  [
    test_draw_wind "basic" init_game East 14;
    test_draw_wind_raises "Test East" init_game East [];
    test_draw_wind_raises "Test West" init_game West [];
    (*This might seem needless, but most of the issues we've had so far have
      come from not accounting for specifics with wind directions*)
    test_draw_wind_raises "Test North" init_game North [];
    test_draw_wind_raises "Test South" init_game South [];
    test_draw_consistent "Test Remain After Draw" init_game East true;
    test_draw_consistent "Test Remain After Draw" init_game West true;
    test_draw_consistent "Test Remain After Draw" init_game North true;
    test_draw_consistent "Test Remain After Draw" init_game South true;
    test_draw_discard "Test Draw->Discard associativity E" init_game East true;
    test_draw_discard "Test Draw->Discard associativity W" init_game West true;
    test_draw_discard "Test Draw->Discard associativity N" init_game North true;
    test_draw_discard "Test Draw->Discard associativity S" init_game South true;
    string_of_list_test "Test String of List" init_game East
      "[Pin 4; Pin 9; Man 1; Man 1; Red Man 5; Man 6; Sou 1; Sou 3; Sou 4; \
       Wind East; Wind South; Dragon White; Dragon Green; ]";
    string_of_list_test "Test String of List" init_game West
      "[Pin 3; Pin 4; Pin 5; Pin 8; Pin 9; Man 1; Man 1; Man 2; Man 6; Sou 1; \
       Sou 3; Sou 9; Wind South; ]";
    round_wind_test "Test getting round wind" init_game East;
    round_wind_test "Test getting round wind"
      (progress_game 1 init_game East)
      East;
    round_wind_test "Test getting round wind"
      (progress_game 5 init_game East)
      East;
    count_tiles_left "initial state tiles left" init_game 70;
    count_tiles_left "5 iterations tiles left test"
      (progress_game 5 init_game East)
      65;
    count_tiles_left "5- iterations tiles left test"
      (progress_game 50 init_game East)
      20;
  ]

let suite = "test suite for Mahjong" >::: List.flatten [ method_tests ]
let _ = run_test_tt_main suite
