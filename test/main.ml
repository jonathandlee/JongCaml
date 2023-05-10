open OUnit2
open Mahjong
open Game

(* Create some basic inputs for testing*)
let init_game = setup_game ()

(* [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
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

let method_tests =
  [
    test_draw_wind "basic" init_game East 14;
    test_draw_wind_raises "Test East" init_game East [];
    test_draw_wind_raises "Test West" init_game West [];
    test_draw_wind_raises "Test North" init_game North [];
    test_draw_wind_raises "Test South" init_game South [];
  ]

let suite = "test suite for Mahjong" >::: List.flatten [ method_tests ]
let _ = run_test_tt_main suite
