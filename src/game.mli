(** Representation of the Mahjong game (tiles and hands)

    This module represents:

    - the tiles remaining in a Mahjong game
    - draws from the wall and dead wall
    - the hands of four players
    - points of the four players
    - round (and main wind)
    - riichi

    Actions (chii, pon, kan) and point calculation will be in separate modules. *)

type direction =
  | North
  | South
  | East
  | West  (** Direction of a wind tile. Can be North, South, East, or West *)

type color =
  | Red
  | Green
  | White  (** Color of a dragon tile. Can be Red, Green, or White *)

type value =
  | Integer of int
  | Direction of direction
  | Color of color
      (** Value of a tile. Can be and Integer, Direction, or Color *)

type suit =
  | Pin
  | Man
  | Sou
  | Wind
  | Dragon
      (** All suits and honors in Mahjong. Can be Pin, Man, Sou, Wind, or
          Dragon. *)

type tile = suit * value * bool
(** The tiles in Mahjong. Represented by a value, suit, and boolean. *)

type meld =
  | Chi of (tile * tile * tile)
  | Pon of (tile * tile * tile)
  | Kan of (tile * tile * tile * tile)
      (** Stores open triples or quadruples. Can be either a chi, pon, or kan
          sequence *)

type hand = {
  draw : tile option;
  tiles : tile list;
  melds : meld list;
}

type block =
  | Triple of tile
  | Sequence of tile * tile * tile
  | Ryanmen of tile * tile
  | Kanchan of tile * tile
  | Penchan of tile * tile
  | Single of tile
  | Pair of tile
  | Invalid
      (** Represents any given block of tiles. Can be a Triple, Sequence,
          Ryanmen, Kanchan, Penchan, Single, or Pair. *)

(** Hand of an individual player. Contains the most recently drawn tile and the
    other tiles in a user's hand, separated into tiles and melds. *)

type wall
(** Wall from which tiles are drawn. Contains a list of tiles and the current
    position of the board. *)

type player
(** Contains information about a player's hand, points, riichi, position (North,
    East, South, or West), and discards *)

type state
(** The state of the Mahjong game. Contains information on the wall, dead wall,
    dora, hidden dora, players, round and wind *)

exception EmptyHand
(** Raised when a players hand is empty *)

exception InvalidSuit of string
(** Raised when an invalid suit is given when initializing the game *)

exception OutOfTiles
(** Raised when there are no more tiles in the wall to draw*)

val setup_game : int -> unit -> state
(** [setup_game ()] deals four hands and sets up the state of the board for the
    game to begin. *)

val draw_tile : state -> direction -> bool -> state
(** [draw_tile board wind from_dead] draws a tile from either the wall or dead
    wall (depending on value of from_dead) to the hand of the player with wind
    wind. This occurrs in the game state board. *)

val discard_tile : state -> direction -> state
(** [discard tile board wind] discards a user defined tile from the hand of
    player with wind wind in game state board. *)

val discard_tile_gui : string -> state -> direction -> state
(** [discard tile board wind] discards a user defined tile from the hand of
    player with wind wind in game state board. This differs from
    discard_tile_gui in that it takes a discrete value as input, rather than
    prompting the user to input their response in the console*)

val round_wind : state -> direction
(** [round_wind s] gives a [direction] representing the main round wind. Useful
    for comparisons! *)

val round_number : state -> int
(** [round_number s] gives the number of round of the game. *)

val string_of_wind : direction -> string
(** [string_of_wind d] gives the string representation of a wind value (e.g.,
    wind East evaluates to "East", etc)*)

val get_points : player -> int
(** [get_points p] gives the integer representation of a players points.
    Initializes at 25000 *)

val get_player : state -> direction -> player
(** [get_player s d] gives the player with wind d from game s. *)

val tile_suit : tile -> suit
(** [tile_suit t] gives the suit of tile t. *)

val tile_value : tile -> value
(** [tile_value t] gives the value of tile t. *)

val tile_dora : tile -> int
(** [tile_dora a] gives the dora value of the current tile. *)

val get_wall : state -> wall
(** [get_wall s] gives you a value of type wall*)

val tiles_left : wall -> int
(** [tiles_left w] gives the number of tiles left in the wall. *)

val closed_hand_tiles : hand -> tile list
(** [closed_hand_tiles h] gives the tiles in the closed section of hand h. *)

val open_hand_tiles : hand -> tile list
(** [open_hand_tiles h] gives the tiles in the open section of hand h. *)

val drawn_tile : hand -> tile
(** [drawn_tile h] returns the currently in draw tile of a hand. Raises
    OutofTiles exception if hand has no currently drawn tile. *)

val string_of_list : tile list -> string
(** [string_of_list l] converts the list l to a string representation in the
    form "[l1; l2; l3; ... ln; ]" *)

val string_of_tile : tile -> string
(** [string_of_tile t] converts tile t into a string representation of itself*)

val string_list_of_tile : tile list -> string list
(** [string_list_of_tile l] converts list l into a list representation of each
    tile*)

val hand_of_player : player -> hand
(** [hand_of_player p] gets the current hand of player p*)

val dragon_triplet : block -> bool
(** [dragon_triplet b] returns true if block is a dragon triplet, else false*)

val wind_triplet : block -> bool
(** [wind_triplet b] returns true if block is a wind triplet, else false*)

val prevalent_wind_triplet : block -> direction -> bool
(** [prevalent_wind_triplet b d] returns true if block is a triplet of the
    current board wind, else fales*)

val count_block_terminals : block -> int
(** [count_block_terminals b] returns the amount of terminal tiles in the block
    b*)

val count_block_honors : block -> int
(** [count_block_honors b] returns the amount of honor tiles in the block b*)

val combine : block -> tile -> block
(** [combine b t] attempts to combine block b with tile t to create new block
    b'. Returns invalid if block is an invalid combination *)

val is_sequence : block -> bool
(** [is_sequence] b checks to see if block b is a sequence *)

val is_triple : block -> bool
(** [is_sequence] b checks to see if block b is a sequence *)

val is_pair : block -> bool
(** [is_sequence] b checks to see if block b is a sequence *)

val hand_to_string : hand -> string
(** [hand_to_string h] is the string represented of a hand, represented as
    "[t1, t2, t3]"*)

val create_single : tile -> block
(** [create_single t] creates a block containing a single tile. Useful for
    testing*)

val invalid_block : block
(* [invalid_block] is an object of invalid block type. Useful for checking if a
   single is valid *)

val generate_melds : tile -> tile list -> block list
(** [generate_melds t lst] generates all the possible block lists made possible
    by melding a tile given a tile lists*)

val melding : state -> direction -> state
(** [melding s d] checks the most recent discard of player of game state s and
    wind d to see if any other player can meld that tile. *)
