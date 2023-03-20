(** Representation of the Mahjong game (tiles and hands)

    This module represents:

    - the tiles remaining in a Mahjong game
    - draws from the wall and dead wall
    - the hands of four players
    - points of the four players
    - round (and main wind)
    - riichi

    Actions (chii, pon, kan) and point calculation will be in separate modules. *)

type direction
(** Direction of a wind tile. Can be North, South, East, or West *)

type color
(** Color of a dragon tile. Can be Red, Green, or White *)

type value
(** Value of a tile. Can be and Integer, Direction, or Color *)

type suit
(** All suits and honors in Mahjong. Can be Pin, Man, Sou, Wind, or Dragon. *)

type tile
(** The tiles in Mahjong. Represented by a value, suit, and boolean. *)

type meld
(** Stores open triples or quadruples. Can be either a chi, pon, or kan sequence *)

type hand
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

type block
(** Represents any given block of tiles. Can be a Triple, Sequence, Ryanmen,
    Kanchan, Penchan, Single, or Pair. *)

exception EmptyHand
(** Raised when a players hand is empty *)

exception InvalidSuit of string
(** Raised when an invalid suit is given when initializing the game *)

exception OutOfTiles
(** Raised when there are no more tiles in the wall to draw*)

val setup_game : state
(** [setup_game t] deals four hands using tiles t and sets up the state of the
    board for the game to begin. *)

val draw_tile : state -> direction -> bool -> state
(** [draw_tile board wind from_dead] draws a tile from either the wall or dead
    wall (depending on value of from_dead) to the hand of the player with wind
    wind. This occurrs in the game state board. *)

val discard_tile : state -> direction -> state
(** [discard tile board wind] discards a user defined tile from the hand of
    player with wind wind in game state board. *)

val round_wind : state -> direction
(** [round_wind s] gives a [direction] representing the main round wind. Useful
    for comparisons! *)

val round_number : state -> int
(** [round_number s] gives the number of round of the game. *)

val get_player : state -> direction -> player
(** [get_player s d] gives the player with wind d from game s. *)

val tile_suit : tile -> suit
(** [tile_suit t] gives the suit of tile t. *)

val tile_value : tile -> value
(** [tile_value t] gives the value of tile t. *)

val tile_dora : tile -> int
(** [tile_dora a] gives the dora value of the current tile. *)

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

val combine : block -> tile -> block
val create_single : tile -> block
val invalid_block : block
