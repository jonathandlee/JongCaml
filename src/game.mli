(** Representation of the Mahjong game (tiles and hands)

    This module represents:

    - the tiles remaining in a Mahjong game
    - draws from the wall and dead wall
    - the hands of four players
    - points of the four players
    - round (and main wind)
    - riichi

    Actions (chii, pon, kan) and point calculation will be in separate modules. *)

type state
(** The state of the Mahjong game. Contains information on the wall, players,
    round and wind *)

type hand
(** Hand of an individual player. Separates tiles and melds*)

type tile
(** The tiles in Mahjong*)

type meld
(** Either a chi, pon, or kan sequence*)

type suit
(** All suits and honors in Mahjong*)

type wall
(** Wall from which tiles are drawn*)

type player
(** Contains information about a player's hand, points, riichi, position (North,
    East, South, or West), and discards *)

exception InvalidSuit of string
(** Raised when an invalid suit is given when initializing the game *)

exception OutOfTiles
(** Raised when there are no more tiles in the wall to draw*)

val game_init : int -> suit -> state
(** [game_init n] creates a state representing the game in round [n] and [suit]
    wind. Requires: suit is a wind*)

val round_wind : state -> suit
(** [round_wind s] gives a [suit] representing the main round wind. Useful for
    comparisons!*)

val round_number : state -> int
(** [round_number s] gives the number of round of the game.*)

val get_player : state -> int -> player
(** [get_player s n] gives the nth player of the game (East being the first
    player and going in a ccw direction)*)

val tile_value : tile -> suit
(** [tile_value a] gives the suit. For instance, a tile representing the "One
    Pin" will return the suit [Pin of 1]. *)

val tile_dora : tile -> int
(** [tile_dora a] gives the dora value of the current tile. For instance, a red
    five would have a dora value of one. *)

val wall_draw : wall -> tile
(** [wall_draw w] draws first tile in the wall. Requires: the wall is not out of
    tiles. Raises: OutOfTiles exception. *)

val wall_pop : wall -> wall
(** [wall_pop w] returns the wall after a tile has been drawn. *)

val deadwall_draw : wall -> tile
(** [deadwall_draw w] draws first tile from the dead wall.*)

val tiles_left : wall -> int
(** [tiles_left w] gives the number of tiles left in the wall.*)

val hand_draw : wall -> hand * wall
(** [hand_draw w] creates a hand by drawing 13 tiles from the wall and returns
    both the hand and the remaining wall.*)

val setup_game : wall -> hand list * wall
(** [setup_game wall] deals four hands and sets up the state of the board for
    the game to begin *)

val closed_hand_tiles : hand -> tile list

val open_hand_tiles : hand -> tile list
