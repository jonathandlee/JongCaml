open Game
(** Functions relating to hand and yaku calculation

    Current Yaku Supported:

    - Menzen Tsumo - 1 han *)

type prep_hand
(** All possible subsets of a given hand consisting only of protoruns or single
    waits. Will elaborate more on this after implementation*)

val complete : hand -> block list option
(** Check if a hand is complete (4 runs/triples and a pair). *)
val prep_hand : hand -> prep_hand
(** Generate all possible proto-hands (hands consisting only of protoruns and pairs). For now only stores completed hands!*)
