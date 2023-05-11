open Game
(** Functions relating to hand and yaku calculation

    Current Yaku Supported:

    - Menzen Tsumo - 1 han *)

type prep_hand
(** All possible subsets of a given hand consisting only of protoruns or single
    waits. Will elaborate more on this after implementation*)

val complete : hand -> block list option
(** Check if a hand is complete (4 runs/triples and a pair). *)
(* val prep_hand : hand -> prep_hand [TODO: COMMENTED OUT SO DUNE BUILD RUNS] *)
(** Generate all possible proto-hands (hands consisting only of protoruns and
   pairs). For now only stores completed hands!*)

val count_honors : block list -> int
(** [count_honors b] counts the number of honor tiles in a block list*)

val count_terminals : block list -> int
(** [count_terminals b] counts the number of terminal tiles in a block list*)

val check_tanyao : block list -> bool
(** [check_tanyao b] checks if a winning hand has tanyao*)

val check_triplet_dragons : block list -> int
(** [check_triplet_dragons] returns number of triplet dragons in hand*)
