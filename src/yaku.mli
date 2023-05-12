open Game
(** Functions relating to hand and yaku calculation

    Current Yaku Supported:

    - Menzen Tsumo - 1 han *)

type prep_hand
(** All possible subsets of a given hand consisting only of protoruns or single
    waits. Will elaborate more on this after implementation*)

val complete : hand -> block list option
(** Check if a hand is complete (4 runs/triples and a pair). *)

val complete_closed : hand -> block list option
(** Checks if a hand is complete USING ONLY CLOSED TILES (and drawn tile). *)

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

val check_tsumo : hand -> bool
(** [check_tsumo hand] checks if the hand has no melds. Requires: "hand" is a
    winning hand (see [complete hand])*)

val check_iipeikou : hand -> int
(** [check_iipeikou hand] checks how many pure double sequences are in the hand.
    (Usually return 1 or 0, if the result is 2, the alternative "ryanpeikou"
    yaku will be calculated instead) *)

val check_triple_triplets : block list -> bool
(** [check_triple_triplets b] checks if triple triplets (sanshoku doukou) is
    present in the block list*)

val check_all_triplets : block list -> bool
(** [check_all_triplets b] checks if the hand is all triplets (and one pair).
    Also known as "toitoi"*)

val check_all_concealed_triplets : hand -> bool
(** [check_all_concealed_triplets h] checks if the hand satsifies the sanankou
    (three concealed triplets) yaku. Requires: hand is complete*)

val check_shousangen : block list -> bool
(** [check_shousangen b] checks if the hand has shousangen (little three dragon)
    yaku.*)

val check_chiitoitsu : block list -> bool
(** [check_chiitoitsu b] checks if the hand has chiitoitsu (seven pairs) yaku.
    Menzenchin only*)

val check_chanta : block list -> bool
(** [check_chanta b] checks if the hand has chanta (half outside hand) yaku. -1
    after making a call*)

val check_ittsu : block list -> bool
(** [check_ittsu b] checks if the hand has ittsu (pure straight) yaku. -1 after
    making a call*)

val check_sanshiki : block list -> bool
(** [check_sanshiki b] checks if the hand has mixed triple sequence (sanshoku
    doujun) yaku. -1 after making a call*)
