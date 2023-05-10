open Game
(** Functions relating to rendering the game GUI*)
val render : state -> bool -> direction -> unit

val create_board_2 : state -> bool -> direction -> state
(** Create_board_2 [state s]  [bool b] [direction w] creates a GUI 
representationof game [g], with current player [direction]. [b] dictates 
whether or not this is the first round. This function acts similar to 
a promise; state is only returned upon player choosing which tile to discard*)
