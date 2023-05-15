open Game
(** Functions relating to rendering the game GUI*)

val create_board_2 : state -> bool -> direction -> state * bool
(** Create_board_2 [state s] [bool b] [direction w] creates a GUI
    representationof game [g], with current player [direction]. [b] dictates
    whether or not this is the first round. This function acts similar to a
    promise; a new state is only returned upon player choosing which tile to
    discard*)

val render_lose_screen : string -> unit
(** [render_lose_screen s] renders a board representing the losing screen with
    string text s displayed*)
