module type GAME =
sig
  (* TYPES *)

  (* names the players of the game *)
  type player = 
    | P1 
    | P2

  (* expresses the states of the game,
   * i.e., what the board looks like and whose turn it is *)
  type state

  (* describes the game's moves *)
  type move

  (* INITIAL VALUES *)

  (* defines the state at the start of the game *)
  val initial_state : state

  (* TYPE CONVERSIONS *)

  (* returns the name of a player *)
  val string_of_player : player -> string

  (* describes a state in words *)
  val string_of_state : state -> string

  (* describes a move in words *)
  val string_of_move : move -> string

  (* describes the game status (is it over or not),
   * and if it is over, the final outcome *)
  val status_string: state -> string

  (* Tool for a human player. 
   * produces the move that a string represents *)
  val move_of_string : string -> move

  (* GAME LOGIC *)

  (* determines whether a given move is legal in a given state *)
  val is_legal_move : state -> move -> bool

  (* produces the list of allowable moves in a given state *)
  val legal_moves : state -> move list

  (* produces the current player in a given state *)
  val current_player : state -> player

  (* executes the given move and produces the new game state *)
  val next_state : state -> move -> state

  (* estimates value of a given state
   * Remember: positive values are better for P1
   *       and negative values are better for P2 *)
  val estimate_value : state -> float

  (* determines whether the game is over or not given a state *)
  val is_game_over : state -> bool
end ;;
