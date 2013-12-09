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

  (* estimates value of a given state *)
  val estimate_value : state -> float

  (* determines whether the game is over or not given a state *)
  val is_game_over : state -> bool
end;;

module Game =
struct

  (* two possible players - P1 moves by placing 1's, P2 moves by placing 2's *)
  type player = 
      | P1
      | P2

  type state = (int list list) * player (* A list of a list of int, each int is a row *)
  type move = int (* an integer that represents the index in which to place an element *)

  (* initial_state: int -> int -> int list list
     Creates a list of list of ints that represents a matrix of 0's with dimension width by height
     NOTE : the list of list representation of the board is in reverse. The first sublist is the 
     top list in the matrix and so on. *)
  let initial_state = 
    let make_initial_state (width : int) (height : int) (player_ : player): state = 
      let rec create_board (width : int) (height : int) : int list list = 
        let rec create_row (width : int)  =
          match width with 
            | 0 -> []
            | _ -> 0 :: (create_row (width - 1)) 
        in let row = (create_row (width)) in
          match height with 
            | 0 -> []
            | _ -> row :: (create_board width (height - 1))
      in ((create_board width height), P1) in
      (make_initial_state 7 6 P1)

  (* string_of_player : player -> string
     I/P : player 
     O/P : Returns either player 1 or player 2 depending on whose turn it is *)
  let string_of_player (player_ : player) : string = 
    match player_ with
      | P1 -> "Player 1"
      | P2 -> "Player 2"

  (* string_of_state: state -> string
     I/P : state of the game 
     O/P : Prints out a matrix representation of the board *)
  let rec string_of_state ((board, player) : state) : string =
    let rev_board = (List.rev board) in 
    let rec row_string (row : int list) = 
      match row with 
        | [] -> "\n"
        | rhd :: rtl -> (string_of_int rhd) ^ " | " ^ (row_string rtl) in 
    let rec print_board (board : int list list) =
      match board with
        | [] -> " "
        | hd :: tl -> (row_string hd) ^ (print_board tl) in 
      (print_board rev_board) ^ " It's " ^ (string_of_player player) ^ "'s turn."

  (* string_of_move : int -> string 
     Returns a string representation of the move taken. *)
  let string_of_move (move_ : move) : string = 
    string_of_int move_

  (* turns a string that contains a valid number into an move type (int) *)
  let move_of_string (str : string) : move = int_of_string str

  (* is_legal_move : state -> move -> bool 
     checks if the entry in the top-most level (last sublist) corresponding to the index given
     by the move is vacant. If it is vacant -> true, if it is not vacant then that column is full -> false*)
  let is_legal_move ((board, player) : state) (move_ : move) : bool = 
    let rev_board = (List.rev board) in 
      match rev_board with 
        | [] -> failwith "Error : invalid board configuration"
        | hd :: tl -> let width = (List.length hd) in
              if move_ > (width - 1) then false 
              else if move_ < 0 then false
              else if (List.nth hd move_) != 0 then false
              else true

  (* legal_moves : state -> int list
     returns all the legal possible move given a state by checking for vacancies in the last sublist of
     of the current state's board list. 
     legal_moves : state -> int list 
     I/P : state of game
     O/P : a list of all the legal moves at the current game state*)
  let legal_moves ((board, player) : state) : int list = 
    let rev_board = (List.rev board) in 
      match rev_board with 
        | [] -> failwith "Error : invalid board configuration"
        | hd :: tl -> 
            let rec get_indices alon counter = 
              match alon with 
                | [] -> []
                | thd :: ttl -> if thd = 0 then counter :: (get_indices ttl (counter + 1))
                    else (get_indices ttl (counter + 1)) in
              (get_indices hd 0)

  (* current_player : state -> player
     simply returns the current player given the current state of the game. *)
  let current_player ((board, player_) : state) : player = player_

  (* next_state : state -> move -> state
     given the current state of the game, next_state returns the next state in the game *)
  (* insert_elmnt is a helper used to position the element in the row in which it belongs 
     make_move is responsible for making the actual move by checking if the move_th element of the bottom 
     row (first list in the board) is 0, if it is, then make_move insert the appropriate element into that 
     row, if it is not, then the next row in the board is tested. NOTE : this whole process is only carried
     out if the move provided by the user is a legal move, if it is not, next state throws an error. *)
  let next_state ((board, player) : state) (move_ : move) : state = 
    let rec into_row (row : int list) (move_ : int) (elmnt : int) (counter : int) =
      match row with 
        | [] -> []
        | rhd :: rtl -> 
            if counter < move_  then rhd :: (into_row rtl move_ elmnt (counter + 1))
            else (elmnt :: rtl) in 
    let rec make_move (board : int list list) : int list list = 
      match board with
        | [] -> failwith "Error : invalid board configuration."
        | hd :: tl -> 
            if List.nth hd move_ != 0 then hd :: (make_move tl)
            else 
              match player with 
                | P1 -> (into_row hd move_ 1 0) :: tl
                | P2 -> (into_row hd move_ 2 0) :: tl in
      match player with 
        | P1 -> ((make_move board), P2)
        | P2 -> ((make_move board), P1)

  (* estimate value will assing a weight to every possible move from a given state 
     estimate_value : state -> float 
     I/P : the state of the game 
     O/P : the float representing the value of the board *)
  let estimate_value ((board, player) : state) : float =
    let height = List.length board and width = List.length (List.hd board) in
    (* get_board_value : int list list -> int 
       I/P : board, the current board
       O/P : board_value, which equals the relative
       value of the current position. More positive numbers 
       indicate a better position for player 1, more negative
       numbers indicate a better position for player 2.*)
    let get_board_value (board : int list list) : float  =

      (* check_sublist: int list -> int -> int 
         I/P : aloi, a list of integers, chip, an integer
         O/P : a number indicating the value of the list. Any
         list containing a combination of just the chip and zeroes
         is equal to the number of chips in the list. If the sequence
         is interrupted by chips of different types, then the list has
         a value of 0.*)
      let check_sublist (aloi : int list) (chip : int) : float = 
        let rec _check_sublist (aloi : int list) (chip : int) (counter : float) : float =
          match aloi with 
            | [] -> counter
            | hd :: tl -> 
                if hd = chip && counter = 3.0 then (_check_sublist tl chip (counter +. 100000.0)) else  
                if hd = chip then (_check_sublist tl chip (counter +. 1.0))
                else (_check_sublist tl chip counter) in 
          _check_sublist aloi chip 0.0 in

      (* check_rows : int list list -> int -> int
         I/P : board, the current board state and the chip
         O/P : the result of applying check_sublist to every 
         row subsequence of length 4 in the board state. The row
         is not checked if it contains only 0's. *)
      let rec check_rows (board : int list list) (chip : int) : float = 
        match board with 
          | hd :: tl -> 
              let height = List.length board in
              let rec _check_rows (board : int list list) (chip : int) : float = 
                match board with
                  | [] -> 0.0
                  | hd :: tl -> 
                      if (not (List.mem chip hd)) then (check_rows tl chip) else
                        let rec check_row (row : int list) (chip : int) : float= 
                          if width = 4 then 
                            match row with 
                              | [rhd ; rhd2 ; rhd3 ; rhd4] -> (check_sublist [rhd ; rhd2 ; rhd3 ; rhd4] chip)
                              | _ -> failwith "error : invalid board" else
                            match row with 
                              | rhd :: rhd2 :: rhd3 :: rhd4 :: rhd5 :: rtl -> 
                                  if [rhd ; rhd2 ; rhd3 ; rhd4 ; rhd5] = [0 ; chip ; chip ; chip ; 0] &&
                                     List.length tl = height - 1 then 
                                    (check_sublist [rhd ; rhd2 ; rhd3 ; rhd4] chip) +. 10000.0 +. 
                                      (check_row (rhd2 :: rhd3 :: rhd4 :: rtl) chip) else 
                                    (check_sublist [rhd ; rhd2 ; rhd3 ; rhd4] chip) +. 
                                      (check_row (rhd2 :: rhd3 :: rhd4 :: rtl) chip)
                              | [rhd ; rhd2 ; rhd3 ; rhd4] -> (check_sublist [rhd ; rhd2 ; rhd3 ; rhd4] chip)
                              | _ -> 0.0 in
                          (check_row hd chip) +. (_check_rows tl chip) in 
                _check_rows (board) chip
          | _ -> 0.0 in

      (* check_columns : int list list -> int -> int
         I/P : board, the current board state, chip, the int to be checked
         O/P : the result of applying check_sublist to every column in the 
         current board state and taking the sum. *)
      let rec check_columns (board: int list list) (chip : int) : float = 
        let rec get_column (board : int list list) : int list = 
          match board with 
            | [] -> []
            | bhd :: btl -> 
                match bhd with 
                  | [] -> []
                  | rhd :: rtl -> rhd :: (get_column btl) in
          match board with 
            | [] -> failwith "Doesn't require this type of checking"
            | hd :: tl -> 
                match hd with 
                  | [] -> 0.0
                  | _ -> 
                      let column = (get_column (board)) and restlist = (List.map (fun x -> List.tl x) board) in 
                        match column with 
                          | rhd :: rhd1 :: rhd2 :: rhd3 :: rtl ->
                              (check_sublist column chip) +. (check_columns restlist chip) 
                          | _ -> 0.0 in

      (* check_diagonals : int list list -> int -> int
         I/P: board, chip, the current board state and the chip
         O/P: the result of applying check_sublist to every diagonal 
         sublist of length four in the current board state. Correction
         for non square board states is accomplished by correct_non_square. *)
      let rec check_diagonals (__board__ : int list list) (chip : int) : float = 
        let rec check_square_matrix (board : int list list) (chip: int) : float = 
          (* gets the bottom right subsquare matrix *)
          let get_subsquare_matrix (board : int list list) =
            (List.map (fun x -> List.tl x) (List.tl board)) in

          (* gets the top right to bottom left diagonal of a square matrix *)
          let rec get_square_diagonal board counter= 
            match board with 
              | [] -> []
              | hd :: tl -> 
                  (List.nth hd counter) :: (get_square_diagonal tl (counter + 1)) in

          (* gets the values given by lists made from diagonals of a square matrix 
             check_base_square_matrix : int list list -> int -> float 
             I/P : the board, the chip
             O/P : the result of applying check_sublist to the diagonals of a square matrix *)
          let rec check_base_square_matrix (board : int list list) (chip : int) : float = 
            let rev_board = List.rev board in
            let diagonal1 = (get_square_diagonal board 0) and 
              diagonal2 = (get_square_diagonal rev_board 0) in
            let sum1 = (check_sublist diagonal1 chip) and sum2 = (check_sublist diagonal2 chip) in
              (sum1 +. sum2) in
          let height = (List.length board) in 
            if height = 4 then (check_base_square_matrix board chip)
            else 
              let rev_board = (List.rev board) in 
              let 
                subsquare1 = (get_subsquare_matrix board) and
                subsquare2 = (get_subsquare_matrix (List.rev board)) and
                subsquare3 = (get_subsquare_matrix (List.map (fun x -> List.rev x) board)) and
                subsquare4 = (get_subsquare_matrix (List.map (fun x -> List.rev x) rev_board)) in
                (check_square_matrix subsquare1 chip) +. (check_square_matrix subsquare2 chip) +. 
                  (check_square_matrix subsquare3 chip) +. (check_square_matrix subsquare4 chip) in

        (* returns either the left most or top most subsquare matrix depending on the 
           configuration of the given board state.
           correct_non_square : int list list -> int list list 
           I/P : the board corresponding to the current state of the game
           O/P : either the left-most of top-most subsquare matrix of the current
           board state.*)
        let rec correct_non_square (board : int list list) : int list list =
          match board with 
            | [] -> failwith "Error : invalid board" 
            | hd :: tl -> 
                let width = List.length (List.hd board)  in 
                (* is called when the height > width to get the topmost square submatrix *)
                let rec get_by_vertical (board : int list list) (hcounter : int) =
                  if hcounter < width then (List.hd board) :: (get_by_vertical (List.tl board) (hcounter + 1)) else [] in
                (* used in the body of get_by_horizon to correct each row *)
                let rec correct_row (row : int list) (rcounter : int) = 
                  if rcounter < height then 
                    (List.hd row) :: (correct_row (List.tl row) (rcounter + 1))
                  else [] in
                (* maps the correct_row function to every row in the matrix if 
                   width > height *)
                let get_by_horizon (board : int list list) (rcounter : int) = 
                  (List.map (fun x -> (correct_row x rcounter)) board) in
                  if height > width then (get_by_vertical (board) 0) else (get_by_horizon (board) 0) in
        let width = (List.length (List.hd __board__))  and  height = (List.length __board__) in
          if width > height then 
            (check_square_matrix (correct_non_square __board__) chip) +. (check_diagonals (List.map (fun x -> List.tl x) __board__) chip)
          else if height > width then (check_square_matrix (correct_non_square __board__) chip) +. (check_diagonals (List.tl __board__) chip) 
          else (check_square_matrix __board__ chip) in
      (* the board value is the result of subtracted all the check procedures applied for player 1's chip and player 2's chip *)
      let player_1_value = (check_rows board 1) +. (check_columns board 1) +. (check_diagonals board 1) in
      let player_2_value = (check_rows board 2) +. (check_columns board 2) +. (check_diagonals board 2) in
      let board_value = player_1_value -. player_2_value in
        board_value in
      (get_board_value board)

  (* is_game_over : state -> bool
     returns true if the game is over and false otherwise 
     List of all Relevant Helpers :
     check4 : checks if there are 4 successive occurrences of a single element in a list
     check_rows : checks all rows in board for 4 successive occurrences
     check_columns : checks all columns for 4 successive occurrences
     check_diagonals : checks all relevant diagonals for 4 occurrences of the same element
  *)
  let is_game_over ((board, player) : state) : bool =
    (* checks if there is a sublist of four identical consecutive elements *)
    let rec check4 (aloi : int list) (elmnt : int) : bool = 
      match aloi with 
        | [] -> false
        | hd :: tl -> if hd != elmnt then (check4 tl elmnt)
            else 
              match aloi with 
                | [] -> false
                | hd :: hd1 :: hd2 :: hd3 :: tl -> 
                    if [hd ; hd1 ; hd2 ; hd3] = [elmnt ; elmnt ; elmnt ; elmnt] then true
                    else (check4 (hd1 :: hd2 :: hd3 :: tl) elmnt)
                | _ -> false in 

    (* checks if there is a sequence of 4 identical 1's or 2's in any row 
       check_rows : int list list -> int -> bool
       I/P : int list list -> int -> bool 
       O/P : a boolean describing whether or not there exists a 
       connect 4 in any of the rows in the current board state*)
    let rec check_rows (board : int list list) (elmnt : int) : bool =
      match board with 
        | [] -> false 
        | hd :: tl -> if (check4 hd elmnt) then true else (check_rows tl elmnt) in

    (* checks if there exists a vertical list of 4 identical elements 
       check_columnns : int list list -> int -> bool
       I/P : the board corresponding to the current state of the game, the element to check for
       O/P : a boolean describing whether or not there exists a connect 4 in any of the columns in 
       the board. *)
    let rec check_columns (board : int list list) (elmnt: int) : bool =
      let rec check_columns_ (board : int list list) (index : int) : bool =

        (* used to get the element at the appropriate index of each row 
           get_index : int list -> int -> int -> int
           I/P : a row, an index and a count
           O/P : the element at the appropriate location *)
        let rec get_index (row : int list) (column_index : int) (count : int) : int=
          if column_index = count then (List.hd row) 
          else (get_index (List.tl row) column_index (count + 1)) in

        (* creates a column with a given input index 
           get_column : int list list -> int -> inst list
           I/P : the board state corresponding to the current state of the game. 
           O/P : the left-most column of the board of the current state of the game *)
        let get_column (board : int list list) (index : int): int list = List.map (fun x -> get_index x index 0) board in
          if (check4 (get_column board index) elmnt) then true 
          else if index = ((List.length (List.hd board)) - 1) && (not (check4 (get_column board index) elmnt)) 
          then false
          else check_columns_ board (index + 1) in
        (check_columns_ board 0) in

    (* checks the two principal diagonals of a square matrix 
       check_square_diagonals : int list list -> int -> bool
       I/P : the board corresponding to the current state of the game 
       O/P : a boolean that tells whether or not there exists a connect 4
       in either of the two principle diagonals of the matrix. *)
    let rec check_square_diagonals (board : int list list) (elmnt : int) : bool =
      (* creates a list that represents the top-right to bottom-left diagonal
         in the current matrix *)
      let rec get_diagonal board counter= 
        match board with 
          | [] -> []
          | hd :: tl -> 
              (List.nth hd counter) :: (get_diagonal tl (counter + 1)) in 
      let rev_board_y = (List.map (fun x -> List.rev x) board) in
        if check4 (get_diagonal board 0) elmnt || check4 (get_diagonal rev_board_y 0) elmnt then true else false in

    (* gets the top right hand corner submatrix of a square matrix whose width > 4.
       this procedure will not be called on a square matrix whose dimensions <= 4
       because this is the smallest possible board configuration of connect 4.
       this procedure will also not be called on a non square matrix 
       get_subsquare_matrix int list list -> int list list
       I/P : the board corresponding to the current state of the game 
       O/P : the bottom-right subsquare matrix of the board *)
    let get_subsquare_matrix (board : int list list) : int list list =
      (* truncates the matrix so that the top-right square submatrix is returned *)
      let rec truncate_row board = 
        match board with
          | [] -> failwith "Error : invalid board"
          | hd :: [] -> []
          | hd :: tl -> 
              hd :: (truncate_row tl) in 
        List.map (fun x -> List.tl x) (truncate_row board) in

    (* takes the first valid square submatrix from a non-square matrix 
       correct_non_square : int list list -> int list list
       I/P: the board corresponding to the current state of the game
       O/P: either the left-most or the top-most subsquare matrix depending on the 
       dimensions of the non-square board. *)
    let rec correct_non_square (board : int list list) : int list list =
      let height = List.length board and width = List.length (List.hd board)  in

      (* is called when the height > width to get the topmost square submatrix 
         get_by_vertical : int list list -> int -> int list list
         I/P : a board corresponding the current state of the game, a counter
         O/P : the top most subsquare matrix of the boardstate. *)
      let rec get_by_vertical (board : int list list) (hcounter : int) =
        if hcounter < width then (List.hd board) :: (get_by_vertical (List.tl board) (hcounter + 1)) else [] in

      (* used in the body of get_by_horizon to correct each row 
         correct_row : int list -> int -> int list
         I/P : a row in the boar matrix, a counter
         O/P : the row corrected to be used to build the 
         subsquare matrix. *)
      let rec correct_row (row : int list) (rcounter : int) : int list = 
        if rcounter < height then 
          (List.hd row):: (correct_row (List.tl row) (rcounter + 1))
        else [] in

      (* maps the correct_row function to every row in the matrix if 
         width > height 
         get_by_horizon : int list list -> int 
         I/P : a board, a counter 
         O/P : the left most subsquare matrix of the board*)
      let get_by_horizon (board : int list list) (rcounter : int) : int list list = 
        (List.map (fun x -> (correct_row x rcounter)) board) in
        if height > width then (get_by_vertical board 0) else (get_by_horizon board 0) in

    (* checks whether there exists a diagonal sublist of 4 consecutive identical elements
       in a board. The board does not have to be a square, and can be of an arbitrary size
       check_diagonals : board -> elmnt -> bool
       I/P : the board corresponding the current state of the game and the element to be checked for
       O/P : a bool representing whether or not there exists a diagonal containing a connect 4 *)
    let rec check_diagonals (board : int list list) (elmnt : int) : bool = 
      let height = List.length board and width = List.length (List.hd board)  in 
        if (width = 4 && height = 4) then (check_square_diagonals board (elmnt : int)) 
        else if ((width = height) && (width > 4) && (height > 4)) then 
          check_diagonals (get_subsquare_matrix board) elmnt ||
          check_diagonals (get_subsquare_matrix (List.rev board)) elmnt ||
          check_diagonals (get_subsquare_matrix (List.map (fun x -> List.rev x) board)) elmnt ||
          check_diagonals (get_subsquare_matrix (List.rev (List.map (fun x -> List.rev x) board))) elmnt
        else if ((abs (width - height)) = 1) then
          if (height = 4) then 
            check_diagonals (correct_non_square board) elmnt ||
            check_diagonals (correct_non_square (List.map (fun x -> List.rev x) board)) elmnt
          else if (width = 4) then 
            check_diagonals (correct_non_square board) elmnt ||
            check_diagonals (correct_non_square (List.rev board)) elmnt
          else if (height > width) then 
            check_diagonals (correct_non_square board) elmnt ||
            check_diagonals (correct_non_square (List.rev board)) elmnt
          else 
            check_diagonals (correct_non_square board) elmnt ||
            check_diagonals (correct_non_square (List.map (fun x -> List.rev x) board)) elmnt
        else if (width = 4) && (height > 4) then 
          check_diagonals (correct_non_square board) elmnt || (check_diagonals (List.tl board) elmnt)
        else if (height = 4) && (width > 4) then 
          check_diagonals (correct_non_square board) elmnt || (check_diagonals (List.map (fun x -> List.tl x) board)) elmnt
        else check_diagonals (correct_non_square board) elmnt ||
             (if (height > width) then check_diagonals (correct_non_square (List.tl board)) elmnt
              else check_diagonals (correct_non_square (List.map (fun x -> List.tl x) board)) elmnt) in

      match player with 
        | P1 -> if (not (List.mem 0 (List.hd (List.rev board)))) && 
                   not ((check_rows board 2) || (check_columns board 2) || (check_diagonals board 2)) then true else
            if (check_rows board 2) || (check_columns board 2) || (check_diagonals board 2) then true else false
        | P2 -> if (not (List.mem 0 (List.hd (List.rev board)))) && 
                   not ((check_rows board 1) || (check_columns board 1) || (check_diagonals board 1)) then true else
            if (check_rows board 1) || (check_columns board 1) || (check_diagonals board 1) then true else false 

  (* player one always places 1's on the board, player 2 always places 2's
     status_string : state -> string 
     I/P : a game state 
     O/P : a string that describes the state of the game *)
  let status_string ((board, player) : state) : string = 
    if (not (List.mem 0 (List.hd (List.rev board)))) then "Tie game!"
    else if (is_game_over (board, player)) then 
      match player with 
        | P1 -> "Game Over. Player 2 Wins!"
        | P2 -> "Game Over. Player 1 Wins!"
    else "The game is still in play."
end;;


module type GAME_PLAYER =
sig
  open Game

  (* given a state, and decides which legal move to make *)
  val next_move : state -> move
end ;;

(* The HumanPlayer makes its moves based on user input.
 * We've written this player for you because it uses some imperative language constructs. *)
module HumanPlayer : GAME_PLAYER =
struct
  open Game

  let move_string state =
    (string_of_player (current_player state)) ^ ", please type your move: "

  let bad_move_string str =
    "Bad move for this state: " ^ str ^ "!\n"

  let rec next_move state =
    print_string (move_string state);
    flush stdout;

    try
      let str = (input_line stdin) in
      let new_move = move_of_string str in
        if (is_legal_move state new_move) then new_move
        else (print_string (bad_move_string str); next_move state)
    with 
      | End_of_file -> (print_endline "\nEnding game"; exit 1)
      | _ -> (print_endline "That move makes no sense!"; next_move state)
end;;

module AIPlayer : GAME_PLAYER =
struct
  open Game

  let next_move state =

    (* max_list : 'a list -> 'a 
       I/P : a list of datum 
       O/P : the maximal datum in the list *)
    let rec max_list l =
      match l with
        | [] -> failwith "error : can't take the max of an empty list"
        | [a] -> a
        | hd :: hd2 :: tl -> 
            if hd > hd2 then 
              (max_list (hd :: tl)) else
              (max_list (hd2 :: tl)) in
    (* min_list : 'a list -> 'a
       I/P : a list of datum
       O/P : the minimal datum in the list*)
    let rec min_list l =
      match l with
        | [] -> failwith "error : can't take the min of an empty list"
        | [a] -> a 
        | hd :: hd2 :: tl -> 
            if hd < hd2 then 
              (min_list (hd :: tl))
            else (min_list (hd2 :: tl)) in

    let possible_moves = legal_moves state in
    let next_states = List.map (fun move -> (next_state state move), move) possible_moves in

    (* minimax : state -> int -> float
       I/P : state, a Game.state
       O/P : an estimate value that represents the 
       value of the board 5 moves down the game tree *)
    let rec minimax state counter = 
      let possible_moves = legal_moves state in
      let next_next_states = List.map (fun move -> next_state state move) possible_moves in


        match state with 
          | (board, player) -> 
              if is_game_over state then 
                match player with 
                  | P1 -> -1000000.
                  | P2 -> (1000000.) else
              if counter = 1 then 
                match player with 
                  | P1 -> max_list  (List.map (estimate_value) next_next_states) 
                  | P2 -> min_list (List.map (estimate_value) next_next_states) else

                match player with 
                  | P1 -> max_list (List.map (fun _state -> minimax _state (counter - 1)) next_next_states)
                  | P2 -> min_list (List.map (fun _state -> minimax _state (counter - 1)) next_next_states) in

    (* find_min_estimate : (float * move) list -> move *)
    let rec find_max_estimate (tuplist : (float * move) list) : move = 
      match tuplist with 
        | [] -> failwith "no max"
        | [(est, move)] -> move
        | (est1, move1) :: (est2, move2) :: tl -> 
            if est1 > est2 then 
              (find_max_estimate ((est1, move1) :: tl)) else
              (find_max_estimate ((est2, move2) :: tl)) in

    (* find_min_estimate : (float * move) list -> move *)
    let rec find_min_estimate (tuplist : (float * move) list) : move = 
      match tuplist with 
        | [] -> failwith "no max"
        | [(est, move)] -> move
        | (est1, move1) :: (est2, move2) :: tl -> 
            if est1 < est2 then 
              (find_min_estimate ((est1, move1) :: tl)) else
              (find_min_estimate ((est2, move2) :: tl)) in

    let minimax_estimates = List.map (fun (state, move) -> (minimax state 4), move) next_states in

      match state with 
        | (board, player) -> 
            match player with 
              | P1 -> find_max_estimate minimax_estimates
              | P2 -> find_min_estimate minimax_estimates

end;;

(* The Referee coordinates the other modules. 
 * Invoke start_game to start playing a game. *)
module Referee =
struct
  open Game

  module Player1 = HumanPlayer
  module Player2 = HumanPlayer

  let game_on_string player move =
    (string_of_player player) ^ " decides to make the move " ^ (string_of_move move) ^ "."

  let rec play state =
    print_endline (string_of_state state);
    if (is_game_over state) then (print_endline 
                                    ("Game over! Final result: " ^ (status_string state)); ())
    else let player = current_player state in
      let move = (match player with
                   | P1 -> Player1.next_move state
                   | P2 -> Player2.next_move state) in
        print_endline (game_on_string player move);
        play (next_state state move)

  let start_game () = play (initial_state)
end;;

Referee.start_game ();;








