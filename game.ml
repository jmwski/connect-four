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
      if width < 4 || height < 4 then failwith "Error : invalid board dimensions" else
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
     Returns either player 1 or player 2 depending on whose turn it is *)
  let string_of_player (player_ : player) : string = 
    match player_ with
      | P1 -> "Player 1"
      | P2 -> "Player 2"
  (* string_of_state: state -> string
     Prints out a matrix representation of the board *)
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
     of the current state's board list. *)
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
    let rec make_move ((hd :: tl) : int list list) : int list list = 
      if List.nth hd move_ != 0 then hd :: (make_move tl)
      else 
        match player with 
          | P1 -> (into_row hd move_ 1 0) :: tl
          | P2 -> (into_row hd move_ 2 0) :: tl in
      match player with 
        | P1 -> ((make_move board), P2)
        | P2 -> ((make_move board), P1)
  (* estimate value will assing a weight to every possible move from a given state  *)
  let estimate_value ((board, player) : state) : float = 
    failwith "Error : procedure not yet written"
  (* is_game_over : state -> bool
     returns true if the game is over and false otherwise 
     List of all Relevant Helpers :
     check4 : checks if there are 4 successive occurrences of a single element in a list
     check_rows : checks all rows in board for 4 successive occurrences
     check_columns : checks all columns for 4 successive occurrences
     check_diagonals : checks all relevant diagonals for 4 occurrences of the same element
     crop_aoi : crops the board state to the smallest possible rectangle containing all non-vacant entries *)
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
    (* checks if there is a sequence of 4 identical 1's or 2's in any row *)
    let rec check_rows (board : int list list) (elmnt : int) : bool =
      match board with 
        | [] -> false 
        | hd :: tl -> if (check4 hd elmnt) then true else (check_rows tl elmnt) in 
    (* checks if there exists a vertical list of 4 identical elements  *)
    let rec check_columns (board : int list list) (elmnt: int) =
      let rec check_columns_ (board : int list list) (index : int) : bool =
        (* used to get the element at the appropriate index of each row *)
        let rec get_index ((bhd :: btl) : int list) (column_index : int) (count : int) =
          if column_index = count then bhd 
          else (get_index btl column_index (count + 1)) in
        (* creates a column with a given input index  *)
        let get_column (board : int list list) (index : int) = List.map (fun x -> get_index x index 0) board in
          if (check4 (get_column board index) elmnt) then true 
          else if index = ((List.length (List.hd board)) - 1) && (not (check4 (get_column board index) elmnt)) 
          then false
          else check_columns_ board (index + 1) in
        (check_columns_ board 0) in
    (* checks the two principal diagonals of a square matrix *)
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
       this procedure will also not be called on a non square matrix *)
    let get_subsquare_matrix (board : int list list) : int list list =
      (* truncates the matrix so that the top-right square submatrix is returned *)
      let rec truncate_row board = 
        match board with
          | hd :: [] -> []
          | hd :: tl -> 
              hd :: (truncate_row tl) in 
        List.map (fun x -> match x with | rhd :: rtl -> rtl) (truncate_row board) in
    (* takes the first valid square submatrix from a non-square matrix *)
    let rec correct_non_square ((hd :: tl) : int list list) : int list list =
      let height = List.length (hd :: tl) and width = List.length hd in
      (* is called when the height > width to get the topmost square submatrix *)
      let rec get_by_vertical ((hd :: tl) : int list list) (hcounter : int) =
        if hcounter < width then hd :: (get_by_vertical tl (hcounter + 1)) else [] in
      (* used in the body of get_by_horizon to correct each row *)
      let rec correct_row ((rhd :: rtl) : int list) (rcounter : int) = 
        if rcounter < height then 
          rhd :: (correct_row rtl (rcounter + 1))
        else [] in
      (* maps the correct_row function to every row in the matrix if 
         width > height *)
      let get_by_horizon ((hd :: tl) : int list list) (rcounter : int) = 
        (List.map (fun x -> (correct_row x rcounter)) (hd :: tl)) in
        if height > width then (get_by_vertical (hd :: tl) 0) else (get_by_horizon (hd :: tl) 0) in
    (* checks whether there exists a diagonal sublist of 4 consecutive identical elements
       in a board. The board does not have to be a square, and can be of an arbitrary size *)
    let rec check_diagonals ((hd :: tl) : int list list) (elmnt : int) : bool = 
      let height = List.length (hd :: tl) and width = List.length hd in 
        if (width = 4 && height = 4) then (check_square_diagonals (hd :: tl) (elmnt : int)) 
        else if ((width = height) && (width > 4) && (height > 4)) then 
          check_diagonals (get_subsquare_matrix (hd :: tl)) elmnt ||
          check_diagonals (get_subsquare_matrix (List.rev (hd :: tl))) elmnt ||
          check_diagonals (get_subsquare_matrix (List.map (fun x -> List.rev x) (hd :: tl))) elmnt ||
          check_diagonals (get_subsquare_matrix (List.rev (List.map (fun x -> List.rev x) (hd :: tl)))) elmnt
        else if ((abs (width - height)) = 1) then
          if (height = 4) then 
            check_diagonals (correct_non_square (hd :: tl)) elmnt ||
            check_diagonals (correct_non_square (List.map (fun x -> List.rev x) (hd :: tl))) elmnt
          else if (width = 4) then 
            check_diagonals (correct_non_square (hd :: tl)) elmnt ||
            check_diagonals (correct_non_square (List.rev (hd :: tl))) elmnt
          else if (height > width) then 
            check_diagonals (correct_non_square (hd :: tl)) elmnt ||
            check_diagonals (correct_non_square (List.rev (hd :: tl))) elmnt
          else 
            check_diagonals (correct_non_square (hd :: tl)) elmnt ||
            check_diagonals (correct_non_square (List.map (fun x -> List.rev x) (hd :: tl))) elmnt
        else if (width = 4) && (height > 4) then 
          check_diagonals (correct_non_square (hd :: tl)) elmnt || (check_diagonals tl elmnt)
        else if (height = 4) && (width > 4) then 
          check_diagonals (correct_non_square (hd :: tl)) elmnt || (check_diagonals (List.map (fun x -> List.tl x) (hd :: tl))) elmnt
        else check_diagonals (correct_non_square (hd :: tl)) elmnt ||
             (if (height > width) then check_diagonals (correct_non_square tl) elmnt
              else check_diagonals (correct_non_square (List.map (fun x -> List.tl x) (hd :: tl))) elmnt) in
      match player with 
        | P1 -> if (check_rows board 2) || (check_columns board 2) || (check_diagonals board 2) then true else false
        | P2 -> if (check_rows board 1) || (check_columns board 1) || (check_diagonals board 1) then true else false
  (* crops the board state to the smallest possible rectangle containing all the non
     vacant entries *)
  (* let crop_aoi (board : int list list) = 
     (* crops the height of the board *)
     let rec crop_height (hd :: tl : int list list) = 
     if (List.mem 1 hd) || (List.mem 2 hd) then hd :: (crop_height tl) else [] in
     (* crops the width of the board in one direction -- in the function call this procedure 
     is called on the reverse of the original cropped list *)
     let rec crop_width ((hd :: tl) : int list list) =
     match hd with 
     | [] -> [[]]
     | rhd :: rtl -> if rhd = 0 then (crop_width (List.map (fun x -> List.tl x) (hd :: tl))) else ((rhd :: rtl) :: tl) in
     crop_height (List.map (fun x -> List.rev x) 
     (crop_width 
     (List.map (fun x -> List.rev x) 
     (crop_width board)))) in 
     (* creates cropped board *)
     let aoi = crop_aoi board in 
     match player with 
     | P1 -> if ((check_rows aoi 2) || (check_columns aoi 2) || (check_diagonals aoi 2)) then true else false 
     | P2 -> if ((check_rows aoi 1) || (check_columns aoi 1) || (check_diagonals aoi 1)) then true else false *)
  (* player one always places 1's on the board, player 2 always places 2's *)
  let status_string ((board, player) : state) : string = 
    if (not (List.mem 0 (List.hd (List.rev board)))) && (not (is_game_over (board, player))) then "Tie game!"
    else if (is_game_over (board, player)) then 
      match player with 
        | P1 -> "Game Over. Player 2 Wins!"
        | P2 -> "Game Over. Player 1 Wins!"
    else "The game is still in play."
end;;



