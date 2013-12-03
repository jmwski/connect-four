#use "player_signature.ml" ;;

(* The HumanPlayer makes its moves based on user input. *)
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
end ;;
