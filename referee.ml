#use "human_player.ml" ;;
#use "ai_player.ml";;

(* The Referee coordinates the other modules. 
 * Invoke start_game to start playing a game. *)
module Referee =
struct
  open Game

  module Player1 = AIPlayer
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
end ;;

Referee.start_game () ;;
