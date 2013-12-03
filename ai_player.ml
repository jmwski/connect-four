#use "player_signature.ml" ;;

(* Fill in the code for your AI player here. *)
(* Note that your AI player simply needs to decide on its next move.
 * It does not have to worry about printing to the screen or any of
 * the other silliness in the HumanPlayers's chunk of code. *)
module AIPlayer : GAME_PLAYER =
struct
  open Game

  let next_move state = 
    let l = legal_moves(state) in 
      (List.hd l)
end ;;
