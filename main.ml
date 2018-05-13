open Command

(* Returns: true if id is a duplicate of an element in lst, case insensitive
   Requires: lst is a list of (player_id, player_type) *)
let rec is_dup id lst =
  match lst with
  | [] -> false
  | (x, _) :: t ->
    if String.lowercase_ascii x = String.lowercase_ascii id then true
    else is_dup id t

(* Returns: State.player list
   Prompt players for player names and AIs. *)
let rec get_players acc (num:int): (string * State.player_type) list =
  if num = 5 then List.rev acc
  else begin
    print_endline "------------------------------------------------------------------------------";
    print_endline ("Enter name of player " ^ (string_of_int num));
    print_endline ("Do not include spaces in player name");
    print_endline "(Enter \"Done\" if finished, require 2-4 players)";
    print_endline ("(Enter \"Bot1\", \"Bot2\", \"Bot3\", \"Bot4\"" ^
                   " if you want a smart AI to play the game)");
    print_string  "> ";
    let s = read_line () in
    let s' = String.lowercase_ascii s in
    if (s' = "done" && (num = 1 || num = 2))
    then (print_endline "You need 2-4 players.\n";
          get_players acc num)
    else if s' = "done"
    then (print_endline "\nFinished inputing player names.\n";
          List.rev acc)
    else
    if (s' = "bot1" || s' = "bot2" || s' = "bot3" || s' = "bot4")
    then (print_endline ("Player " ^ string_of_int num ^ " is a smart bot\n");
          get_players ((s,AI)::acc) (num+1))
    else if String.length s > 8
    then (print_endline "\nPlayer names must be 8 characters or less.\n";
          get_players acc num)
    else if is_dup s acc
    then (print_endline "\nThis name is already taken.\n";
          get_players acc num)
    else (print_endline ("\nPlayer " ^ string_of_int num ^ " is: " ^ s  ^ "\n");
          get_players ((s,Human)::acc) (num+1))
  end

(* A safer version of State.do that handles exceptions and failwith messages *)
let safe_do' (st:State.state) (c:command) : State.state option =
  try Some (State.do' st c)
  with Failure s -> print_endline (s ^ "\n"); None

(* [Parse_trade cur s opt] is the command option of a
   tradeforprop or tradeawayprop preroll command *)
let parse_trade (cur:string) (s:string) (opt:string): command option =
  let rec merge lst acc =
    match lst with
    |[] -> acc
    |h::t -> h ^ " " ^ (merge t acc) in
  let lst = String.split_on_char ' ' s in
  if List.length lst < 3
  then begin print_endline ("Invalid preroll command: " ^
                            "\"TradeForProp\" and \"TradeAwayProp\" " ^
                            "need 3 arguments: "^
                            "playerid, propertyid & money\n");
    None end
  else
    let playerid = List.nth lst 0 in
    let amount_str = List.nth lst (List.length lst - 1) in
    let amount_int = int_of_string_opt amount_str in
    match amount_int with
    | None ->
      print_endline ("Invalid preroll command: " ^
                     "\"TradeForProp\" and \"TradeAwayProp\" " ^
                     "need last argument: money needs to be type integer\n");
      None
    | Some i ->
      let propid = String.trim(merge List.(lst |> tl |> rev |> tl |> rev) "") in
      if opt = "for"
      then begin
        print_endline ("Preroll command: Trade for property \"" ^ propid ^
                       "\" from \"" ^ playerid ^
                       "\" for \"" ^ amount_str ^"\"\n");
        Some (Preroll (TradeForProp (playerid, propid, i)))
      end
      else begin
        print_endline ("Preroll command: Trade away property \"" ^ propid ^
                       "\" to \"" ^ playerid ^
                       "\" for \"" ^ amount_str ^ "\"\n");
        Some (Preroll (TradeAwayProp (playerid, propid, i)))
      end

(* Prompts user for "yes" or "no", repeats prompt if no y/n answer given *)
let rec yes_no ans =
match ans with
| "yes" -> "yes"
| "no" -> "no"
| _ ->
  (print_endline "\nPlease enter [yes] or [no].");
  print_string  "> ";
  yes_no (String.lowercase_ascii (read_line ()))

(* Returns: list of player_ids of players on space of given index
   Requires: st is a valid state and i < State.board_size *)
let rec players_on_space (lst : State.player list) i =
  match lst with
  | [] -> []
  | h :: t ->
    if h.position = i then h.player_id :: (players_on_space t i)
    else players_on_space t i

(* Returns: contents of given option
   Requires: input is not None *)
let get_op = function
  | None -> failwith "None"
  | Some x -> x

(* Returns: ()
   Prints player_ids in lst, formatted to keep them in line
   Requires: n is 0 *)
let rec print_players n lst =
  match lst with
  | [] -> print_string (String.make (40 - n * 10) '.')
  | h :: t ->
    print_string h;
    print_string (String.make (10 - (String.length h)) '.');
    print_players (n + 1) t

(* Returns: name of given space *)
let space_name (space : State.space) =
  match space.space_type with
  | Property ->
    (match space.property with
     | None -> "Error"
     | Some p -> p)
  | Jail -> "Office Hours"
  | Chest -> "Community Chest"
  | Mystery -> "Chance"
  | Go -> "Go"
  | GoJail -> "Go to Office Hours"
  | Free -> "Free Parking"

(* Returns: ()
   Prints info for space i *)
let print_space (st : State.state) i =
  let space = List.nth st.board.spaces i in
  let name = space_name space in
  let players = players_on_space st.players i in
  let owner =
    match space.space_type with
    | Property -> (State.get_prop st (get_op space.property)).owner
    | _ -> None in
  let index = string_of_int (i + 1) in
  print_string (index ^ (if String.length index = 1 then "." else ""));
  print_string "..";
  print_string name;
  print_string (String.make (22 - (String.length name)) '.');
  print_string
    (match owner with
     | None -> "............"
     | Some x -> x ^ (String.make (12 - (String.length x)) '.'));
  print_players 0 players;
  print_string "\n"

(* Returns: ()
   Displays board representation by printing each space in order along with info
   about the space *)
let print_board st =
  print_endline "\n----------------------------------- BOARD ------------------------------------\n";
  print_endline "    Space                 Owner       Players";
  for i = 0 to (State.board_size - 1) do
    print_space st i
  done;
  print_endline ""

(* Prints info on properties owned by one player *)
let rec print_player_prop (st : State.state) (props : State.property list) =
  match props with
  | [] -> print_endline ""
  | p :: t ->
    print_string "------";
    print_string p.prop_id;
    print_string (String.make (22 - (String.length p.prop_id)) '.');
    let color =
      match p.color with
      | Red -> "Red"
      | Orange -> "Orange"
      | Yellow -> "Yellow"
      | Green -> "Green"
      | LBlue -> "Light Blue"
      | DBlue -> "Dark Blue"
      | Pink -> "Pink"
      | Brown -> "Brown" in
    print_string color;
    print_string (String.make (13 - (String.length color)) '.');
    let rent = "$" ^ string_of_int ((List.nth p.rent p.buildings)
                                    * (st.color_mult p.color)) in
    print_string rent;
    print_string (String.make (15 - (String.length rent)) '.');
    print_string (string_of_int (if p.buildings = 5 then 4 else p.buildings));
    print_string (String.make 10 '.');
    print_string (string_of_int (if p.buildings = 5 then 1 else 0));
    print_string (String.make 10 '.');
    print_endline "";
    print_player_prop st t

(* Prints stats of current player in state *)
let print_stats (st : State.state) =
  let p = State.get_player st st.current_player in
  print_endline "\n------------------------------- PLAYER STATS ---------------------------------\n";
  print_endline ("Player: " ^ st.current_player);
  print_endline ("---Current position: "
                 ^ space_name (List.nth st.board.spaces p.position));
  print_endline ("---Money: $" ^ string_of_int p.cash);
  print_endline ("---Owned properties:");
  print_endline "      Property              Color        Current Rent   # Houses   # Hotels";
  print_player_prop st (State.get_player_prop p st)

(* Process the preroll phase of each turn *)
let preroll_human (st:State.state) (i:int) (c:string) : State.state * int =
  let s = String.lowercase_ascii c in
  match String.index_opt s ' ' with
  | None -> begin
      match s with
      | "roll" -> begin
          let command = Preroll Roll in
          match safe_do' st command with
          | Some st' -> (st', 1)
          | None -> (st, i)
        end
      | "board" -> print_board st; (st, i)
      | "stats" -> print_stats st; (st, i)
      | _ ->
        print_endline "\nPlease enter a valid command.\n";
        (st, i)
    end
  | Some index -> begin
      let before = String.sub s 0 index in
      let after = String.sub s (index + 1) (String.length s - index - 1) in
      match before with
      | "build" -> begin
          let command = Preroll (Build after) in
          match safe_do' st command with
          | Some st' -> (st', i)
          | None -> (st, i)
        end
      | "tradeforprop" -> begin
          match parse_trade st.current_player after "for" with
          | Some command -> begin
              match safe_do' st command with
              | Some st' -> begin
                  print_endline "Do you accept the trade? Enter [yes] or [no].";
                  let ans = yes_no (String.lowercase_ascii (read_line ())) in
                  match ans with
                  | "yes" -> print_endline "Trade accepted!\n";
                    (st', i)
                  | "no" -> print_endline "Trade denied!\n";
                    (st, i)
                  | _ -> failwith "impossible"
                end
              | None -> (st, i)
            end
          | None -> (st, i)
        end
      | "tradeawayprop" -> begin
          match parse_trade st.current_player after "away" with
          | Some command -> begin
              match safe_do' st command with
              | Some st' -> begin
                  print_endline "Do you accept the trade? Enter [yes] or [no].";
                  let ans = yes_no (String.lowercase_ascii (read_line ())) in
                  match ans with
                  | "yes" -> print_endline "Trade accepted!\n";
                    (st', i)
                  | "no" -> print_endline "Trade denied!\n";
                    (st, i)
                  | _ -> failwith "impossible"
                end
              | None -> (st, i)
            end
          | None -> (st, i)
        end
      | _ ->
        print_endline "Invalid (multi word) preroll command, see \"Help\"\n";
        (st, i)
    end

(* Process the postroll phase of each turn *)
let rec postroll_human (st:State.state) (i:int) : State.state * int =
  match State.get_space_type st st.current_player with
  | Free ->
    let st' = State.do' st (Postroll FinishTurn) in
    (st', 2)
  | Property -> begin
      let cur_space = State.get_space st st.current_player in
      let prop_name = State.prop_of_space cur_space.space_id in
      if (State.has_owner st prop_name &&
          State.owns_prop st st.current_player prop_name)
      then begin
        print_endline ("You already own this property.");
        let st' = State.do' st (Postroll FinishTurn) in
        (st', 2)
      end
      else if (State.has_owner st prop_name &&
               State.owns_prop st st.current_player prop_name <> true)
      then begin
        let command = Postroll (PayRent prop_name) in
        let st' = State.do' st command in
        let st'' = State.do' st' (Postroll FinishTurn) in
        (st'', 2)
      end
      else begin
        print_endline ("Do you wish to buy this property? Enter [yes] or [no].");
        print_string  "> ";
        let s = yes_no (String.lowercase_ascii (read_line ())) in
        match s with
        | "yes" ->
          let command = Postroll (Buy prop_name) in
          begin
            match safe_do' st command with
            | Some st' -> (State.do' st' (Postroll FinishTurn), 2)
            | None -> (State.do' st (Postroll FinishTurn), 2)
          end
        | "no" ->
          let st' = State.do' st (Postroll FinishTurn) in
          (st', 2)
        | _ -> failwith "impossible"
      end
    end
  | Jail ->
    let st' = State.do' st (Postroll FinishTurn) in
    (st', 2)
  | Chest ->
    let command = Postroll CommChest in
    let st' = State.do' st command in
    let st'' = State.do' st' (Postroll FinishTurn) in
    (st'', 2)
  | Mystery ->
    let command = Postroll MysteryMove in
    let st' = State.do' st command in
    postroll_human st' 1
  | Go ->
    let st' = State.do' st (Postroll FinishTurn) in
    (st', 2)
  | GoJail ->
    let command = Postroll GoJail in
    let st' = State.do' st command in
    let st'' = State.do' st' (Postroll FinishTurn) in
    (st'', 2)

(*let rec preroll_bot (st:State.state) (i:int) : State.state * int =
  let command = Controller.preroll st in
  match command with
  |Preroll Roll ->
    let st' = State.do' st command in
    (st', 1)
  |Preroll (Build s) -> (st, i)
  |Preroll (Trade (s1, s2, s3, i)) -> (st, i)*)

(* Process the bankrupt phase of each turn *)

let rec bankrupt (st:State.state) (i:int) : State.state * int =
  let is_bankrupt = State.get_bankrupt st in
  match is_bankrupt with
  | [] -> (st, 0)
  | h::t -> begin
      print_endline ("Player " ^ h ^ " is bankrupt.");
      print_endline ("Do you accept the bankruptcy? Enter [Accept] or [Fight].");
      let s = String.lowercase_ascii (read_line ()) in
      match s with
      | "accept"->
        print_endline ("You decided to accept bankruptcy.");
        let command = Bankrupt (Accept h) in
        let st' = State.do' st command in
        (st', i)
      | _->
        print_endline ("You decided to fight bankruptcy by trading away props. ");
        print_endline ("tradeaway command: ");
        let s = String.lowercase_ascii (read_line ()) in
        match String.index_opt s ' ' with
        | None ->
          print_endline ("Invalid tradeawayprop command");
          (st, i)
        | Some index -> begin
            let before = String.sub s 0 index in
            let after = String.sub s (index + 1) (String.length s - index - 1) in
            match before with
            | "tradeawayprop" -> begin
                match parse_trade st.current_player after "away" with
                | Some command -> begin
                    match safe_do' st command with
                    | Some st' -> begin
                        print_endline ("Do you accept the trade? Enter [yes] to accept,"
                                       ^ "otherwise enter anything other than [yes] to reject");
                        let ans = String.lowercase_ascii (read_line ()) in
                        match ans with
                        | "yes" -> print_endline "Trade accepted!\n";
                          (st', i)
                        | _ -> print_endline "Trade denied!\n";
                          (st, i)
                      end
                    | None -> (st, i)
                  end
                | None -> (st, i)
              end
            | _->
              print_endline ("Invalid tradeawayprop command");
              (st, i)
          end
    end

(* [winner st] is [""] is there is no winner, else is winner player's name *)
let winner (st:State.state) : string =
  match State.check_win st with
  | None -> ""
  | Some x -> x

(* Loop through each player's turn's individual phase*)
let rec loop ((st:State.state),(i:int)) : unit =
  if (State.get_player st st.current_player).player_type = Human then
    if winner st = st.current_player then
      print_endline ("Player " ^ st.current_player ^ " has won")
    else if i = 1
    then loop (postroll_human st i)
    else if i = 2
    then loop (bankrupt st i)
    else begin
      print_endline "------------------------------------------------------------------------------";
      print_endline ("Current player is " ^ st.current_player ^ "\n");
      print_endline "What would you like to do? Use the following commands:";
      print_endline "---Board **View board**";
      print_endline "---Stats **View player information**";
      print_endline "---Build [Property name]";
      print_endline "---TradeForProp [Player name] [Property name] [Amount offered]";
      print_endline "---TradeAwayProp [Player name] [Property name] [Amount to receive]";
      print_endline "---TradeProps [Player name] [Property to give] [Property to receive]";
      print_endline "---Roll";
      print_endline "---Quit";
      print_string  "> ";
      let s = String.lowercase_ascii (read_line ()) in
      match s with
      | "quit" -> print_endline "\nYou have quit the game.\n";
      | c -> loop (preroll_human st i c)
    end
  else
  if winner st = st.current_player then
    print_endline ("Bot " ^ st.current_player ^ " has won")
  else if i = 1
  then loop (postroll_human st i)
  else if i = 2
  then loop (bankrupt st i)
  else begin
    print_endline "------------------------------------------------------------------------------";
    print_endline ("Current bot is " ^ st.current_player);
    print_endline "Enter command below:";
    print_string  "> ";
    let s = String.lowercase_ascii (read_line ()) in
    loop (preroll_human st i s)
  end

(* Initiates the game *)
let main () : unit =
  print_endline "\n------------------------------------------------------------------------------";
  print_endline "WELCOME TO CORNELL MONOPOLY.";
  let playerids = get_players [] 1 in
  (State.init_state playerids, 0) |> loop

(* Initiates the game of monopoly *)
let () = main ()
