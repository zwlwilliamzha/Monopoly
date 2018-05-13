open Command

(* ============================== TYPES ============================== *)

type prop_color = Red | Orange | Yellow | Green | LBlue | DBlue | Pink | Brown
type space_type = Property | Jail | Chest | Mystery | Go | GoJail | Free
type building = House | Hotel
type player_type = Human | AI

(* fst: amount of money given or taken
   snd: whether action applies to current player only or all players *)
type chest_card = int * [`All | `Current]

type mystery_card = int

(* Must be length 6, with the values corresponding to:
   base rent, 1 house, 2 houses, 3 houses, 4 houses, hotel *)
type rent_values = int list

type property = {
  prop_id: string;
  color: prop_color;
  price: int;
  rent: rent_values;
  owner: string option;
  buildings: int;
  building_cost: int;
}

type player = {
  player_type: player_type;
  player_id: string;
  properties: string list;
  position: int;
  cash: int;
  in_jail: bool * int;
}

type space = {
  space_id: int;
  space_type: space_type;
  property: string option;
}

type board = {
  spaces: space list;
  chest_cards: chest_card list;
  mystery_cards: mystery_card list;
}

type state = {
  board: board;
  current_player: string;
  players: player list;
  player_order: string list;
  properties: property list;
  color_mult: prop_color -> int;
}

(* ============================== BOARD ============================== *)
let board_size = 36
let jail_space = 9

let prop_by_color = function
  | Red -> ["Arts Quad"; "Goldwin Smith"; "PSB"]
  | Orange -> ["Uris"; "Okies"; "Clocktower"]
  | Yellow -> ["Risley"; "Dickson"; "Nasties"]
  | Green -> ["Plantations"; "Dairy Bay"; "Vet School"]
  | LBlue -> ["Gates"; "Duffield"; "Eng Quad"]
  | DBlue -> ["Mann"; "Ag Quad"; "Kennedy"]
  | Pink -> ["Slope"; "Noyes"; "Baker Flagpole"]
  | Brown -> ["Ives"; "Statler"; "Sage";]

let all_properties : property list = [
  {
    prop_id = "Ives";
    color = Brown;
    price = 140;
    rent = [10; 50; 150; 450; 625; 750];
    owner = None;
    buildings = 0;
    building_cost = 100;
  }; {
    prop_id = "Statler";
    color = Brown;
    price = 350;
    rent = [35; 175; 500; 1100; 1300; 1500];
    owner = None;
    buildings = 0;
    building_cost = 200;
  }; {
    prop_id = "Sage";
    color = Brown;
    price = 280;
    rent = [26; 120; 360; 850; 1025; 1200];
    owner = None;
    buildings = 0;
    building_cost = 150;
  }; {
    prop_id = "Gates";
    color = LBlue;
    price = 320;
    rent = [28; 150; 450; 1000; 1200; 1400];
    owner = None;
    buildings = 0;
    building_cost = 200;
  }; {
    prop_id = "Duffield";
    color = LBlue;
    price = 320;
    rent = [28; 150; 450; 1000; 1200; 1400];
    owner = None;
    buildings = 0;
    building_cost = 200;
  }; {
    prop_id = "Eng Quad";
    color = LBlue;
    price = 180;
    rent = [14; 70; 200; 550; 750; 950];
    owner = None;
    buildings = 0;
    building_cost = 100;
  }; {
    prop_id = "Slope";
    color = Pink;
    price = 220;
    rent = [18; 90; 250; 700; 875; 1050];
    owner = None;
    buildings = 0;
    building_cost = 150;
  }; {
    prop_id = "Noyes";
    color = Pink;
    price = 160;
    rent = [12; 60; 180; 500; 700; 900];
    owner = None;
    buildings = 0;
    building_cost = 100;
  }; {
    prop_id = "Baker Flagpole";
    color = Pink;
    price = 60;
    rent = [4; 20; 60; 180; 320; 450];
    owner = None;
    buildings = 0;
    building_cost = 50;
  }; {
    prop_id = "Uris";
    color = Orange;
    price = 160;
    rent = [12; 60; 180; 500; 700; 900];
    owner = None;
    buildings = 0;
    building_cost = 100;
  }; {
    prop_id = "Okies";
    color = Orange;
    price = 400;
    rent = [50; 200; 600; 1400; 1700; 2000];
    owner = None;
    buildings = 0;
    building_cost = 200;
  }; {
    prop_id = "Clocktower";
    color = Orange;
    price = 150;
    rent = [6; 30; 90; 270; 400; 550];
    owner = None;
    buildings = 0;
    building_cost = 50;
  }; {
    prop_id = "Arts Quad";
    color = Red;
    price = 180;
    rent = [14; 70; 200; 550; 750; 950];
    owner = None;
    buildings = 0;
    building_cost = 100;
  }; {
    prop_id = "Goldwin Smith";
    color = Red;
    price = 60;
    rent = [4; 20; 60; 180; 320; 450];
    owner = None;
    buildings = 0;
    building_cost = 50;
  }; {
    prop_id = "PSB";
    color = Red;
    price = 320;
    rent = [28; 150; 450; 1000; 1200; 1400];
    owner = None;
    buildings = 0;
    building_cost = 200;
  }; {
    prop_id = "Risley";
    color = Yellow;
    price = 100;
    rent = [6; 30; 90; 270; 400; 550];
    owner = None;
    buildings = 0;
    building_cost = 50;
  }; {
    prop_id = "Dickson";
    color = Yellow;
    price = 140;
    rent = [10; 50; 150; 450; 625; 750];
    owner = None;
    buildings = 0;
    building_cost = 100;
  }; {
    prop_id = "Nasties";
    color = Yellow;
    price = 140;
    rent = [10; 50; 150; 450; 625; 750];
    owner = None;
    buildings = 0;
    building_cost = 100;
  }; {
    prop_id = "Plantations";
    color = Green;
    price = 220;
    rent = [18; 90; 250; 700; 875; 1050];
    owner = None;
    buildings = 0;
    building_cost = 150;
  }; {
    prop_id = "Vet School";
    color = Green;
    price = 280;
    rent = [26; 120; 360; 850; 1025; 1200];
    owner = None;
    buildings = 0;
    building_cost = 150;
  }; {
    prop_id = "Dairy Bar";
    color = Green;
    price = 160;
    rent = [12; 60; 180; 500; 700; 900];
    owner = None;
    buildings = 0;
    building_cost = 100;
  }; {
    prop_id = "Mann";
    color = DBlue;
    price = 350;
    rent = [35; 175; 500; 1100; 1300; 1500];
    owner = None;
    buildings = 0;
    building_cost = 200;
  }; {
    prop_id = "Ag Quad";
    color = DBlue;
    price = 180;
    rent = [14; 70; 200; 550; 750; 950];
    owner = None;
    buildings = 0;
    building_cost = 100;
  }; {
    prop_id = "Kennedy";
    color = DBlue;
    price = 280;
    rent = [26; 120; 360; 850; 1025; 1200];
    owner = None;
    buildings = 0;
    building_cost = 150;
  }
]

let all_chest : chest_card list =
  [(-50, `All); (-50, `Current); (100, `All); (80, `Current);
   (-100, `All); (-200, `Current); (200, `All); (200, `Current)]

let all_mystery : mystery_card list = [5; -10; 7; 1; -2; 4; 8; 3; 6; -1; -5]

let board_spaces : space list = [
  {
    space_id = 0;
    space_type = Go;
    property = None;
  }; {
    space_id = 1;
    space_type = Property;
    property = Some "Ives";
  }; {
    space_id = 2;
    space_type = Chest;
    property = None;
  }; {
    space_id = 3;
    space_type = Property;
    property = Some "Statler";
  }; {
    space_id = 4;
    space_type = Property;
    property = Some "Sage";
  }; {
    space_id = 5;
    space_type = Property;
    property = Some "Gates";
  }; {
    space_id = 6;
    space_type = Mystery;
    property = None;
  }; {
    space_id = 7;
    space_type = Property;
    property = Some "Duffield";
  }; {
    space_id = 8;
    space_type = Property;
    property = Some "Eng Quad";
  }; {
    space_id = 9;
    space_type = Jail;
    property = None;
  }; {
    space_id = 10;
    space_type = Property;
    property = Some "Slope";
  }; {
    space_id = 11;
    space_type = Chest;
    property = None;
  }; {
    space_id = 12;
    space_type = Property;
    property = Some "Noyes";
  }; {
    space_id = 13;
    space_type = Property;
    property = Some "Baker Flagpole";
  }; {
    space_id = 14;
    space_type = Property;
    property = Some "Uris";
  }; {
    space_id = 15;
    space_type = Chest;
    property = None;
  }; {
    space_id = 16;
    space_type = Property;
    property = Some "Okies";
  }; {
    space_id = 17;
    space_type = Property;
    property = Some "Clocktower";
  }; {
    space_id = 18;
    space_type = Free;
    property = None;
  }; {
    space_id = 19;
    space_type = Property;
    property = Some "Arts Quad";
  }; {
    space_id = 20;
    space_type = Property;
    property = Some "Goldwin Smith";
  }; {
    space_id = 21;
    space_type = Mystery;
    property = None;
  }; {
    space_id = 22;
    space_type = Property;
    property = Some "PSB";
  }; {
    space_id = 23;
    space_type = Property;
    property = Some "Risley";
  }; {
    space_id = 24;
    space_type = Property;
    property = Some "Dickson";
  }; {
    space_id = 25;
    space_type = Chest;
    property = None;
  }; {
    space_id = 26;
    space_type = Property;
    property = Some "Nasties";
  }; {
    space_id = 27;
    space_type = GoJail;
    property = None;
  }; {
    space_id = 28;
    space_type = Property;
    property = Some "Plantations";
  }; {
    space_id = 29;
    space_type = Property;
    property = Some "Vet School";
  }; {
    space_id = 30;
    space_type = Chest;
    property = None;
  }; {
    space_id = 31;
    space_type = Property;
    property = Some "Dairy Bar";
  }; {
    space_id = 32;
    space_type = Property;
    property = Some "Mann";
  }; {
    space_id = 33;
    space_type = Property;
    property = Some "Ag Quad";
  }; {
    space_id = 34;
    space_type = Mystery;
    property = None;
  }; {
    space_id = 35;
    space_type = Property;
    property = Some "Kennedy";
  };
]

let board1 : board = {
  spaces = board_spaces;
  chest_cards = all_chest;
  mystery_cards = all_mystery;
}

(* ========================== INIT_STATE HELPERS ========================== *)

(* Returns: list of players
   Requires: list of (player_id, player_type) with length 2-4 *)
let create_players names =
  let rec f = function
    | [] -> []
    | (name, p_type) :: t ->
      {
        player_type = p_type;
        player_id = name;
        properties = [];
        position = 0;
        cash = 1000;
        in_jail = false, 0;
      } :: f t in
  names |> f |> List.rev

(* Returns: list of player_ids corresponding to given player list
   Requires: players is a valid player list *)
let get_ids players =
  let rec f = function
    | [] -> []
    | h :: t -> h.player_id :: f t in
  players |> f |> List.rev

(* ============================== DO' HELPERS ============================== *)

(* Returns: x mod n, modified so that negative numbers count down from n
   Requires: x >= -n *)
let mod' x n =
  if x >= 0 then x mod n
  else x + n

(* Returns: player with given player_id
   Requires: id is a player_id found in state *)
let get_player st id =
  let rec f = function
    | [] -> failwith "\nPlayer does not exist."
    | h :: t ->
      if String.lowercase_ascii h.player_id = String.lowercase_ascii id then h
      else f t
  in f st.players

(* Returns: property with given prop_id
   Requires: id is a prop_id found in state *)
let get_prop st id =
  let rec f = function
    | [] -> failwith "\nProperty does not exist."
    | h :: t ->
      if String.lowercase_ascii h.prop_id = String.lowercase_ascii id then h
      else f t
  in f st.properties

(* Returns: property list of player p
   Requires: p is a valid player, st is a valid state *)
let get_player_prop (p : player) st =
  let rec f acc lst =
    match lst with
    | [] -> acc
    | h :: t -> f ((get_prop st h) :: acc) t in
  f [] p.properties

(* Returns: true if player is the owner of prop and false otherwise *)
let is_owner st player prop =
  match prop.owner with
  | None -> false
  | Some owner ->
    String.lowercase_ascii player.player_id = String.lowercase_ascii owner

(* Returns number of properties with the same color in the list
   Requires lst is a list of properties and colr is a vaild property color *)
let rec same_color lst colr acc =
  match lst with
  | [] -> acc
  | h::t ->
    if h.color = colr then same_color t colr (acc+1)
    else same_color t colr acc

(* Returns: list of players where the player named id is removed
   Requires: input list is a list of players *)
let rec remove_player id = function
  | [] -> []
  | h :: t ->
    if String.lowercase_ascii h.player_id = String.lowercase_ascii id
    then remove_player id t
    else h :: (remove_player id t)

(* Returns: list of properties where the property named id is removed
   Requires: input list is a list of properties *)
let rec remove_prop id = function
  | [] -> []
  | h :: t ->
    if String.lowercase_ascii h.prop_id = String.lowercase_ascii id
    then remove_prop id t
    else h :: (remove_prop id t)

(* Returns: given string list with string id removed
   Requires: input list is a list of ids *)
let remove_id id lst =
  let rec f = function
    | [] -> []
    | h :: t ->
      if String.lowercase_ascii id = String.lowercase_ascii h then f t
      else h :: f t in
  lst |> f |> List.rev

(* Returns: updated state after decreasing jail counter of current player
   Requires: current_player jail counter must be true and greater than 0 *)
let jail_update st p =
  let p' = {
    player_type = p.player_type;
    player_id = p.player_id;
    properties = p.properties;
    position = p.position;
    cash = p.cash;
    in_jail =
      if snd p.in_jail = 1 then (false, 0)
      else true, (snd p.in_jail) - 1;
  } in
  print_endline ("You have " ^ (p'.in_jail |> snd |> string_of_int) ^
                 " more turns in office hours.\n");
  let others = remove_player p.player_id st.players in
  {
    board = st.board;
    current_player = st.current_player;
    players = p' :: others;
    player_order = st.player_order;
    properties = st.properties;
    color_mult = st.color_mult;
  }

(* Returns: state after freeing player p from jail
   Requires: st is a valid state and p is a valid player *)
let free_jail st p =
  print_endline "You finished the assignment just in time for the due date.
You can leave office hours now.\n";
  let p' = {
    player_type = p.player_type;
    player_id = p.player_id;
    properties = p.properties;
    position = p.position;
    cash = p.cash;
    in_jail = false, 0;
  }
  in let others = remove_player p.player_id st.players in
  {
    board = st.board;
    current_player = st.current_player;
    players = p' :: others;
    player_order = st.player_order;
    properties = st.properties;
    color_mult = st.color_mult;
  }

(* Returns: state after moving current player n spaces
   Requires: n is an int and st is a valid state *)
let move st n =
  let p = get_player st st.current_player in
  let pos' = mod' (p.position + n) board_size in
  (* If player passes or lands on pass-go space, player gets $200 *)
  let cash' =
    if n > 0 && pos' <= p.position
    then (print_endline "You passed Go and collected $200."; p.cash + 200)
    else p.cash in
  let space = List.nth st.board.spaces pos' in
  (match space.space_type with
   | Property ->
     let prop = match space.property with None -> "" | Some x -> x in
     print_endline ("You landed on: " ^ prop ^ "\n")
   | Jail -> print_endline "You landed on: Office Hours\n"
   | Chest -> print_endline "You landed on: Community Chest\n"
   | Mystery -> print_endline "You landed on: Chance\n"
   | Go -> print_endline "You landed on: Go\n"
   | GoJail -> print_endline "You landed on: Go to Office Hours
\nYou failed another prelim? Time to study away in office hours.\n"
   | Free -> print_endline "You landed on: Free Parking
\nWow, lucky you! That doesn't happen much here at Cornell.\n");
  let p' = {
    player_type = p.player_type;
    player_id = p.player_id;
    properties = p.properties;
    position = pos';
    cash = cash';
    in_jail = p.in_jail;
  } in {
    board = st.board;
    current_player = st.current_player;
    players = p' :: (remove_player p.player_id st.players);
    player_order = st.player_order;
    properties = st.properties;
    color_mult = st.color_mult;
  }

(* Returns: state after adding building to given property
   Requires: p is an owned property *)
let add_building st prop player =
  let prop' = {
    prop_id = prop.prop_id;
    color = prop.color;
    price = prop.price;
    rent = prop.rent;
    owner = prop.owner;
    buildings = prop.buildings + 1;
    building_cost = prop.building_cost;
  } in
  print_endline ("\nYou built a "
                 ^ (if prop'.buildings = 5 then "hotel" else "house")
                 ^ " on " ^ prop'.prop_id ^ ".\n");
  let player' = {
    player_type = player.player_type;
    player_id = player.player_id;
    properties = player.properties;
    position = player.position;
    cash = player.cash - prop.building_cost;
    in_jail = player.in_jail;
  } in {
    board = st.board;
    current_player = st.current_player;
    players = player' :: (remove_player st.current_player st.players);
    player_order = st.player_order;
    properties = prop' :: (remove_prop prop.prop_id st.properties);
    color_mult = st.color_mult;
  }

(* Returns: state after building hotel on property with prop_id id
   Requires:
   - id is a valid prop_id
   - given prop must have exactly 4 houses already built
   - current player must be owner of the prop
   - current player must have enough money to build *)
let do_build st id =
  let prop = get_prop st id in
  let player = get_player st st.current_player in
  if prop.buildings >= 5
  then failwith "\nYou have already built four houses and a hotel on this property."
  else if st.current_player != match prop.owner with Some x -> x | None -> ""
  then failwith "\nYou don't own this property."
  else if player.cash < prop.building_cost
  then failwith "\nYou don't have enough money!"
  else add_building st prop player

(* Returns: true if all prop_ids in props are in lst
   Requires: lst is a list of valid prop_ids *)
let rec has_all_props lst props =
  match props with
  | [] -> true
  | h :: t -> if List.mem h lst then has_all_props lst t else false

(* Returns: 2 if the properties of color c are all owned by the same player
   and 1 otherwise
   Requires: st is a valid state and c is a valid color *)
let update_color st c =
  let props = prop_by_color c in
  let rec f (players : player list) lst =
    match players with
    | [] -> 1
    | h :: t -> if has_all_props h.properties lst then 2 else f t lst in
  f st.players props

(* Returns: state with updated color_mult function
   Requires: st is a valid state *)
let update_mult st =
  let f = fun c ->
    match c with
    | Red -> update_color st Red
    | Orange -> update_color st Orange
    | Yellow -> update_color st Yellow
    | Green -> update_color st Green
    | LBlue -> update_color st LBlue
    | DBlue -> update_color st DBlue
    | Pink -> update_color st Pink
    | Brown -> update_color st Brown in
  {
    board = st.board;
    current_player = st.current_player;
    players = st.players;
    player_order = st.player_order;
    properties = st.properties;
    color_mult = f;
  }

(* Returns: state after current_player buys prop from receiver for amt
   Requires:
   - current_player has at least amt in cash
   - receiver owns prop *)
let do_tradefor st receiver prop amt =
  let partner = get_player st receiver in
  let current = get_player st st.current_player in
  let p = get_prop st prop in
  if current.cash < amt then failwith ("You do not have enough money.\n")
  else if not (is_owner st partner p)
  then failwith (partner.player_id ^ " does not own this property.\n")
  else print_endline "Trade successful.\n";
  let current' = {
    player_type = current.player_type;
    player_id = current.player_id;
    properties = p.prop_id :: current.properties;
    position = current.position;
    cash = current.cash - amt;
    in_jail = current.in_jail;
  } in
  let partner' = {
    player_type = partner.player_type;
    player_id = partner.player_id;
    properties = remove_id p.prop_id partner.properties;
    position = partner.position;
    cash = partner.cash + amt;
    in_jail = partner.in_jail;
  } in
  let p' = {
    prop_id = p.prop_id;
    color = p.color;
    price = p.price;
    rent = p.rent;
    owner = Some current.player_id;
    buildings = p.buildings;
    building_cost = p.building_cost;
  } in
  let st' = {
    board = st.board;
    current_player = st.current_player;
    players = current' :: partner' :: (st.players
                                       |> remove_player receiver
                                       |> remove_player st.current_player);
    player_order = st.player_order;
    properties = p' :: (remove_prop prop st.properties);
    color_mult = st.color_mult;
  } in
  update_mult st'

  (* Returns: state after current_player trades away prop to receiver for amt
     Requires:
     - current_player has at least amt in cash
     - receiver owns prop *)
  let do_tradeaway st receiver prop amt =
    let r = get_player st receiver in
    let current = get_player st st.current_player in
    let p = get_prop st prop in
    if r.cash < amt then failwith (r.player_id ^ " does not have enough money.\n")
    else if not (is_owner st current p)
    then failwith ("You do not own this property.\n")
    else print_endline "Trade successful.\n";
    let current' = {
      player_type = current.player_type;
      player_id = current.player_id;
      properties = remove_id p.prop_id current.properties;
      position = current.position;
      cash = current.cash + amt;
      in_jail = current.in_jail;
    } in
    let r' = {
      player_type =r.player_type;
      player_id = r.player_id;
      properties = p.prop_id :: r.properties;
      position = r.position;
      cash = r.cash - amt;
      in_jail = r.in_jail;
    } in
    let p' = {
      prop_id = p.prop_id;
      color = p.color;
      price = p.price;
      rent = p.rent;
      owner = Some r.player_id;
      buildings = p.buildings;
      building_cost = p.building_cost;
    } in
    let st' = {
      board = st.board;
      current_player = st.current_player;
      players = current' :: r' :: (st.players
                                         |> remove_player receiver
                                         |> remove_player st.current_player);
      player_order = st.player_order;
      properties = p' :: (remove_prop prop st.properties);
      color_mult = st.color_mult;
    } in
    update_mult st'

  (* Returns: state after current_player trades away give to receiver for take
     Requires:
     - current_player owns give
     - receiver owns take *)
  let do_tradeprops st receiver give take =
    let r = get_player st receiver in
    let current = get_player st st.current_player in
    let g = get_prop st give in
    let t = get_prop st take in
    if not (is_owner st r t)
    then failwith (r.player_id ^ " is does not own " ^ t.prop_id ^ ".\n")
    else if not (is_owner st current g)
    then failwith ("You do not own " ^ g.prop_id ^ ".\n")
    else print_endline "Trade successful.\n";
    let current' = {
      player_type = current.player_type;
      player_id = current.player_id;
      properties = t.prop_id :: (remove_id g.prop_id current.properties);
      position = current.position;
      cash = current.cash;
      in_jail = current.in_jail;
    } in
    let r' = {
      player_type =r.player_type;
      player_id = r.player_id;
      properties = g.prop_id :: (remove_id t.prop_id r.properties);
      position = r.position;
      cash = r.cash;
      in_jail = r.in_jail;
    } in
    let g' = {
      prop_id = g.prop_id;
      color = g.color;
      price = g.price;
      rent = g.rent;
      owner = Some r.player_id;
      buildings = g.buildings;
      building_cost = g.building_cost;
    } in
    let t' = {
      prop_id = t.prop_id;
      color = t.color;
      price = t.price;
      rent = t.rent;
      owner = Some current.player_id;
      buildings = t.buildings;
      building_cost = t.building_cost;
    } in
    let st' = {
      board = st.board;
      current_player = st.current_player;
      players = current' :: r' :: (st.players
                                         |> remove_player receiver
                                         |> remove_player st.current_player);
      player_order = st.player_order;
      properties = g' :: t' ::
                   (remove_prop take (remove_prop give st.properties));
      color_mult = st.color_mult;
    } in
    update_mult st'

(* Returns: state after handling preroll command
   Requires: st is valid state and c is a preroll_command *)
let rec do_preroll st c =
  let p = get_player st st.current_player in
  match c with
  | Roll -> do_roll st p
  | Build prop -> do_build st prop
  | TradeForProp (receiver, prop, amt) -> do_tradefor st receiver prop amt
  | TradeAwayProp (receiver, prop, amt) -> do_tradeaway st receiver prop amt
  | TradeProps (receiver, give, take) -> do_tradeprops st receiver give take

(* Returns: state after handling roll and the subsequent move
   Requries: st is a valid state and p is the current player
   If player is in jail and rolls a double, player goes back to Roll. *)
and do_roll st p =
  let die1 = (Random.int 6) + 1 in
  let die2 = (Random.int 6) + 1 in
  print_endline ("\nYou rolled a " ^ string_of_int die1 ^ " and a "
                 ^ string_of_int die2 ^ ".");
  if fst p.in_jail && die1 != die2 then jail_update st p
  else if fst p.in_jail
  then (print_endline "That's a double!\n";
        do_preroll (free_jail st p) Roll)
  else move st (die1 + die2)

(* Returns: state after choosing random mystery card and moving that
   number of spaces
   Requires: st is a valid state *)
let do_mystery st =
  let n = (st.board.mystery_cards |> List.length |> Random.int)
          |> List.nth st.board.mystery_cards in
  print_endline ("You drew a Chance Card: Move "
                 ^ (if n < 0 then "backwards " ^ string_of_int (-1*n)
                    else "forwards " ^ string_of_int n)
                 ^ " spaces\n");
  move st n

(* Returns: list of players with cash for each modified by amt
   Requires: input list is a list of players *)
let rec chest_all amt = function
  | [] -> []
  | h :: t ->
    let cash' =
      if h.cash + amt <= 0
      then (print_endline (h.player_id ^ " is now bankrupt.\n"); 0)
      else h.cash + amt in
    {
      player_type = h.player_type;
      player_id = h.player_id;
      properties = h.properties;
      position = h.position;
      cash = cash';
      in_jail = h.in_jail;
    } :: chest_all amt t

(* Returns: state after applying random community chest card
   Requires: st is a valid state *)
let do_chest st =
  let card = (st.board.chest_cards |> List.length |> Random.int)
             |> List.nth st.board.chest_cards in
  let players' = match card with
    | n, `All ->
      print_endline ("You drew a Community Chest card: All players "
                     ^ (if n < 0 then "lose $" ^ (string_of_int (-1*n))
                        else "get $" ^ string_of_int n ^ ".\n"));
      chest_all n st.players
    | n, `Current ->
      print_endline ("You drew a Community Chest card: You "
                     ^ (if n < 0 then "lose $" ^ (string_of_int (-1*n))
                        else "get $" ^ string_of_int n ^ ".\n"));
      ([st.current_player |> get_player st]
       |> chest_all n
       |> List.hd) :: (remove_player st.current_player st.players) in
  {
    board = st.board;
    current_player = st.current_player;
    players = players';
    player_order = st.player_order;
    properties = st.properties;
    color_mult = st.color_mult;
  }

(* Returns: state after player p pays rent to the owner of property with id prop
   Requires: prop is owned by someone *)
let do_payrent st p prop =
  let prop' = get_prop st prop in
  let owner = get_player st (match prop'.owner with None -> "" | Some x -> x) in
  let amt = (List.nth prop'.rent (prop'.buildings))
            * (st.color_mult prop'.color) in
  let amt' =
    if p.cash <= amt
    then ((print_endline ("You paid $" ^ string_of_int p.cash ^ " in rent to "
                          ^ owner.player_id ^ ".\nYou are now bankrupt.\n"));
          p.cash)
    else ((print_endline ("You paid $" ^ string_of_int amt ^ " in rent to "
                          ^ owner.player_id ^ ".\n"));
          amt) in
  let p' = {
    player_type = p.player_type;
    player_id = p.player_id;
    properties = p.properties;
    position = p.position;
    cash = p.cash - amt';
    in_jail = p.in_jail;
  } in
  let owner' = {
    player_type = owner.player_type;
    player_id = owner.player_id;
    properties = owner.properties;
    position = owner.position;
    cash = owner.cash + amt';
    in_jail = owner.in_jail;
  } in {
    board = st.board;
    current_player = st.current_player;
    players = p' :: owner' :: (st.players
                               |> remove_player p.player_id
                               |> remove_player owner.player_id);
    player_order = st.player_order;
    properties = st.properties;
    color_mult = st.color_mult;
  }

(* Returns: state after sending current player to jail
   Requires: st is a valid state *)
let do_gojail st =
  print_endline "You have been put in jail. You will be let out in 3 turns.\n";
  let p = get_player st st.current_player in
  let p' = {
    player_type = p.player_type;
    player_id = p.player_id;
    properties = p.properties;
    position = jail_space;
    cash = p.cash;
    in_jail = true, 3;
  } in
  {
    board = st.board;
    current_player = st.current_player;
    players = p' :: (remove_player p.player_id st.players);
    player_order = st.player_order;
    properties = st.properties;
    color_mult = st.color_mult;
  }

(* Returns: state after current player buys prop
   Requires: prop is not already owned and current player has enough money *)
let do_buy st prop =
  let p = get_prop st prop in
  let player = get_player st st.current_player in
  if p.owner != None then failwith "\nThis property already has an owner."
  else if player.cash < p.price then failwith "\nYou don't have enough money!"
  else
    print_endline "\nPurchase successful.\n";
  let p' = {
    prop_id = p.prop_id;
    color = p.color;
    price = p.price;
    rent = p.rent;
    owner = Some st.current_player;
    buildings = p.buildings;
    building_cost = p.building_cost;
  } in
  let player' = {
    player_type = player.player_type;
    player_id = player.player_id;
    properties = p.prop_id :: player.properties;
    position = player.position;
    cash = player.cash - p.price;
    in_jail = player.in_jail;
  } in
  let st' = {
    board = st.board;
    current_player = st.current_player;
    players = player' :: (remove_player st.current_player st.players);
    player_order = st.player_order;
    properties = p' :: (remove_prop prop st.properties);
    color_mult = st.color_mult;
  } in
  update_mult st'

(* Returns: state after switching to next player
   Requires: st is a valid state *)
let do_finish st =
  let rec f = function
    | [] -> failwith "\nPlayer not found."
    | h :: t ->
      if String.lowercase_ascii h = String.lowercase_ascii st.current_player
      then 0
      else 1 + (f t) in
  let i = f st.player_order in
  let p = List.nth st.player_order ((i+1) mod (List.length st.player_order)) in
  {
    board = st.board;
    current_player = p;
    players = st.players;
    player_order = st.player_order;
    properties = st.properties;
    color_mult = st.color_mult;
  }

(* Returns: state after handling postroll command
   Requires: st is valid state and c is a postroll_command *)
let do_postroll st c =
  let p = get_player st st.current_player in
  match c with
  | MysteryMove -> do_mystery st
  | CommChest -> do_chest st
  | PayRent prop -> do_payrent st p prop
  | GoJail -> do_gojail st
  | Buy prop -> do_buy st prop
  | FinishTurn -> do_finish st

(* Returns: list of properties modified so that they have no owner
   Requries: lst is a list of prop_ids that are owned by the same player *)
let rec reset_props st lst =
  match lst with
  | [] -> []
  | h :: t ->
    let p = get_prop st h in
    {
      prop_id = p.prop_id;
      color = p.color;
      price = p.price;
      rent = p.rent;
      owner = None;
      buildings = 0;
      building_cost = p.building_cost;
    } :: reset_props st t

(* Returns: version of acc where all properties in lst have been removed
   Requires:
   - acc is a list of properties
   - lst is a list of prop_ids of properties to be removed *)
let rec remove_props acc lst =
  match lst with
  | [] -> acc
  | h :: t -> remove_props (remove_prop h acc) t

(* Returns: state after player p accepts bankrupcy
   Player p is removed from the players list, and their properties become
   unowned. The properties also lose any buildings on it.
   Requires: player with id p is bankrupt and st is a valid state *)
let do_accept st p =
  let p_props = (get_player st p).properties in
  let players = remove_player p st.players in
  let resetted = reset_props st p_props in
  print_endline "You are out of the game.\n";
  let props = resetted @ (remove_props st.properties p_props) in
  let st' = {
    board = st.board;
    current_player = st.current_player;
    players = players;
    player_order = remove_id st.current_player st.player_order;
    properties = props;
    color_mult = st.color_mult;
  } in
update_mult st'

(* Returns: state after handling bankrupt command
   Requires: st is valid state and c is a bankrupt_command *)
let do_bankrupt st c =
  match c with
  | Accept p -> do_accept st p
  | Fight (TradeAwayProp (r, prop, amt)) -> do_tradeaway st r prop amt
  | _ -> failwith "\nInvalid Fight command."

(* ========================== ACCESSIBLE BY MAIN ========================== *)

let init_state names =
  Random.self_init ();
  let players_list = create_players names in
  {
    board = board1;
    current_player = names |> List.hd |> fst;
    players = players_list;
    player_order = get_ids players_list;
    properties = all_properties;
    color_mult =
      fun c ->
        match c with
        | Red -> 1
        | Orange -> 1
        | Yellow -> 1
        | Green -> 1
        | LBlue -> 1
        | DBlue -> 1
        | Pink -> 1
        | Brown -> 1
  }

let owns_prop st player prop =
  let player' = get_player st player in
  List.mem prop player'.properties

let has_owner st prop =
  let prop' = get_prop st prop in
  prop'.owner != None

let num_building st prop =
  let prop' = get_prop st prop in
  prop'.buildings

let is_in_jail st player =
  let player' = get_player st player in
  match player'.in_jail with
  | true, _ -> true
  | false, _ -> false

let get_bankrupt st =
  let rec f = function
    | [] -> []
    | h :: t -> if h.cash <= 0 then h.player_id :: f t else f t in
  f st.players

let prop_of_space i =
  match (List.nth board1.spaces i).property with
  | None -> failwith "\nNot a property space."
  | Some x -> x

let get_space_type st p =
  let p' = get_player st p in
  let pos = p'.position in
  (List.nth st.board.spaces pos).space_type

let get_space st p =
  let p' = get_player st p in
  let pos = p'.position in
  List.nth st.board.spaces pos

let check_win st =
  if List.length st.players > 1 then None
  else Some (List.hd st.players).player_id

let is_in_game st player =
  List.exists (fun x -> x.player_id = player) st.players

let do' st c =
  match c with
  | Preroll c' -> do_preroll st c'
  | Postroll c' -> do_postroll st c'
  | Bankrupt c' -> do_bankrupt st c'
