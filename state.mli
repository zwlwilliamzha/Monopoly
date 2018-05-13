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
and player = {
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
  players: player list; (* in order *)
  player_order: string list;
  properties: property list;
  color_mult: prop_color -> int;
}

(* Game board *)
val board1 : board
val board_size : int
val jail_space : int

(* Returns: initial st of game given list of players
   Requires: players is a list of (player_id, player_type) with length 2-4 *)
val init_state : (string * player_type) list -> state

(* Returns: [owns_prop st player_id prop_id] is true if the player with id
   [player_id] owns the property with id [prop_id] and false otherwise
   Requires: player_id and prop_id exist in st *)
val owns_prop : state -> string -> string -> bool

(* Returns: [has_owner st prop_id] is true if the property with prop_id
   has an owner and false otherwise
   Requires: prop_id is a valid prop_id *)
val has_owner : state -> string -> bool

(* Returns: [num_building st prop_id] is the number of buildings currently
   on the property with id prop_id
   Requires: prop_id is a valid prop_id *)
val num_building : state -> string -> int

(* Returns: [is_in_jail st player_id] is true if the player with id player_id
   is in jail, and false otherwise
   Requires: player_id is a valid player_id *)
val is_in_jail : state -> string -> bool

(* Returns: [get_bankrupt st] is the list of player_ids of players that are
   currently going bankrupt. Players in this list should be given the
   accept/fight choice before they are removed from the game.
   Requires: st is the state after a turn *)
val get_bankrupt : state -> string list

(* Returns: player with given player_id
   Requires: id is a player_id found in state *)
val get_player : state -> string -> player

(* Returns: property with given prop_id
   Requires: id is a prop_id found in state *)
val get_prop : state -> string -> property

(*Returns: players property list
  Requires: p is a vaild player, st is a vaild state*)
val get_player_prop : player -> state -> property list

(*Returns number of properties with the same color in the list
  Requires lst is a list of properties and colr is a vaild property color*)
val same_color : property list -> prop_color -> int -> int

(* Returns: [prop_of_space i] is the property of the space with index i
   Requires: space i is a property space *)
val prop_of_space : int -> string

(* Returns: [get_space_type st player_id] is the space_type that the player
   with id player_id is currently on
   Requires: player_id is a valid and found in st *)
val get_space_type : state -> string -> space_type

(* Returns: [get_space st player_id] is the space that the player
   with id player_id is currently on
   Requires: player_id is a valid and found in st *)
val get_space : state -> string -> space

(* Returns: [check_win st] is Some player_id if that player is the winner and
   None if there is no winner yet
   Requires: st is a valid state *)
val check_win : state -> string option

(* Returns: [is_in_game st player_id] is true if the player with id player_id
   is still in the game and false otherwise
   Requires: player_id is a valid player_id *)
val is_in_game : state -> string -> bool

(* Returns: state after carrying out command c
   Requires: st is a valid state *)
val do' : state -> Command.command -> state
