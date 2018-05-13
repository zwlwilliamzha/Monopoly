type preroll_command =
  | Roll
  | Build of string (* property_id *)
  | TradeForProp of string * string * int
  (* Offer recipient player_id, prop_id to receive, amt to give *)
  | TradeAwayProp of string * string * int
  (* Offer recipient player_id, prop_id to give, amt to receive *)
  | TradeProps of string * string * string
  (* Offer recipient player_id, prop_id to give, prop_id to receive*)

type postroll_command =
  | FinishTurn
  | MysteryMove
  | CommChest
  | PayRent of string (* property_id *)
  | GoJail
  | Buy of string (* property_id *)

type bankrupt_command =
  | Accept of string (* player_id *)
  | Fight of preroll_command (* TradeAwayProp, player must be seller *)

type command =
  | Preroll of preroll_command
  | Postroll of postroll_command
  | Bankrupt of bankrupt_command
