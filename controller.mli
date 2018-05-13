(* AI controller decisions will decided through these functions *)
module type AIController = sig

  (* can result in roll, offer trade, or build command *)
  val preroll : State.state -> string

  (* Deciding whether to buy property or not *)
  val buy : State.state -> State.property -> string

  val accept_decline : Command.command -> State.state -> string

  val bankrupt_decide : State.state -> string

  val postroll : State.state -> string

end
