open State
open Command

(*Requires that p is a valid player and cost is an int
  Returns true if player money is at least 1.5 times the cost*)
let enough_money (p:player) cost =
  if p.cash >= (cost * 3 / 2) then
    true else false

let compare_value p_1 p_2 =
  let p1 = float_of_int p_1.price in
  let p2 = float_of_int p_2.price in
      p1 /. p2

(*Finds the first property to build on*)
let rec can_build prop_lst =
  match prop_lst with
  |[] -> None
  |h::t -> begin
    let num_prop = same_color prop_lst h.color 0 in
    if num_prop = 3 then
      if h.buildings < 4 then Some h
      else if h.buildings = 4 then Some h
      else can_build t
    else can_build t
  end

(*Decides if the player has enough money to build on the prop from can_buuild*)
let to_build st =
  let p = get_player st st.current_player in
  let prop_lst = get_player_prop p st in
  let prop = can_build prop_lst in
  match prop with
  |None -> None
  |Some x -> begin
      let price = x.price in
      if enough_money p price then
        prop
      else None
    end

(*Finds the players first closest monopoly*)
let rec close_monopoly prop_lst =
  match prop_lst with
  |[] -> None
  |h::t -> begin
      let num_prop = same_color prop_lst h.color 0 in
      if num_prop = 2 then Some h
      else close_monopoly t
    end

(*Finds the player with the missing property, None is still in play*)
let rec find_missing_prop st p_lst colr =
  match p_lst with
  |[] -> None
  |h::t -> begin
      let prop_lst = get_player_prop h st in
      let num_prop = same_color prop_lst colr 0 in
      if num_prop = 1 then Some h
      else find_missing_prop st t colr
    end

(*Finds a the prop missing for monopoly*)
let rec find_missing_colr colr p_lst =
  match p_lst with
  |[] -> None
  |h::t -> begin
      if h.color = colr then Some h else find_missing_colr colr t
    end

(*Creates the trade between ai and player*)
let find_trade st =
let p = get_player st st.current_player in
let prop_lst = get_player_prop p st in
let clos = close_monopoly prop_lst in
  match clos with
  |None -> None
  |Some prop -> begin
      let p_to_trade = find_missing_prop st st.players prop.color in
      match p_to_trade with
      |None -> None
      |Some play -> begin
          let play_props = get_player_prop play st in
          let needed = close_monopoly play_props in
          let missing = find_missing_colr prop.color play_props in
          match needed with
          |None -> begin
              match missing with
              |None -> None
              |Some miss -> begin
                  let amt = (float_of_int miss.price) *. 1.25
                            |> int_of_float in
                  if enough_money p amt then
                    Some ("TradeForProp " ^ play.player_id
                          ^ miss.prop_id ^ string_of_int amt)
                  else None
                end
            end
          |Some need -> begin
              let ai_has = find_missing_prop st [p] need.color in
              match ai_has with
              |None -> begin
                  match missing with
                  |None -> None
                  |Some miss -> begin
                      let amt = (float_of_int miss.price) *. 1.25
                                |> int_of_float in
                      if enough_money p amt then
                        Some ("TradeForProp " ^ play.player_id
                              ^ miss.prop_id ^ string_of_int amt)
                      else None
                    end
                end
              |Some ai -> begin
                  let play_prop = find_missing_colr need.color prop_lst in
                  match play_prop with
                  |None -> None
                  |Some p_prop -> begin
                      let ai_needs = find_missing_colr prop.color play_props in
                      match ai_needs with
                      |None -> None
                      |Some ai_p ->
                        Some ("TradeProps " ^ play.player_id
                              ^ p_prop.prop_id ^ ai_p.prop_id)
                    end
                end
            end
        end
    end

let preroll st =
  let p = get_player st st.current_player in
  let prop_lst = get_player_prop p st in
  match prop_lst with
  |[] -> "Roll"
  |h::t -> begin
      let bld = to_build st in
      match bld with
      |None -> begin
          let trd = find_trade st in
          match trd with
          |None -> "Roll"
          |Some c -> c
          end
      |Some x -> "Build " ^ x.prop_id
    end

(*let accept_decline offer st =
  match offer with
  |Trade (ai, seller, prop_id, money) -> begin
      let ai_play = get_player st ai in
      let ai_prop = get_player_prop ai_play st in
      let need = close_monopoly ai_prop in
      match need with
      |None -> begin
          if money = 0 then "No"
          else
        end
    end
  |_ -> failwith "Not a trade offer"*)

let rec find_single prop_lst =
  match prop_lst with
  |[] -> None
  |h::t -> begin
      let num_prop = same_color prop_lst h.color 0 in
      if num_prop = 1 then Some h else find_single t
    end

let rec can_afford p_lst price =
  match p_lst with
  |[] -> None
  |h::t -> if h.cash >= price then Some h else can_afford t price

let bankrupt_decide st =
  let p = get_player st st.current_player in
  let prop_lst = get_player_prop p st in
  match prop_lst with
  |[] -> "Accept"
  |h::t -> begin
    let sell = find_single prop_lst in
    match sell with
    |None -> begin
        let tradable = close_monopoly prop_lst in
        match tradable with
        |None -> begin
              let request = h.price + h.buildings * h.building_cost in
              let reciever = can_afford st.players request in
              match reciever with
              |None -> begin
                  let req_2 = int_of_float (float_of_int request *. 0.75) in
                  let rec_2 = can_afford st.players req_2 in
                  match rec_2 with
                  |None -> "Accept"
                  |Some one -> "TradeAwayProp" ^ one.player_id ^
                               h.prop_id ^ (string_of_int req_2)
                end
              |Some play -> "TradeAwayProp" ^ play.player_id ^
                            h.prop_id ^ (string_of_int request)
            end
        |Some prop -> begin
          let requ = prop.price + prop.buildings * prop.building_cost in
          let reci = can_afford st.players requ in
          match reci with
          |None -> begin
              let requ_2 = int_of_float (float_of_int requ *. 0.75) in
              let reci_2 = can_afford st.players requ_2 in
              match reci_2 with
              |None -> "Accept"
              |Some on -> "TradeAwayProp" ^ on.player_id ^
                           h.prop_id ^ (string_of_int requ_2)
            end
          |Some person ->  "TradeAwayProp" ^ person.player_id ^
                        h.prop_id ^ (string_of_int requ)
          end
        end
    |Some pr -> begin
        let reque = pr.price + pr.buildings * pr.building_cost in
        let recip = can_afford st.players reque in
        match recip with
        |None -> begin
            let reque_2 = int_of_float (float_of_int reque *. 0.75) in
            let recip_2 = can_afford st.players reque_2 in
            match recip_2 with
            |None -> "Accept"
            |Some on -> "TradeAwayProp" ^ on.player_id ^
                        h.prop_id ^ (string_of_int reque_2)
          end
        |Some person ->  "TradeAwayProp" ^ person.player_id ^
                         h.prop_id ^ (string_of_int reque)
      end
    end


let buy st prop =
  let p = get_player st st.current_player in
  let prop' = get_prop st prop in
  if enough_money p prop'.price then "yes"
  else "no"

let postroll st =
  let space = get_space st st.current_player in
  match space.property with
  |None -> ""
  |Some x -> buy st x
