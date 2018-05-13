open OUnit2
open State
open Command

let s0 = init_state [("p1", Human); ("p2", Human)]
let s1 = do' s0 (Postroll (Buy "Boardwalk"))
let s2 = do' s1 (Postroll (Buy "STates aVenue"))
let s3 = do' s2 (Postroll GoJail)
let s4 = do' s3 (Preroll Roll)
let s5 = do' s4 (Postroll FinishTurn)
let s6 = do' s5 (Postroll (Buy "kentucky avenue"))
let s7 = do' s6 (Postroll (PayRent "states avenue"))
let s8 = do' s7 (Preroll (BuildHouse "kentucky avenue"))
let s9 = do' s8 (Postroll FinishTurn)
let s10 = do' s9 (Postroll (PayRent "kentucky avenue"))
let s11 = do' s10 (Preroll (Trade ("p1", "p2", "kentucky avenue", 100)))
let s12 = do' s11 (Bankrupt (Accept "p1"))
let s13 = do' s10 (Bankrupt (Fight ((Trade ("p1", "p2", "kentucky avenue", 100)))))

let tests =
  [
    (* Init state *)
    "s0" >:: (fun _ -> assert_equal "p1" (s0.current_player));
    "s0" >:: (fun _ -> assert_equal 2 (List.length s0.players));
    "s0" >:: (fun _ -> assert_equal ["p1"; "p2"] (s0.player_order));
    (* Buy property *)
    "s1" >:: (fun _ -> assert_equal ["Boardwalk"] (get_player s1 "p1").properties);
    "s1" >:: (fun _ -> assert_equal (Some "p1") (get_prop s1 "Boardwalk").owner);
    "s1" >:: (fun _ -> assert_equal 600 (get_player s1 "p1").cash);
    "s2" >:: (fun _ -> assert_equal true (List.mem "Boardwalk" (get_player s2 "p1").properties));
    "s2" >:: (fun _ -> assert_equal true (List.mem "States Avenue" (get_player s2 "p1").properties));
    "s2" >:: (fun _ -> assert_equal (Some "p1") (get_prop s2 "States Avenue").owner);
    "s2" >:: (fun _ -> assert_equal 2 (List.length s2.players));
    "s1" >:: (fun _ -> assert_equal 200 (get_player s2 "p1").cash);
    (* Go to jail *)
    "s3" >:: (fun _ -> assert_equal (true, 3) (get_player s3 "p1").in_jail);
    "s3" >:: (fun _ -> assert_equal jail_space (get_player s3 "p1").position);
    (* Rolling in jail *)
    "s4" >:: (fun _ -> assert_equal (true, 2) (get_player s4 "p1").in_jail);
    (* Finish turn *)
    "s5" >:: (fun _ -> assert_equal "p2" s5.current_player);
    (* Buy property for different player *)
    "s6" >:: (fun _ -> assert_equal ["Kentucky Avenue"] (get_player s6 "p2").properties);
    "s6" >:: (fun _ -> assert_equal (Some "p2") (get_prop s6 "kentucky avenue").owner);
    "s6" >:: (fun _ -> assert_equal 600 (get_player s6 "p2").cash);
    (* Pay rent *)
    "s7" >:: (fun _ -> assert_equal 590 (get_player s7 "p2").cash);
    "s7" >:: (fun _ -> assert_equal 210 (get_player s7 "p1").cash);
    (* Build house *)
    "s8" >:: (fun _ -> assert_equal 440 (get_player s8 "p2").cash);
    "s8" >:: (fun _ -> assert_equal 1 (get_prop s8 "kentucky avenue").buildings);
    (* Finish turn *)
    "s9" >:: (fun _ -> assert_equal "p1" s9.current_player);
    (* Pay rent on property with house *)
    "s10" >:: (fun _ -> assert_equal 530 (get_player s10 "p2").cash);
    "s10" >:: (fun _ -> assert_equal 120 (get_player s10 "p1").cash);
    (* Trade away property with a house on it *)
    "s11" >:: (fun _ -> assert_equal 630 (get_player s11 "p2").cash);
    "s11" >:: (fun _ -> assert_equal 20 (get_player s11 "p1").cash);
    "s11" >:: (fun _ -> assert_equal [] (get_player s11 "p2").properties);
    "s11" >:: (fun _ -> assert_equal true (List.mem "Kentucky Avenue" (get_player s11 "p1").properties));
    (* Player with properties accept bankruptcy *)
    "s12" >:: (fun _ -> assert_equal 1 (List.length s12.players));
    "s12" >:: (fun _ -> assert_equal None (get_prop s12 "kentucky avenue").owner);
    "s12" >:: (fun _ -> assert_equal None (get_prop s12 "boardwalk").owner);
    "s12" >:: (fun _ -> assert_equal None (get_prop s12 "states avenue").owner);
    (* Fight bankruptcy *)
    "s13" >:: (fun _ -> assert_equal 630 (get_player s13 "p2").cash);
    "s13" >:: (fun _ -> assert_equal 20 (get_player s13 "p1").cash);
    "s13" >:: (fun _ -> assert_equal [] (get_player s13 "p2").properties);
    "s13" >:: (fun _ -> assert_equal true (List.mem "Kentucky Avenue" (get_player s13 "p1").properties));
  ]

let _ = run_test_tt_main ("suite" >::: tests)
