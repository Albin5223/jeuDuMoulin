open Mill.Arena
open Mill.Engine

let randomSeed n = Random.int n

let wrap s player =
    let print () =
        Printf.eprintf "\r                    ";
        Printf.eprintf "\r%s%!" s
    in
    ( s,
      {
        strategie_play =
          (fun g p ->
            print ();
            player.strategie_play g p);
        strategie_remove =
          (fun g p ->
            print ();
            player.strategie_remove g p);
      } )

let players =
    [|
      wrap "Mill.random" (player_random randomSeed);
      wrap "Soan" Mill.Soan.bot_SOAN;
      wrap "Lecomte" Mill.Lecomte.player_georges_lecomte;
      wrap "Martinez" Mill.Martinez.alec_bot;
      wrap "Abdorohmang" Mill.Abdorohmang.my_bot;
      wrap "Crague" Mill.Crague.my_player;
      wrap "Friedmann" Mill.Friedmann.smart_player;
      wrap "Joffrin" Mill.Joffrin.bot_evan;
      wrap "Debessel" Mill.Debessel.player_ia;
      wrap "Arrous" Mill.Arrous.player_classic_bot_Thomas_Arrous;
      wrap "Eljamal" Mill.Eljamal.player_ali;
      wrap "Guetteville" Mill.Guetteville.player_optimal_Nathan;
      wrap "Morel" Mill.Morel.ai_strategie;
      wrap "Diallo" Mill.Diallo.player_amd;
    |]

let games = Array.init (Array.length players) (fun _ -> 0)

let durations = Array.init (Array.length players) (fun _ -> 0.)

let elo = Array.init (Array.length players) (fun _ -> 300)

let update_elo rank1 rank2 score1 score2 =
    assert (List.mem score1 [0.; 0.5; 1.]);
    assert (score2 = 1. -. score1);
    (* https://metinmediamath.wordpress.com/2013/11/27/how-to-calculate-the-elo-rating-including-example/ *)
    let rank1 = float_of_int rank1 in
    let rank2 = float_of_int rank2 in
    let transform rank = 10. ** (rank /. 400.) in
    let r1 = transform rank1 in
    let r2 = transform rank2 in
    let expectation ri rj = ri /. (ri +. rj) in
    let e1 = expectation r1 r2 in
    let e2 = expectation r2 r1 in
    let new_rating rank score e = Int.max (int_of_float (Float.round (rank +. (10. *. (score -. e))))) 0 in
    (new_rating rank1 score1 e1, new_rating rank2 score2 e2)

(*
let _ =
    assert (update_elo 2400 2000 1. 0. = (2403, 1997));
    assert (update_elo 2400 2000 0. 1. = (2371, 2029))
*)

let update p1 score1 p2 score2 =
    let rank1 = elo.(p1) in
    let rank2 = elo.(p2) in
    let r1, r2 = update_elo rank1 rank2 score1 score2 in
    elo.(p1) <- r1;
    elo.(p2) <- r2;
    games.(p1) <- games.(p1) + 1;
    games.(p2) <- games.(p2) + 1

let () =
    Random.self_init ();
    players |> Array.iter (fun (p, _) -> Printf.printf "\"%s\";" p);
    Printf.printf "\n%!";

    (*
       Goal: any pair of players must engage in ~50 games as player 1
       vs. player 2 and ~50 games as player 2 vs. player 1.

       This is approximately [100 * Array.length players *
       (Array.length players - 1)] random games.

    *)
    let max_games = 100 * Array.length players * (Array.length players - 1) in
    for k = 1 to max_games do
      let i = Random.int (Array.length players) in

      let j =
          let j = Random.int (Array.length players - 1) in
          if j >= i then j + 1 else j
      in
      let pi, player_i = players.(i) in
      let pj, player_j = players.(j) in
      let score_i, score_j =
          try
            let t = Sys.time () in
            let result = arena player_i player_j Nine_mens_morris in
            let t' = Sys.time () in
            durations.(i) <- durations.(i) +. (t' -. t);
            durations.(j) <- durations.(j) +. (t' -. t);
            match result.winner.color with
            | White -> (1., 0.)
            | Black -> (0., 1.)
          with _ -> (* Draw: no-one wins *) (0.5, 0.5)
      in
      Printf.eprintf "\n%.2f%%: %s (%d) vs. %s (%d): %.1f / %.1f\n"
        (float_of_int k *. 100. /. float_of_int max_games)
        pi elo.(i) pj elo.(j) score_i score_j;
      update i score_i j score_j;
      elo |> Array.iter (fun score -> Printf.printf "%d;" score);
      Printf.printf "\n%!"
    done;
    Printf.eprintf "\n"
