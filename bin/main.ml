open Mill.Arena
open Mill.Engine

let randomSeed n = Random.int n

let wrap s player =
  let print () = Printf.eprintf "\r                    "; Printf.eprintf "\r%s%!" s in
  (s, fun seed ->
      let player = player seed in
      { strategie_play = (fun g p -> print (); player.strategie_play g p);
        strategie_remove = (fun g p -> print (); player.strategie_remove g p) })

let players = [| wrap "Mill.random" player_random;
                 wrap "Mill.random2" player_random2;|]

let score = Array.init (Array.length players) (fun _ -> 0)

let () =
  Random.self_init ();
  Format.open_vbox 0;
  for i = 0 to Array.length players - 1 do
    let (pi, player_i) = players.(i) in
    for j = i + 1 to Array.length players - 1 do
      let (pj, player_j) = players.(j) in
      let max_games = 500 in
      let wins_i = ref 0 in
      let wins_j = ref 0 in
      let t = Sys.time () in
      begin
      try
        for k = 0 to max_games - 1 do
          if Sys.time () -. t >= float_of_int (k + 1)  then
            begin
              failwith "Taking too long"
            end;
          begin try
              let result =
                arena (player_i randomSeed) (player_j randomSeed) Nine_mens_morris in
              match result.winner.color with
              | Black -> incr wins_i
              | White -> incr wins_j
            with
            | _ -> (* Draw: no-one wins *) ()
          end;
          begin try
              let result =
                arena (player_j randomSeed) (player_i randomSeed) Nine_mens_morris in
              match result.winner.color with
              | Black -> incr wins_j
              | White -> incr wins_i
            with _ -> (* Draw: no-one wins *)  ()
          end;
        done;
      with
      | _ ->
        begin
          (* Time-out: produce awkward value *)
          wins_i := -1;
          wins_j := -1
        end;
    end;
      let wins_i_ratio = (!wins_i * 100) / (2 * max_games) in
      let wins_j_ratio = (!wins_j * 100) / (2 * max_games) in
      Format.printf "@[<h>%.1f@;<4 4>%s@;<4 4>%d@;<4 4>%s@;<4 4>%d@]@;" ((Sys.time () -. t) *. (1000. /. (2. *. float_of_int max_games))) pi wins_i_ratio pj wins_j_ratio ;
      if wins_i > wins_j then
        score.(i) <- score.(i) + 1
      else if wins_j > wins_i then
        score.(j) <- score.(j) + 1
    done
  done;
  Format.close_box ();
  Format.open_vbox 0;
  score |> Array.iteri (fun i score ->
      Format.printf "@[<h>%s@;<4 4>%d@]@;" (fst players.(i)) score);
  Format.printf "@.";
  Printf.eprintf "\n"
