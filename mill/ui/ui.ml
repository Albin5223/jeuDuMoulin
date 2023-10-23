module Html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Js = Js_of_ocaml.Js

let js = Js.string
let doc = Html.document
let cell_id i j = js (Printf.sprintf "t%d-%d" i j)

let cell_elt i j =
  Js.Opt.get (doc##getElementById (cell_id i j)) (fun () -> assert false)


let draw_board player_source =
  let open Tictactoe in
  (* Table *)
  let table = Html.createTable doc in
  table##.className := js "board";
  Dom.appendChild doc##.body table;
  (* Rows and columns *)
  for i = 0 to 2 do
    let row = Html.createTr doc in
    Dom.appendChild table row;
    for j = 0 to 2 do
      let col = Html.createTd doc in
      col##.id := cell_id i j;
      Lwt.ignore_result
        (let open Lwt.Syntax in
         let* e = Js_of_ocaml_lwt.Lwt_js_events.click col in
         let e = Js.Opt.get e##.srcElement (fun () -> assert false) in
         e##.className := js "red";
         Scanf.sscanf
           (Js.to_string e##.id)
           "t%d-%d"
           (fun i j ->
             Lwt_mvar.put player_source (Engine.Pos.h i, Engine.Pos.v j)));
      Dom.appendChild row col
    done
  done


let display player p board =
  let open Tictactoe in
  board
  |> Engine.iteri (fun h v -> function
       | None -> ()
       | Some p ->
           let color = match p with Engine.X -> "red" | Engine.O -> "green" in
           let t = cell_elt (Engine.int_of_hpos h) (Engine.int_of_vpos v) in
           t##.className := js color);
  player p board


let player_ui =
  let move = Lwt_mvar.create_empty () in
  ( move,
    fun _ ->
      let open Lwt.Syntax in
      let* r = Lwt_mvar.take move in
      Lwt.return (Some r) )


let onload _ =
  let open Tictactoe.Arena in
  let player_source, player_ui = player_ui in
  draw_board player_source;
  Lwt.ignore_result
    (let open Lwt.Syntax in
     let* r = arena (display (pair ~x:player_ui ~o:player_random)) in
     let res = Html.createP doc in
     res##.innerText := js (Format.asprintf "Result: %a" pp_endplay r.endplay);
     Dom.appendChild doc##.body res;
     Lwt.return ());
  Js._false

let _ = Html.window##.onload := Html.handler onload
