open Engine

let iter_board f board =
    let rec aux x y h =
        match get_square board (x, y) with
        | None -> if not h then () else aux x (y + 1) false
        | Some a ->
            f a;
            aux (x + 1) y true
    in
    aux 0 0 true

let list_max f l =
    if List.length l = 0
    then failwith "erreur"
    else
      let rec aux l acc =
          match l with
          | [] -> acc
          | h :: t -> if f h > f acc then aux t h else aux t acc
      in
      aux l (List.nth l 0)

let list_min f l =
    if List.length l = 0
    then failwith "erreur"
    else
      let rec aux l acc =
          match l with
          | [] -> acc
          | h :: t -> if f h < f acc then aux t h else aux t acc
      in
      aux l (List.nth l 0)
