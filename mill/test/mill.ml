(*Now, We want to generate tree-shapped structures, such as recursive types*)

type aexp = 
  |X
  |Lit of int
  |Plus of aexp * aexp
  |Times of aexp * aexp


let generate_leaf =
  let open QCheck in
    Gen.oneof[Gen.return X;
              Gen.small_int |> Gen.map (fun i -> Lit i)
              ]

(* With this function, we will generate a size,and then a tree-shaped structure which has this size*)
(* The function Gen.oneof will randomly select an option from the list*)
let generate_eaxp1 =
  let open QCheck in 
    Gen.sized(Gen.fix (fun recgen n ->
                        match n with 
                        |0-> generate_leaf
                        |_ -> Gen.oneof [Gen.map2 (fun l r -> Plus (l,r)) (recgen (n/2)) (recgen (n/2));
                                        Gen.map2 (fun l r -> Times (l,r)) (recgen (n/2)) (recgen (n/2))]))

(*This function will generate a complete structure*)
(* To solve this problem, we can add an option in the list*)

let generate_eaxp2 =
  let open QCheck in 
    Gen.sized(Gen.fix (fun recgen n ->
                        match n with 
                        |0-> generate_leaf
                        |_ -> Gen.oneof [Gen.map2 (fun l r -> Plus (l,r)) (recgen (n/2)) (recgen (n/2));
                                        Gen.map2 (fun l r -> Times (l,r)) (recgen (n/2)) (recgen (n/2));
                                        generate_leaf]))
              
(*Except that we are about to find out that this generator rarely goes beyond 3 or 4 deep*)

  (*So we can do the same thing by adding frequencies*)

let generate_eaxp3 =
  let open QCheck in 
    Gen.sized(Gen.fix (fun recgen n ->
                        match n with 
                        |0-> generate_leaf
                        |_ -> Gen.frequency [(3,Gen.map2 (fun l r -> Plus (l,r)) (recgen (n/2)) (recgen (n/2)));
                                        (3,Gen.map2 (fun l r -> Times (l,r)) (recgen (n/2)) (recgen (n/2)));
                                        (1,generate_leaf)]))
    

let rec interpret exp x = 
  match exp with 
    |X -> x
    |Lit a -> a
    |Plus (a,b) -> interpret a x + interpret b x
    |Times(a,b) -> interpret a x * interpret b x


(*Let's test our generators*)

let arbitrary_eaxp3 = QCheck.make generate_eaxp3

let test_interpret3 = 
  let open QCheck in 
    Test.make ~count:100 ~name:"for all ae : eaxp -> interpret(Plus(ae,ae)) = interpret(Times(2,ae)) )"
      (pair (arbitrary_eaxp3) (small_int))
      (fun (e1, x) -> interpret (Plus(e1,e1)) x = interpret (Times(Lit(2),e1)) x)
                                  
let arbitrary_eaxp2 = QCheck.make generate_eaxp2

let test_interpret2 = 
  let open QCheck in
    Test.make ~count:100 ~name:"for all ae : eaxp -> interpret(Plus(ae,ae)) = interpret(Times(2,ae)) )"
      (pair (arbitrary_eaxp2) (small_int))
      (fun (e1, x) -> interpret (Plus(e1,e1)) x = interpret (Times(Lit(2),e1)) x)



let arbitrary_eaxp1 = QCheck.make generate_eaxp1

let test_interpret1 = 
  let open QCheck in
    Test.make ~count:100 ~name:"for all ae : eaxp -> interpret(Plus(ae,ae)) = interpret(Times(2,ae)) )"
      (pair (arbitrary_eaxp1) (small_int))
      (fun (e1, x) -> interpret (Plus(e1,e1)) x = interpret (Times(Lit(2),e1)) x)


let () =
  let open Alcotest in
    run "NOTES4" [("generate_aexp1", [QCheck_alcotest.to_alcotest test_interpret1]);
                  ("generate_aexp2", [QCheck_alcotest.to_alcotest test_interpret2]);
                  ("generate_aexp3", [ QCheck_alcotest.to_alcotest test_interpret3]);]
                                