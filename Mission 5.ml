
(*MISSION 5*)

(*LESSON 1: PRACTICING ON LISTS*)

(*EXERCICE: First-order functions on lists*)

let rec nth list int = match list with
  |[] -> failwith "List empty"
  |a::rest -> let rec count = function
      |[] -> 0
      |x::rest-> 1+count rest
      in if count list = int then a
      else if count list < int then failwith "List too small"
      else nth rest int;;
  
let list1 = ["1";"2";"3";"4";"5"];;
nth list1 3;;
(*nth list1 6;;*)

  
let rec rev acu = function 
  |[] -> acu
  |a::rest -> rev (a::acu) rest;;
  
rev [] list1;;
  
let rec append list1 list2 = match list1 with
  |[] -> list2
  |a::rest -> a::(append rest list2);;
  
let list1 = [1;2;3;4;5];;
let list2 = [6;7;8;9;10];;
  
append list1 list2;;
  
let rec rev_append list1 list2 = match rev [] list1 with
  |[] -> list2
  |a::rest -> a::(append rest list2);;
  
rev_append list1 list2;;
  
(*Exercise : Find your way in the OCaml manual*)

(*Exercise : Higher-order functions on lists*)

let rec map f = function
  |[]->[]
  |x::rest->(f x) :: (map f rest) ;;

(*A tail recursive function is one where the last tast that the function 
  does is calling itself. It doesn't do  any other operation other than
  calling itself. The accumulator version of functions are a good way to
  achieve that*)

map (fun x -> x + 1) [1;2;3;4;5;6];;

let rec rev_map f acu = function
  |[] -> acu 
  |x::rest-> rev_map f (f x::acu) rest;;

rev_map (fun x -> x + 1) [] [1;2;3;4;5;6];;

let rec iter f list = match list with
  |[] -> ()
  |x::rest -> f x; iter f rest;;

iter (Printf.printf "Element : %d\n%!") [ 2 ; 4 ; 6 ];;

let rec print_list sep conv alist = match alist with
  |[] -> ()
  |x::rest -> Printf.printf"%s %s" (conv x) sep; print_list sep conv rest;;

print_list ", " string_of_int [ 4 ; 8 ; 99 ];;
print_list " ++ " (fun x -> x) [ "aa" ; "bb" ; "cc" ];;

let rec fold (op:int -> int -> int) (acu:int) = function
  |[] -> acu
  |x::rest -> fold op (op acu x) rest;;
(*fold : (int -> int -> int) -> int -> int list -> int*)

fold (+) 0 [ 1 ; 2 ; 3 ; 4 ];;
fold ( * ) 1 [ 1 ; 2 ; 3 ; 4 ];;
  
let rec fold op acu = function
  |[] -> acu
  |x::rest -> fold op (op acu x) rest;;
(*fold : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a*) 

fold (fun a b -> a ^ " " ^ string_of_int b) "" [ 1 ; 2 ; 3 ; 4];;
fold (fun a b -> if a < b then b else a) 0 [ 10 ; 40 ; 20 ; 30 ];;
fold (fun a b -> b :: a) [] [ 4 ; 3 ; 2 ; 1 ];;

let exists1 f list = match (f,list) with
  |(_,[]) -> false
  |(_,x::rest) -> fold (fun a b -> f b || a) false list;;

let rec exists2 f = function
  |[] -> false
  |x::rest -> if f x = true then true
      else exists2 f rest;;
  
exists1 (fun x -> x < 10) [ 20 ; 5 ; 30 ];;
exists1 (fun x -> x < 10) [ 20 ; 40 ; 30 ];;
exists1 (fun x -> x < 10) [];;
  
exists2 (fun x -> x < 10) [ 20 ; 5 ; 30 ];;
exists2 (fun x -> x < 10) [ 20 ; 40 ; 30 ];;
exists2 (fun x -> x < 10) [];;
  
let (++) f g x = f (g x);;
((fun x -> x - 10) ++ abs) (-20);;
(abs ++ (fun x -> x - 10)) (-20);;
  
let rec forall f = function
  |[] -> true
  |x::rest -> f x && forall f rest;;
  
forall (fun x -> x>0) [1;2;3;-4;5;6];;
forall (fun x -> x>0) [1;2;3;4;5;6];;
  
let forall1 f = function
  |[] -> true
  |x::rest -> not (exists2 (not ++ f) (x::rest));;

forall1 (fun x -> x>0) [1;2;3;-4;5;6];;
forall1 (fun x -> x>0) [1;2;3;4;5;6];;  

(*Exercise : Association lists*)

let rec assoc key = function
  |[] -> raise Not_found
  |x::rest -> match x with
    |(a,b) -> if key=a then b else assoc key rest;;

let assoc1 = [ ("Rostom", true);("Lucy", true) ; ("Mike", false) ; ("Hilary", false) ;
               ("Donald", true) ];;

assoc "Donald" assoc1;;
assoc "Mike" assoc1;;
assoc "donald" assoc1;;

rev [] assoc1;;

let remove_assoc key list =
  let rec remove_assoc1 key list = match list with
    |[] -> []
    |x::rest -> match x with
      |(a,b) -> if key=a then rest 
          else remove_assoc1 key rest 
  in
  append (rev [] (remove_assoc1 key (rev []list))) (remove_assoc1 key list);;

remove_assoc "Mike" assoc1;;

  
(*FUCKING TREES*)

type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree;;
let max a b = if b > a then b else a;;

let rec depth tree = match tree with
  |Leaf(_) -> 0
  |Node(a,b) -> 1 + max (depth a) (depth b);;

depth (Leaf 100);;
depth (Node (Leaf 100, Leaf 200));;
depth (Node (Leaf 100, Node (Leaf 100, Node (Leaf 100, Leaf 200))));;

let rec build1 n x = match n with
  |0 -> Leaf(x)
  |n -> Node(build1 (n-1) x, build1 (n-1) x);;
(*The tree that is being built here is symmetric*)

let rec build2 n x = match (n,x) with
  |(0,x) -> Leaf(x)
  |(n,x) -> Node(Leaf(x), build2 (n-1) x);;
(*The tree that is being built here is asymmetric*)

(*Note: The Nodes do not have a value assigned to them.
  Only the leaves are assigned with a value.
  This means that if an elemend doesn't have a value, it's a node*)

depth (build2 13 "a");;

let tree1= build1 2 0;;
let tree2= build2 2 0;;

let print_tree tos tree =
  let rec loop margin = function
    | Leaf x -> Printf.printf "___ %s\n%!" (tos x)
    | Node (a,b) ->
        Printf.printf "____" ;
        loop (margin ^ "|   ") a ;
        Printf.printf "%s|\n%s|" margin margin ;
        loop (margin ^ "    ") b
  in
  loop "   " tree;;

print_tree string_of_int (Leaf 100);;
print_tree string_of_int(Node (Leaf 100, Leaf 200));;
print_tree string_of_int(Node (Leaf 100, Node (Leaf 100, Node (Leaf 100, Leaf 200))));;



print_tree string_of_int tree1;;
print_tree string_of_int tree2;;
(* the parameter toss is a function that converts the content type of 
   the tree to a string. In this case, the trees' content are integers.
   Therefore we use the function string_of_int.
     Toss is dependant on the type of variable the tree contains*)

let f a b = (a,b);;
let g (a,b) = a ;;
let h (a,b) = b;;

print_tree string_of_int (

  Node(
    Node(
      Node(Leaf 10,Leaf 12) ,Node(Leaf 14,Leaf 16)),
    
    Node(
      Node(Leaf 18,Leaf 20),Node(Leaf 22,Leaf 24))
  ));;


(*let build_fold n x f = I give up*)
  
let rec tmap f = function
  |Leaf (x) -> Leaf(f x)
  |Node(a,b) -> Node (tmap f a, tmap f b);;

let tree1= build1 2 0;;
print_tree string_of_int tree1;;

let tree2 = tmap string_of_int tree1;;
print_tree (fun x->x) tree2;;


let rec tfind pred = function
  |Leaf (x) -> if pred x = true then Some x else None
  |Node(a,b) -> match tfind pred a with
       | None -> tfind pred b
       | r -> r
  ;;
                  
let rec contains c = function
  |Leaf (x) -> if c = x then true else false
  |Node(a,b) -> contains c a || contains c b;;
                     
let tree=(Node(Node(Leaf 1, Leaf 2),Node(Leaf 3, Leaf 4)));;
print_tree string_of_int tree;;

tfind (fun x -> if x = 1 then true else false) tree;;
contains 1 tree;;
contains 5 tree;;

let rec replace pred sub = function 
  |Leaf(x) -> if pred (Leaf (x)) = true then sub else Leaf(x)
  |Node(a,b) -> if pred (Node(a,b)) then sub else
        Node (replace pred sub a, replace pred sub b);;


let tree = 
  Node(Node(Node(Leaf 10,Leaf 12) ,Node(Leaf 14,Leaf 16)),
       Node( Node(Leaf 18,Leaf 20),Node(Leaf 22,Leaf 24)));;
print_tree string_of_int tree;;

let tree_rep = replace (fun t->contains 14 t && depth t = 1) (Leaf 0) tree;;
print_tree string_of_int tree_rep;;
depth tree_rep;;