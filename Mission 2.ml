
(*Recursive functions*)
(*without acc*)

let rec count_ones = function
  |[] -> 0
  |x::rest-> if x=1 then 1+count_ones rest  else count_ones rest ;;

(*with acc*)
let rec count_ones2 acu = function
  |[] -> acu
  |x::rest-> if x=1 then count_ones2 (acu+1) rest  else count_ones2 acu rest  ;;



(*without acc*)
let rec sum = function
  |[] -> 0
  |x::rest -> x + sum rest;;

(*with acc*)
let rec sum2 acu = function
  |[] -> acu
  |x::rest -> sum2 (acu+x) rest;;



(*without acc*)
let rec perms = function 
  |[] -> [] 
  |(a,b)::rest -> (b,a) :: perms rest;;

(*with acc*)
let rec permsacu acu = function 
  |[] -> acu
  |(a,b)::rest -> (b,a) :: permsacu acu rest;;

perms [ (1, true) ];;
perms [ ('a', 8) ; ('b', 7) ];;

permsacu [] [ (1, true) ];;
permsacu [] [ ('a', 8) ; ('b', 7) ];;


let rec mk_list = function
  |0 -> [] 
  |x -> x :: mk_list (x-1) ;;

(* l'accumulateur doit changer !*)
let rec mk_aculist acu = function
  |0 -> acu
  |x -> mk_aculist (x::acu) (x-1);;

let rec list_ones acu2 = function
  |0 -> acu2
  |x -> list_ones (1::acu2) (x-1);;

mk_aculist [] 5;;
let l1 = mk_list 10000;;

let l2 = mk_aculist [] 10000;;

let l3 =  list_ones [] 10000;;

sum l2;;
sum2 0 l2;;

count_ones l2;;
count_ones2 0 l2;;
l3;;
count_ones l3;;
count_ones2 0 l3;;


(*Higher-order functions*)

let fun_list = [ count_ones ; sum ]
let fun_pair = ( count_ones, sum )

let fsum f = f 0 + f 1 + f 2 + f 3 ;;
fsum abs;; (*fonction connue*)
fsum (fun x -> 2 * x) ;; (*fonction anonyme*)
                         
let flist f = [ f 0 ; f 1 ; f 2 ; f 3 ] ;;

flist abs;;
flist string_of_int;;
flist (fun x -> x + 1);; (*flist (λx.x+1)*)

(*without acc*)                
let rec fsumlist f = function
  |[]->0
  |x::rest-> f x + fsumlist f rest ;;

fsumlist (fun x -> x+1) [1;1;1;1]

(*with acc*)                
let rec fsumlist_acc f acu = function
  |[]->acu
  |x::rest-> fsumlist_acc f (acu+ f x) rest;;

fsumlist_acc (fun x -> x+1) 0 [1;1;1;1];;

List.fold_left ;;

(*also accessible by doing List.map*)
let rec map f = function
  |[]->[]
  |x::rest->f x :: map f rest ;;

map (fun x -> x + 1) [];;
map (fun x -> x + 1) [ 5 ];;
map (fun x -> x + 1) [ 5 ; 10 ; 15 ];;
map string_of_int [ 5 ; 10 ; 15 ];;
map (fun x -> (x, string_of_int x)) [ 5 ; 10 ; 15 ];;
map;;


(*also accessible by doing List.find*)
let rec find f = function
  |[] -> raise Not_found 
  |x::rest -> if f x = true then x else find f rest;;

find (fun x->true) [];;
find (fun x->(x > 10)) [ 5 ; 12 ; 7 ; 8 ];;
find (fun x->(x > 10)) [ 5 ; 6 ; 7 ; 8 ];;
find (fun x->true) [ 5 ; 10 ; 15 ];;

let omap f = function
  |None -> None
  |Some a -> f a ;;

omap (fun x -> Some true) None;;
omap (fun x -> Some true) (Some 10);;
(* omap (fun x -> !x) (Some true);; *)
omap (fun x -> !x) None;;
omap (fun x -> Some (x + 1)) None;;
omap (fun x -> Some (x + 1)) (Some 100);;
  
  
  
  
