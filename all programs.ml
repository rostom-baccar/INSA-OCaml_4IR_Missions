
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
  
  
  
  
(*Mission 3*)

(*Lesson 1: Pattern matching, inner let*)

(* _ = other *) 
let int_match = function
  | 0 -> "zero"
  | 1 -> "one" 
  | 2 -> "two"
  | _ -> "idk" ;; (*this has to be at last, the order is important*) 
                  (*anything beyond _ will be unmatched*)
  
(*Examples*)
int_match 0;;
int_match 1;;
int_match 2;;
int_match 3;;
int_match 4;;

let bool_match = function
  |true -> "definitely true"
  |false -> "definitely not true" ;;

(*Examples*)
bool_match true;; 
bool_match false;;

let string_match = function
  |"string" -> 0
  |_ -> 1 ;; (*mandatory here*)

(*Examples*)
string_match "string";;
string_match"abc";;

let tuple_match = function 
  | (0, true) -> 1
  | (0, false) -> -1
  | (_, false) -> 2
  | (_, true) -> 3 ;;
(*these 4 cases cover all the possible values of the tuples*)
(*we would have gotten a non-exhaustive pattern warning otherwise*)

(*Examples*)
tuple_match(0,true);;
tuple_match(0,false);;
tuple_match(1,true);;
tuple_match(1,false);;

(*Difference between match and function*)
(*function*)
let confused2 (a,b) = function
  | (0, true) -> 1
  | (n ,_) -> 10 ;; 
(* val confused2 : 'a * 'b -> int * bool -> int = <fun> *)

let bar x y = function
  | (0,0) -> x
  | _ -> y;;
(* val bar : 'a -> 'a -> int * int -> 'a = <fun> *)

(*match*)
let foo x y = match (x,y) with
  | (0,0) -> x
  | _ -> y;; 
(* val foo : int -> int -> int = <fun> *)

(*uncurried version: 1 argument int*int*string*)
let calc_uncurried = function
  | (x, y, "add") -> x + y
  | (x, y, "sub") -> x - y
  | (x, y, "mul") -> x * y
  | (x, 0, "div") -> failwith "Division by zero."
  | (x, y, "div") -> x / y
  | _ -> failwith "Unknown operator." ;;
(*val calc_uncurried : int * int * string -> int = <fun>*)

(*Examples*)
calc_uncurried (110, 220, "add");;
  
(*curried version with match: we have 3 arguments now*)
let calc_curried x y string =
  match (x,y,string) with (*match always groups the arguments in a tuple*)
  | (x, y, "add") -> x + y
  | (x, y, "sub") -> x - y
  | (x, y, "mul") -> x * y
  | (x, 0, "div") -> failwith "Division by zero."
  | (x, y, "div") -> x / y
  | _ -> failwith "Unknown operator." ;;
(*val calc_curried : int -> int -> string -> int = <fun>*) 
                                                           
(*Examples*)
calc_curried 110 220 "add";;
  

let xor a b =
  match (a,b) with
  |(false,false)->false
  |(true,false)->true
  |(false,true)->true
  |(true,true)->false ;;
  
(*Note: Function lets you add in arguments without defining them*)
(*Function is used in case of polymorphic functions*)
(*in the case of xor, match is used to define both a and b*)

type operation = Add |Sub | Mul | Div ;;
  
(*rewritten curried version with match: the 3rd arg is of type operator*)
let calc_curried_op x y op =
  match (x,y,op) with (*match always groups the arguments in a tuple*)
  | (x, y, Add) -> x + y
  | (x, y, Sub) -> x - y
  | (x, y, Mul) -> x * y
  | (x, 0, Div) -> failwith "Division by zero."
  | (x, y, Div) -> x / y;;
  (*| _ -> failwith "Unknown operator." ;; *) 
(*the line above is useless now, we have already enumerated all possible 
  values for the operation type*)

(*Examples*)
calc_curried_op 4 5 Mul;;
  

(*Pitfall: do not get confused by variables in patterns*)
let equal a b =
  match b with
  | a -> true (*warning: a here is not the first argument a*)
  | _ -> false;;
(*a in the pattern-matching is a dummy variable*)
(*meaning we could have replaced it with any other letter*)
(*as a result, the function matches b with anything of type b*)
(*and always returns true*)
(*which is why the last line isn't used*)
(*we also could have replaced the first line with _ ->true*)
  
(*Examples*)
equal 0 0;;
equal 0 1;;

(*Example where the argument is used*)
let equal a b =
  match b with
  | x when x = a -> true
  | _ -> false;;


(*Lesson2 : SEQUENCE*)

let seq1 x =
  Printf.printf "Calling f with value x = %d\n %!" x ;
  x * x + 2;;
(*%d is for ints*)
(*%s is for strings*)
(*s%.3f is for floats with 3 digits after the comma*)
(*%! is for FLUSH*)

(*Examples*)
seq1 3;;

let seq2 x =
  Printf.printf "Calling f with value x = %d\n %!" x ; 
  x + 1 ;
  x * x + 2;; 
(*the compiler processes last arguments first*)
(*x+1 will be ignored*)

(*Examples*)
seq2 3;;

let show f v = 
  Printf.printf "Function called with value %d\n %!"; v; f v ;;
(*type of show:  (a -> b) -> a -> b *)

let double x = x * 2 ;;

(*Examples*)
(*(int -> int) -> int -> int*)
show double 100;; 
(*(int -> string) -> int -> string*)
show string_of_int 100;;

let bool_to_string = function
  |true ->"true"
  |false -> "false";;

(*(bool -> string) -> bool -> string*)
show bool_to_string true;;

let pshow cv f v =
  Printf.printf "Function called with value %d\n %!"; cv v; f v ;; 
(*type of pshow:  (a -> b) -> (a -> c) -> a -> c *)

(*Examples*) 
pshow string_of_int double 0;;
pshow string_of_int double 40;;

(*Another way of calling the function*)
  
let show_double = pshow string_of_int double ;;

let a = show_double 0 ;;
let b = show_double 40 ;;


(*Examples with pairs*)
let sumsquare (x,y) = x * x + y * y 
let string_of_pair (x,y) = Printf.sprintf "(%d, %d)" x y 
let show_sumsquare = pshow string_of_pair sumsquare ;;
let a = show_sumsquare (1, 2) ;;
let b = show_sumsquare (10, 5) ;;

  
(*Lesson 3: INNER LET*)

let is_leap_year n =

   (* Test if n is a multiple of x. *)
  let multiple_of x = n mod x = 0 in

  let is_mul4 = multiple_of 4 in
  let is_mul100 = multiple_of 100 in
  let is_mul400 = multiple_of 400 in

  is_mul400 || (is_mul4 && not is_mul100) ;;


(*Same function with baby steps*)

let is_leap_year2 n =
  (n mod 400 = 0)  || ((n mod 4 = 0) && (not (n mod 100 = 0)));;

let is_leap_year3 n =
  let a = n mod 400 = 0 in
  let b = n mod 4 = 0 in
  let c = n mod 100 = 0 in
  a || b && not c;;

let is_leap_year4 n =
  let multiple x = n mod x = 0 in (*declared above to use it below*)
  let a = multiple 400 in
  let b = multiple 4 in
  let c = multiple 100 in
  a || b && not c;;

(*Examples*)
is_leap_year 2004;;
is_leap_year 2005;;

is_leap_year2 2004;;
is_leap_year2 2005;;

is_leap_year3 2004;;
is_leap_year3 2005;;

is_leap_year4 2004;;
is_leap_year4 2005;;

(*inner let for variables is the equivalent of fun for functions*)

(*Variables can be bound at once woth let and*) 
let res1 =

  let a = 10 in (*c = 10*)

  let a = 20
  and b = a in (*b = c*)

  (a, b);; (*20,10*)

let res2 =

  let a = 10 in (*usused because the inner let below doesn't use it*)

   (* Two inner let (instead of one). *) 
  let a = 20 in
  let b = a in

  (a, b);; (*20,20*)

(*EXERCICE: Guessing the type*) 
let result1 =
  
  let a = 10
  and b = 20
  and c = true
  in

  let a = c
  and b = a
  and c = b
  in

  (a,b,c);; (*Result: (true, 10, 20)*)

let result2 =
  10 + 
  let x = 5 + 5 in
  let y = x * x in
  2 * y;; (*Result: 210*)

let result3 =
  "Foo is " ^ 
  let x = string_of_int (5*5) in
  x ^ " I say." (*Result: "Foo is 25 I say."*)

    (*let with unit type*)
let () = print_endline "Hello.";;

let () =
  print_endline "You get" ;
  print_endline "a shiver" ;
  print_endline "in the dark" ;;

(*EXERCICE: Compare let and match*)
(*All these functions give the same result*)
(*Result f 1 (2,3) = 1+2+3 =6 *)

(*Regular*)
let f a (b,c) = a+b+c;;

(*MATCH*)
let f a p =
  match p with
  | (b,c) -> a+b+c;;

(*INNER LET*)
let f a p =
  let (b,c) = p in
  a+b+c;;

(*FUNCTION*)
let f a = function
  | (b,c) -> a+b+c;;

(*FUN*)
let f a = fun (b,c) -> a+b+c;;

(*EXERCICE*)
(*Rewrite this function in a much simpler way*)

let get_triple arg =
  match arg with
  | (a, p) ->
      begin match p with
        | (b,c) -> [ a ; b ; c ]
      end;;
(*type: 'a * ('a * 'a) -> 'a list*)

let get_triple_simpler (a,(b,c)) = a::b::c::[];;





(*Lesson4: EXPRESSIONS AND DEFINITIONS*)

(*EXERCICE: EXPRESSIONS*)

let fancy1 x =
  begin
    if x mod 2 = 0 then
      (fun a b -> a + b )
    else
      (fun a b -> a - b)
  end
    (x-1) (x+1);;
(* fancy1 : int -> int *)

fancy1 2;;
fancy1 3;;

let fancy2_babysteps a b c =
  "Hello " ^ 
  let f = 
    if (a+b+c) mod  2 = 0 then (fun x y z->x+y+z) 
    else (fun x y z ->x-y-z)
  in string_of_int (f a b c)
     ^ " This better work" ;;

fancy2_babysteps 1 2 3;;

let fancy2 a b c =
  "Hello " ^ 
  let f = 
    if (a+b+c) mod  2 = 0 then begin
      match a with
      |0 -> fun x-> 3*x
      |_ -> function
          |4->0
          |_->1
    end
    else (fun x->6*x)
  in string_of_int (f a)
     ^ " This better work" ;;

fancy2 1 2 3;;




  
(*MISSION 4: TYPE STRUCTURES*)

(*LESSON 1: RECORDS*)

(* This defines a record type. *)
type coordinates =
  { long: float ;
    lat:  float };;

(* This defines only an alias. *)
type path = coordinates list;;

(* Another record *)
type region =
  { region_name: string ;
    borders: path ;
    has_coastline: bool };; 
  
let point1 = { long = -0.3 ;
               lat  = 42.5 };;
let point2 = { point1 with long = -1.0 } ;;


(*Equivalent expressions:*)
let get_lat1 c = c.lat;; (*c.lat : c is of type coordinate*) 
let get_lat2 { lat = x ; _ } = x;;
let get_lat3 { lat ; _ } = lat;;

get_lat1 point1;;
get_lat2 point1;;
get_lat3 point1;;
(*get_lat : coordinates -> float*)

let get_all { long ; lat } = (long, lat);; (*gives a tuple*)

(*Advanced pattern matching*)
(*Checks if region is well defined: region name and at least one border*)
let is_good = function
  | { region_name = "" } -> false
  | { borders = [] } -> false
  | _ -> true;;

(*it is possible to join patterns:*)
(*| { region_name = "" } | { borders = [] } -> false*)

(** Functions are first-class, they can appear in records. **)

(*depricated version of type test*)
(*
  type test =
   { (* A function which should be tested. *)
     fonc: (int -> int) ; 
     (* An argument, which will be given to the function. *) 
     arg: int ; 
     (* The expected result. *)
expect: int };;
*)
(*EXERCICE: RECORDS*)

(*depricated version of type test*)
(*
type 'a test =
  { fonc: ('a -> int) ;
    arg: 'a ;
    expect: int };;
*)
type ('a, 'b ) test_poly =
  { fonc: ('a -> 'b) ;
    arg: 'a ;
    expect: 'b };;

let apply = function
  |{fonc=f; arg=a; expect=e} when f a = e-> true
  |_->false;;

let test1 = {fonc=(fun x->2*x);
             arg=5;
             expect=10} 
and test2 = {fonc=(fun x->2*x);
             arg=5;
             expect=11} ;;

let result1 = apply test1 
and result2 = apply test2;;

(*LESSON 2: PARAMETERIZED TYPES*)

(*EXAMPLES*)
let test31 = {fonc=(fun x -> match x with
    |true->1
    |false->0);
   arg=true;
   expect=1} 
and test41 = {fonc=(fun x-> match x with
    |"rostom"->1
    |_->0
  );
   arg="rostom";
   expect=1} ;;
let test32 = {fonc=(fun x -> match x with
    |true->1
    |false->0);
   arg=true;
   expect=0} 
and test42 = {fonc=(fun x-> match x with
    |"rostom"->1
    |_->0
  );
   arg="not rostom";
   expect=1} ;;

let result31 = apply test31 
and result41 = apply test41
and result32 = apply test32 
and result42 = apply test42;;

let test51 = {fonc=(fun x -> match x with
    |true->"true"
    |false->"false");
   arg=true; 
   expect="true"};;
let test52 = {fonc=(fun x -> match x with
    |true->"true"
    |false->"false");
   arg=true; 
   expect="false"};;

let result51 = apply test51 
and result52 = apply test52;;

(*LESSON 3: ARRAYS*)

Array.make;; (*int -> 'a -> 'a array *)
let john = Array.make 100 true;;
john.(5)<-false;; (*to set a value*)
john.(5);; (*to read a value*)

let foo a i = a.(i);; (*foo : 'a array -> int -> 'a*)
let bar a i v = a.(i) <- v;; (*'a array -> int -> 'a -> unit *)
                             
(*LESSON 4: MUTABLE RECORDS*)

type player =
  { name: string ;
    age: int ;
    mutable points: int };; 

(*Mutable fields can be modified like arrays: foo.points <- 800
 which has type unit.*) 


let show_player player =
  Printf.printf "%s %d %d \n%!" player.name player.age player.points;;
                              
let new_player string int = {
  name=string;
  age=int;
  points=0};;
  
let add_points player int = 
  player.points <- player.points + int;;
  
let rostom = new_player "Rostom" 22;; (*must not use uppercase in var*)
let ines = new_player "Ines" 22;; 

add_points ines 30;;
add_points rostom 10;;
add_points rostom 10;;
add_points rostom 10;;
rostom;;
ines;;
                             
type iplayer = {
  iname: string;
  iage: int;
  ipoints: int};;
  
let show_iplayer iplayer =
  Printf.printf "%s %d %d \n%!" iplayer.iname iplayer.iage iplayer.ipoints;;                      
                             
let new_iplayer string int = {
  iname=string;
  iage=int;
  ipoints=0};;                    


let add_ipoints iplayer int = 
  {iname=iplayer.iname;
   iage=iplayer.iage;
   ipoints=iplayer.ipoints + int }

                             
type 'a ref = { mutable contents: 'a };;                           
(*type 'a ref = { mutable contents : 'a; }*)
let create x = { contents = x };;
(*create : 'a -> 'a ref*)
let read rf = rf.contents;;
(*read : 'a ref -> 'a*)
let write rf v = rf.contents <- v;;
(*write : 'a ref -> 'a -> unit*)

let test () = 
  let ref0 = create 0 in
  write ref0 ((read ref0) + 1);write ref0 ((read ref0) + 1);read ref0;;

test();;

let ref x = { contents = x };;
let (!) rf = rf.contents;;
let (:=) rf v = rf.contents <- v;;
    
let test1()=
  let i = ref 0 in
  i:=!i+1;  i:=!i+1;  i:=!i+1; !i;;

test1 ();;

(*EXERCICE*)
let gen1 =
  fun () -> 0;;
(*EQUIVALENT*)
let gen1 () = 0;; 
gen1();;

let gen2=
  fun()->
    let count = ref 0 in
    count := !count + 1 ;
    !count;;
gen2();; (*count= 1*)
gen2();; (*count= 1*)
gen2();; (*count= 1*)

let gen3 =
  let count = ref 0 in
  fun () ->
    count := !count + 1 ;
    !count;;

gen3();; (*count= 1*)
gen3();; (*count= 2*)
gen3();; (*count= 3*)

(*LESSON 5: VARIANT DATATYPES*)
type color = White | Yellow | Green | Blue | Red;;
type role = Player | Referee;;
type role = Player of color * int | Referee;;

let role1 = Referee;;
let role2 = Player (Green, 8);;
let role3 = Player (Yellow, 10);;
let role4 = Player (Yellow, 12);;

let get_number = function
  | Referee -> 0
  | Player (_, nb) -> nb;; 

type people =
  { name: string ;
    role: role ;
    age: int };;

let boolean a b =
  if a = b then true else false;;

let same_team people1 people2 = match (people1.role,people2.role) with
  |(Player(color1,int1),Player(color2,int2))-> if color1=color2
      then true else false
  |_->false;; 
           
let people1 = { name = "ref1";
                role = role1;
                age = 45};;
let people2 = { name = "p1";
                role = role2;
                age = 25};;
let people3 = { name = "p2";
                role = role3;
                age = 23};;
let people4 = { name = "p3";
                role = role4;
                age = 28};;

same_team people1 people2;; (*false*)
same_team people2 people3;; (*false*)
same_team people3 people4;; (*true*) 


let is_number people int = match people.role with
  |Player(color,number) -> if int = number then true else false
  |_ -> false;;

is_number people1 1;; (*Referee: false*)
is_number people2 10;; (*false*)                 
is_number people2 8;; (*true*)
                      
(* LESSON 5: Parameterized and recursive variants *)

type 'a mylist = Empty | Cell of 'a * 'a mylist;;
let cell1 = Cell( 10, Cell(20, Empty) );;

let myhd = function
  |Empty -> failwith "empty list"
  |Cell(a,b)->a;;

myhd cell1;;

let mytl = function
  |Empty -> failwith "empty list"
  |Cell(a,b)->b;;

mytl cell1;;

let rec mylength = function
  |Empty -> 0
  |Cell(a,b)-> 1 + let result=Cell(myhd b, mytl b) in
               mylength result;;

myhd cell1;;
mytl cell1;;
cell1;;
mylength cell1;;


let rec mylength = function
  |Empty -> 0
  |Cell(a,b)-> 1 + mylength b;;
               
mylength cell1;;             
               
let rec mylength2 acc = function
  |Empty -> acc
  |Cell(a,b)-> mylength2 (acc+1) b;;
               
mylength2 0 cell1;; 
               
let mylength l =
  let rec loop acu l = function 
    |Empty -> l
    |Cell(a,b)-> mylength2 (l+1) b
  in loop 0 l;;

(*EXERCICE: OPTION TYPE*)
let ohd = function
  |[] -> None
  |x::_ -> Some x;;

let otl = function
  |[] -> None
  |_::rest -> Some rest;;

(*LESSON 6: EXERCICES ON LISTS*)

type color = White | Yellow | Green | Blue | Red;; 
type role = Player of color * int | Referee;;

type people =
  { name: string ;
    role: role ;
    age: int };;

let rec get_referees = function
  |[] -> []
  |a::rest -> if (a.role=Referee) then a::get_referees rest else get_referees rest;;

let r1 = { name = "ref1";
           role = Referee;
           age = 40}
and r2 = { name = "ref2";
           role = Referee;
           age = 45}
and r3 = { name = "ref3";
           role = Referee;
           age = 50}
and p1 = { name = "player1";
           role = Player (Yellow, 10);
           age = 18}
and p2 = { name = "player2";
           role = Player (Green, 11);
           age = 45}
and p3 = { name = "player3";
           role = Player (Blue, 12);
           age = 45}
and p4 = { name = "player4";
           role = Player (Red, 13);
           age = 19};;


let list1 = [r1;p1;r2;p2;p3;r3;p4];;
get_referees list1;;

let rec get_younger list int = match (list,int) with
  |([],_) -> []
  |(a::rest,_) -> if (a.age<int) then a::get_younger rest int
      else get_younger rest int;;

get_younger list1 20;;

let rec find_color list color = match (list,color) with
  |([],_) -> None 
  |(a::rest,_)-> match a.role with
    |Referee -> find_color rest color
    |Player(c,n)-> if color = c then Some a else find_color rest color;; 

find_color list1 Blue;;

(*EXERCICE: GENERIC FUNCTIONS*)

let rec filter pred alist = match (pred,alist) with
  |(pred,[]) -> []
  |(pred, a::rest) -> if pred a = true then a::filter pred rest
      else filter pred rest;;
    
filter (fun x -> x mod 2 = 0) [ 1 ; 2 ; 3 ; 4 ; 5 ; 6];;

let get_referees1 = let f =(fun x ->if x.role=Referee then true else false) in
  filter f list1;;

let get_younger1 = let f =(fun x -> if x.age<20 then true else false) in
  filter f list1;;

let rec find pred alist = match (pred,alist) with
  |(pred,[]) -> None
  |(pred, a::rest) -> if pred a = true then Some a
      else find pred rest;;

let has_color color people = match (color,people.role) with
  |(_,Referee) -> false
  |(color,Player(c,n)) -> if color=c then true else false;;

has_color Blue p1;;
has_color Blue p3;;

let rec find_color1 list color = match list with
  |[] -> None
  |a::rest -> if has_color color a then Some a else find_color1 rest color;;

  
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
nth list1 6;;

  
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
   the tree to a string. In this case, the trees' content are interers.
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
  |Node(a,b) -> tfind pred a;;
                   
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