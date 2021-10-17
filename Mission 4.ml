

  
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

  
