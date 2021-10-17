
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




  
