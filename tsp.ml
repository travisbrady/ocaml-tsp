open Printf
module R=Random

let tours_per_gen = 1800 
let generations = 150
let mut_prob = 0.5
let cross_prob = 0.5
let k_tourn_entrants = 3

let opt_file = "data/eil51.opt.tour"
let file = "data/eil51.tsp"
let num = Str.regexp "^ *[0-9]"
let space = Str.regexp " +"

(* val list_min : int list -> int *)
let list_min = List.fold_left min 1000000
(* val split_coords : string -> string list *)
let split_coords x = Str.split space x
(*val nint : float -> int *)
let nint x = truncate(x +. 0.5);;
(*val dist : 'a * float * float -> 'b * float * float -> int *)
let dist (_, x1, y1) (_, x2, y2) = 
    nint(sqrt(((x1 -. x2)**2.0 +. (y1 -. y2)**2.0)))

(* Accepts list of 3 strings and produces a 3-tuple *)
(*val make_city : string list -> (int * float * float) option *)
let make_city = function
      [num; x; y] -> Some (int_of_string num, float_of_string x, float_of_string y)
    | _ -> None

(* Like Haskell's Data.Maybe.catMaybes *)
(* val de_option : 'a option list -> 'a list *)
let de_option l = List.rev (List.fold_left (fun acc x ->
    match x with
        | None -> acc
        | Some thing -> thing::acc) [] l)

(* val create_dist_map : ('a * float * float) list -> int list list *)
let create_dist_map cities = List.map (fun x ->
    List.map (fun y -> dist x y) cities) cities

(*val make_dist_array : ('a * float * float) list -> int array array *)
let make_dist_array cities = 
    Array.of_list (List.map Array.of_list (create_dist_map cities))

(* Lookup distance from one city to another *)
(* val get_dist : int -> int -> 'a array array -> 'a *)
let get_dist num1 num2 da = let x = num1 - 1 in
    let y = num2 - 1 in
    da.(x).(y)

(* val tour_dist : int array array -> int list -> int *)
let tour_dist da tour =
    let rec tour_dist' tour total = match tour with
        | [] -> total
        | [x] -> total + (get_dist x 1 da)
        | (x::y::rest) -> tour_dist' (y::rest) ((get_dist x y da) + total)
    in
    tour_dist' (1::tour) 0

let proc_file = 
    let ic = open_in file in
    try
        let length = in_channel_length ic in
        let buf = String.create length in
            really_input ic buf 0 length;
            close_in ic;
            Str.split (Str.regexp "\n") buf
    with e ->
        close_in_noerr ic;
        raise e

let get_opt_tour = 
    let ic = open_in opt_file in
    try
        let length = in_channel_length ic in
        let buf = String.create length in
            really_input ic buf 0 length;
            close_in ic;
            let lines = Str.split (Str.regexp "\n") buf in
            let c = List.filter (fun x -> Str.string_match num x 0) lines in
            let cities = List.map int_of_string c in
            List.tl cities
    with e ->
        close_in_noerr ic;
        raise e

let init_proc =
    let lines = proc_file in
    let coords = List.filter (fun x -> 
        Str.string_match num x 0) lines in
    let hey = List.map split_coords coords in
    let opt_cities = List.map make_city hey in
    let cities = de_option(opt_cities) in
    make_dist_array cities

(*
val make_tour : int -> int list
*)
let make_tour ncities = Util.shuffle(Util.range 2 ncities)
(* val make_many_tours : int -> int -> int list list *)
let make_many_tours ncities ntours = List.map (fun x -> 
    make_tour ncities) (Util.range 1 ntours)

(* val get_next_city : int list -> int -> int *)
let get_next_city tour city =
    let rec loop t = match t with
        | [] -> raise Not_found
        | [last] -> 1
        | (curr::next::rest) when curr==city -> next
        | (curr::rest) -> loop rest
    in
    match city with
        | 1 -> List.hd tour
        | x -> loop tour

(* val random_of : 'a list -> int -> 'a *)
let random_of xs n = List.nth xs (R.int n)

(* val argmin : ('a list -> int) -> 'a list list -> 'a list *)
let argmin dist_machine lst =
    let rec argmin' lst curr curr_dist = match lst with
        | [] -> curr
        | (x::xs) -> let this_dist = dist_machine x in
            if this_dist < curr_dist
                then argmin' xs x this_dist 
                else argmin' xs curr curr_dist
    in
    argmin' lst [] 1000000

(* val tournament : ('a list -> int) -> int -> 'a list list -> 'a list *)
let tournament dist_machine n tours = 
    let len = List.length tours in
    let contestants = List.map (fun x ->
        random_of tours len) (Util.range 0 n) in
    argmin dist_machine contestants

(* val choose_one :
 * 'a array array -> int -> int list -> int list -> int list -> int
 *)
let choose_one dist_array city mom dad not_picked =
    let mom_next = get_next_city mom city in
    let dad_next = get_next_city dad city in
    let mom_dist = dist_array.(city - 1).(mom_next - 1) in
    let dad_dist = dist_array.(city - 1).(dad_next - 1) in
    match mom_dist <= dad_dist && List.mem mom_next not_picked with
        | true -> mom_next
        | false -> match dad_dist <= mom_dist 
            && List.mem dad_next not_picked with
            | true -> dad_next
            | false -> List.hd not_picked

(* val mutate : ('a list -> 'b) -> 'a list -> 'a list *)
let mutate dist_machine tour = 
    let rec two_opt tour = 
        let tour_len = (List.length tour + 1) in
        let ll = [R.int tour_len; R.int tour_len] in
        let [si; ei] = List.sort compare ll in
        let _start = Util.take si tour in
        let mid = List.rev (Util.take (ei - si) (Util.drop si tour)) in
        let _end = Util.drop ei tour in
        let _done = List.concat [_start; mid; _end] in
        match (dist_machine _done ) < (dist_machine tour) with
            | true -> two_opt _done
            | false -> tour
    in
    let mut_roll = R.float 1.0 in
    match mut_roll < mut_prob with
        | true -> two_opt tour
        | false -> tour

(*
val greedy_cross :
  (int -> 'a -> 'b -> int list -> int) ->
  'a -> 'b -> int list -> int list -> int list
*)
let rec greedy_cross chooser mom dad not_picked acc =
    match not_picked with
        | [] -> acc
        | [last] -> last::acc
        | _ ->
        let city = match acc with
            | [] -> 1
            | (x::xs) -> x
        in
        let the_chosen = chooser city mom dad not_picked in
            greedy_cross chooser mom dad (Util.delete the_chosen not_picked) (the_chosen::acc)

(* val maybe_cross : (int -> int list -> 'a -> int list -> int) ->
  ('b -> 'a) -> int list -> 'b -> int list
*)
let maybe_cross chooser tourn_machine tour pop =
    let cross_roll = R.float 1.0 in
    match cross_roll < cross_prob with
        | false -> tour
        | true -> let dad = tourn_machine pop in
            greedy_cross chooser tour dad tour []

(* val gen_generation :
  ('a -> 'b -> 'c) -> ('b -> 'a) -> ('c -> 'd) -> 'b -> int -> 'd list
*)
let gen_generation crosser tourn_machine mutator pop pop_size =
    let rec gen_generation' pop_size acc = 
        match pop_size with
        | 0 -> acc
        | _ -> 
            let mom = tourn_machine pop in
            let this_tour = crosser mom pop in
            let mutant = mutator this_tour in
            gen_generation' (pop_size - 1) (mutant::acc)
    in
    gen_generation' pop_size []

(* val keep_on :
  ('a -> int) -> ('a list -> int -> 'a list) -> 'a list -> int -> 'a list
*)
let rec keep_on dist_machine genmach pop generations =
    match generations with
        | 0 -> pop
        | x -> let ng = genmach pop tours_per_gen
        in
            let dists = List.map dist_machine ng in
            let mini = list_min dists in
            if x mod 10 == 0 then printf "%d\n%!" mini else ();
            keep_on dist_machine genmach ng (x - 1)
let _ = 
    R.self_init ();
    let dm = init_proc in
    let ncities = Array.length dm in
    printf "Begin with %d\n%!" ncities;
    let dist_machine = tour_dist dm in
    let tours = make_many_tours ncities 15000 in
    let chooser = choose_one dm in
    let tourn_machine = tournament dist_machine k_tourn_entrants in
    let crosser = maybe_cross chooser tourn_machine in
    let gengen = gen_generation crosser tourn_machine (mutate dist_machine) in
    print_string "Go!\n";
    let b = keep_on dist_machine gengen tours generations in
    let best = argmin dist_machine b in
    let ot = get_opt_tour in
    let opt_dist = dist_machine ot in
    printf "best_test\n";
    printf "%d\n\n" opt_dist;
    List.map (fun x -> printf "%d\n" x) best;
