open Printf
module R=Random

let mut_prob = 0.5
let opt_file = "data/eil51.opt.tour"
let file = "data/eil51.tsp"
let num = Str.regexp "^[0-9]"
let space = Str.regexp " "
let sum = List.fold_left (+) 0

let split_coords x = Str.split space x

let nint x = truncate(x +. 0.5);;

let dist (_, x1, y1) (_, x2, y2) = 
    nint(sqrt(((x1 -. x2)**2.0 +. (y1 -. y2)**2.0)))

(* Accepts list of 3 string and produces a 3-tuple *)
let make_city = function
      [num; x; y] -> Some (int_of_string num, float_of_string x, float_of_string y)
    | _ -> None
;;

let de_option l = List.rev (List.fold_left (fun acc x ->
    match x with
        None -> acc
        | Some thing -> thing::acc) [] l);;

let create_dist_map cities = List.map (fun x ->
    List.map (fun y -> dist x y) cities) cities
;;

let make_dist_array cities = Array.of_list 
    (List.map Array.of_list (create_dist_map cities))

(* Lookup distance from one city to another *)
let get_dist num1 num2 da = let x = num1 - 1 in
    let y = num2 - 1 in
    da.(x).(y)

let tour_dist da tour =
    let rec tour_dist' tour total = match tour with
        [] -> total
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
            List.append cities [1]
    with e ->
        close_in_noerr ic;
        raise e

let init_proc =
    let lines = proc_file in
    let coords = List.filter (fun x -> 
        Str.string_match num x 0) lines in
    let hey = List.map split_coords coords in
    let cities = de_option(List.map make_city hey) in
    make_dist_array cities

let rec two_opt dist_machine tour = 
    let tour_len = (List.length tour + 1) in
    let ll = [R.int tour_len; R.int tour_len] in
    let [si; ei] = List.sort compare ll in
    let _start = Util.take si tour in
    let mid = List.rev (Util.take (ei - si) (Util.drop si tour)) in
    let _end = Util.drop ei tour in
    let _done = List.concat [_start; mid; _end] in
    match (dist_machine _done ) < (dist_machine tour) with
        true -> two_opt dist_machine _done
        | false -> tour

let make_tour ncities = Util.shuffle(Util.range 2 ncities)

let make_many_tours ncities ntours = List.map (fun x -> 
    make_tour ncities) (Util.range 1 ntours)

let get_next_city tour city = 
    let ind = List.find (fun x -> x==city) tour in
    List.nth tour ind

let list_max = List.fold_left max 0
let list_min = List.fold_left min 1000000

let random_of xs n = List.nth xs (R.int n)

let tournament dist_machine n tours = 
    let len = List.length tours in
    let contestants = List.map (fun x ->
        random_of tours len) (Util.range 0 n) in
    let dists = List.map dist_machine contestants in
    List.nth tours (Util.indmin dists)

let choose_one dist_array city mom dad not_picked =
    let mom_next = get_next_city mom city in
    let dad_next = get_next_city dad city in
    let mom_dist = dist_array.(city).(mom_next) in
    let dad_dist = dist_array.(city).(dad_next) in
    match mom_dist <= dad_dist && List.mem mom_next not_picked with
          true -> mom_next
        | false -> match dad_dist <= mom_dist 
            && List.mem dad_next not_picked with
              true -> dad_next
            | false -> List.hd not_picked

let rec greedy_cross chooser mom dad not_picked acc =
    match not_picked with
        | [] -> acc
        | _ ->
        let city = match acc with
            | [] -> 1
            | (x::xs) -> x
        in
        let the_chosen = chooser city mom dad not_picked in
            greedy_cross chooser mom dad (Util.delete the_chosen not_picked) (the_chosen::acc)

let mutate mutator tour = 
    let mut_roll = R.float 1.0 in
    match mut_roll < mut_prob with
          true -> mutator tour
        | false -> tour

let _ = 

    let dm = init_proc in
    let dist_machine = tour_dist dm in
    let opt_tour = get_opt_tour in
    let otd = tour_dist dm opt_tour in 
    printf "%i\n" otd;
    let tour = make_tour 51 in
    let tours = make_many_tours 51 100 in
    printf "%i\n" (List.length tours);
    printf "%i\n" (get_next_city tour 7);
    let tour_dists = List.map (tour_dist dm) tours in
    printf "%i\n" (List.length tour_dists);
    (* let minny = list_min tour_dists in *)
    let winner = tournament dist_machine 3 tours in
    let chooser = choose_one dm in
    let tourn_machine = tournament dist_machine 3 in
    printf "%s\n" "hi";


