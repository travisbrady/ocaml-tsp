open Printf
module R=Random

let tours_per_gen = 1200 
let generations = 100
let mut_prob = 0.9
let cross_prob = 0.03
let opt_file = "data/eil51.opt.tour"
let file = "data/eil51.tsp"
let num = Str.regexp "^[0-9]"
let space = Str.regexp " "
let list_min = List.fold_left min 1000000
let split_coords x = Str.split space x
let nint x = truncate(x +. 0.5);;
let dist (_, x1, y1) (_, x2, y2) = 
    nint(sqrt(((x1 -. x2)**2.0 +. (y1 -. y2)**2.0)))

(* Accepts list of 3 string and produces a 3-tuple *)
let make_city = function
      [num; x; y] -> Some (int_of_string num, float_of_string x, float_of_string y)
    | _ -> None

let de_option l = List.rev (List.fold_left (fun acc x ->
    match x with
        None -> acc
        | Some thing -> thing::acc) [] l)

let create_dist_map cities = List.map (fun x ->
    List.map (fun y -> dist x y) cities) cities

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
            List.tl cities
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

let make_tour ncities = Util.shuffle(Util.range 2 ncities)
let make_many_tours ncities ntours = List.map (fun x -> 
    make_tour ncities) (Util.range 1 ntours)

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

let random_of xs n = List.nth xs (R.int n)

let argmin dist_machine lst =
    let rec argmin' lst curr curr_dist = match lst with
        | [] -> curr
        | (x::xs) -> let this_dist = dist_machine x in
            if this_dist < curr_dist
                then argmin' xs x this_dist 
                else argmin' xs curr curr_dist
    in
    argmin' lst [] 1000000

let tournament dist_machine n tours = 
    let len = List.length tours in
    let contestants = List.map (fun x ->
        random_of tours len) (Util.range 0 n) in
    argmin dist_machine contestants

let choose_one dist_array city mom dad not_picked =
    let mom_next = get_next_city mom city in
    let dad_next = get_next_city dad city in
    let mom_dist = dist_array.(city - 1).(mom_next - 1) in
    let dad_dist = dist_array.(city - 1).(dad_next - 1) in
    match mom_dist <= dad_dist && List.mem mom_next not_picked with
          true -> mom_next
        | false -> match dad_dist <= mom_dist 
            && List.mem dad_next not_picked with
              true -> dad_next
            | false -> List.hd not_picked

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
              true -> two_opt _done
            | false -> tour
    in
    let mut_roll = R.float 1.0 in
    match mut_roll < mut_prob with
          true -> two_opt tour
        | false -> tour

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

let maybe_cross chooser tourn_machine tour pop =
    let cross_roll = R.float 1.0 in
    match cross_roll < cross_prob with
        | false -> tour
        | true -> let dad = tourn_machine pop in
            greedy_cross chooser tour dad tour []
let test_maybe_cross =
    let dm = init_proc in
    let dist_machine = tour_dist dm in
    let tours = make_many_tours 51 100 in
    let chooser = choose_one dm in
    let tourn_machine = tournament dist_machine 3 in
    let crosser = maybe_cross chooser tourn_machine in
    let t = List.hd tours in
    let winner = tourn_machine tours in
    mutate dist_machine winner

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

let rec keep_on dist_machine genmach pop generations =
    match generations with
        | 0 -> pop
        | x -> let ng = genmach pop tours_per_gen
        in
            let dists = List.map dist_machine ng in
            let mini = list_min dists in
            if x mod 5 == 0 then printf "%d\n%!" mini else ();
            keep_on dist_machine genmach ng (x - 1)
    
let _ = 
    R.self_init ();
    printf "%s\n!" "init_proc";
    flush stdout;;
    let dm = init_proc in
    let dist_machine = tour_dist dm in
    let tours = make_many_tours 51 15000 in
    let chooser = choose_one dm in
    let tourn_machine = tournament dist_machine 3 in
    let crosser = maybe_cross chooser tourn_machine in
    let gengen = gen_generation crosser tourn_machine (mutate dist_machine) in
    printf "%s\n!" "Go!";
    let b = keep_on dist_machine gengen tours generations in
    let best = argmin dist_machine b in
    let ot = get_opt_tour in
    let opt_dist = dist_machine ot in
    (*
    List.map (fun x -> printf "%d\n" x) blap;
    *)
    printf "%s\n!" "hi";
    printf "\n%d\n" opt_dist;

