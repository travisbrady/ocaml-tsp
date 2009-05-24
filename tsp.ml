open Printf

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
let make_city row = match row with
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

let tour_dist da tour = sum (List.map (fun (x, y) ->
    get_dist x y da) (Util.zip tour (List.tl tour)))

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

let make_tour ncities = 
    let cr = Util.shuffle(Util.range 2 ncities) in
    List.append (1::cr) [1]

let make_many_tours ncities ntours = List.map (fun x -> 
    make_tour ncities) (Util.range 1 ntours)

let get_next_city tour city = 
    let ind = List.find (fun x -> x==city) tour in
    List.nth tour ind

let list_max = List.fold_left max 0
let list_min = List.fold_left min 1000000

let random_of xs n = List.nth xs (Random.int n)

(* TODO: DO THIS *)
let tournament dist_map n tours = 0


let _ = 

    let dm = init_proc in
    let opt_tour = get_opt_tour in
    printf "%s\n" "hi";
    let otd = tour_dist dm opt_tour in 
    printf "%i\n" otd;
    let tour = make_tour 51 in
    let tours = make_many_tours 51 100 in
    printf "%i\n" (List.length tours);
    printf "%i\n" (get_next_city tour 1);
    let tour_dists = List.map (tour_dist dm) tours in
    printf "%i\n" (List.length tour_dists);
    let minny = list_min tour_dists in
    printf "%i\n" minny;
