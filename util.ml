open Printf

let take n l = 
    let rec _take n l acc = match (n, l) with
        (0,_) -> acc
        | (_,[]) -> acc
        | (nx,(x::xs)) -> _take (n - 1) xs (x::acc) in
    List.rev (_take n l [])

let rec drop n = function
    | _ :: l when n > 0 -> drop (n-1) l
    | l -> l

let split_at n lst =  
    let rec _split_at n l acc1 acc2 =
        match l with
            [] -> (List.rev acc1, List.rev acc2)
            | (x::xs) when n > 0 -> _split_at (n-1) xs (x::acc1) acc2
            | (x::xs) -> _split_at (n-1) xs acc1 (x::acc2)
        in
    _split_at n lst [] []

let shuffle l = 
    let rec shuffle' l acc = match l with
        [] -> acc
        | l -> let k = Random.int (List.length l) in
               let (lead, (x::xs)) = split_at k l in
               shuffle' (List.rev_append lead xs) (x::acc)
        in
    shuffle' l []

let b = [1;2;3;4;5;6;7;8;9;10;11]
let c = split_at 4 b

let rec range i j = if i > j then [] else i :: (range (i+1) j)

(* How is there no zip? *)
let rec zip xs ys = 
    match (xs, ys) with
    | [],_ -> []
    | _,[] -> []
    |x::xs, y::ys -> (x,y)::(zip xs ys)

(* Find the index of the min *)
let indmin xs = 
    let rec indmin' xs currmin currind i = match xs with
        [] -> currind
        | (x::xs) when x < currmin -> indmin' xs x i (i+1)
        | (x::xs) -> indmin' xs currmin currind (i+1)
    in
    indmin' xs 1000000 0 0

let rec delete_by eq x lst = match lst with
    | [] -> []
    | (y::ys) -> if eq x y then ys else y :: delete_by eq x ys

let delete x lst = delete_by (==) x lst
