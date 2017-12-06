module Day6

open System;
open System.IO;

let rec redistribute (n : int) (p : int) (rs : int list) : int list =
    if p >= rs.Length then
        redistribute n 0 rs;
    else if n = 0 then
        rs
    else
        let rs' =   rs
                    |> List.mapi (fun i x -> if i = p then x+1 else x)
        redistribute (n-1) (p+1) rs';

let rec max (n : int) (p : int * int) (rs : int list) : int * int =
    if n >= rs.Length then
        p
    else
        let (_, b) = p
        let q = rs.[n]
   
        if q > b then
            max (n+1) (n, q) rs
        else
            max (n+1) p rs;

let zero (n : int) (rs : int list) : int list =
    rs
    |> List.mapi (fun i x -> if i = n then 0 else x);

let step (rs : int list) : int list =
    let (i, m) = max 0 (0, 0) rs;
    let rs' = zero i rs;
    redistribute m (i+1) rs';

let rec scan (n : int) (ps : (int * int list) list) (rs : int list) : int * int * (int list) =
    let rs' = step rs;
    let i = List.tryFindIndex (fun (_, x) -> x = rs') ps;

    match i with
    | Some m -> (n+1, fst ps.[m], rs')
    | None -> scan (n+1) ([(n, rs')]@ps) rs';

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file)).[0].Split([| '\t' |], StringSplitOptions.RemoveEmptyEntries)
                |> Seq.toList
                |> List.map (Int32.Parse);

    let (n, i, ts) = scan 0 [] input

    printfn "Day 6, part 1: %d" n;

    printfn "Day 6, part 2: %d" (n - i - 1);
