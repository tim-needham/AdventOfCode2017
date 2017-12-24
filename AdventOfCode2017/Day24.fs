module Day24

open System;
open System.IO;

let parse (s : string) : int * int =
    match s.Split([| '/' |], StringSplitOptions.None) with
    | [| a; b |] -> Int32.Parse a, Int32.Parse b;

let candidates (n : int) (ps : (int * int) list) : ((int * int) * ((int * int) list)) list =
    [for i in 0..ps.Length-1 do
        let (x, y) = ps.[i];
        if x = n then
            let qs, rs = List.splitAt i ps;
            yield ((x, y), qs@(rs |> List.tail));
        else if y = n then
            let qs, rs = List.splitAt i ps;
            yield ((y, x), qs@(rs |> List.tail));
    ];

let rec combine (n : int) (ps : (int * int) list) : (int * int) list list =
    match ps with
    | [] -> [[]];
    | xs -> let cs = candidates n xs
                     |> List.collect (fun ((x, y), zs) -> [ for z in (combine y zs) -> (x, y)::z ]);
            []::cs;

let bridge (ps : (int * int) list) : (int * int) list list =
    combine 0 ps
    |> List.where (fun x -> x <> [])
    |> List.where (fun x -> (x
                            |> List.rev
                            |> List.head
                            |> snd) <> 0);

let rec strength (ps : (int * int) list) : int =
    match ps with
    | [] -> 0;
    | (x, y)::xs -> x + y + (strength xs);

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (parse);

    let bs = input
             |> bridge
             |> List.map (fun x -> x.Length, strength x)

    bs
    |> List.sortByDescending (snd)
    |> List.head
    |> (snd)
    |> printfn "Day 24, part 1: %d";

    bs
    |> List.sortByDescending (fun (x, y) -> x, y)
    |> List.head
    |> (snd)
    |> printfn "Day 24, part 2: %d";
