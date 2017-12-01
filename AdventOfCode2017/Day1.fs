module Day1

open System;
open System.IO;

let equals (x : int) (y : int) : int = 
    if x = y then
        x;
    else
        0;

let rec sum (is : (int * int) list) : int =
    match is with
    | [] -> 0
    | (x, y)::xs -> (equals x y) + sum xs;

let neighbours (is : int list) : (int * int) list =
    match is with
    | [] -> []
    | [x] -> []
    | x::xs ->  is @ [x]
                |> List.pairwise;

let opposites (is : int list) : (int * int) list =
    match is with
    | [] -> []
    | [x] -> []
    | xs when (List.length xs) % 2 = 1 -> []
    | x::xs ->  let l = List.length is;
                let ps = List.take (l / 2) is;
                let qs = List.skip (l / 2) is;
                List.zip is (qs @ ps);

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file)).[0]
                |> Seq.toList
                |> List.map (fun x -> Int32.Parse(x.ToString()));

    input
    |> neighbours
    |> sum
    |> printfn "Day 1, part 1: %d";

    input
    |> opposites
    |> sum
    |> printfn "Day 1, part 2: %d";