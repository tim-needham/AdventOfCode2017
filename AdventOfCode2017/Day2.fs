module Day2

open System;
open System.IO;

let total (is : int list) : int =
    match is with
    | [] -> 0
    | xs -> let a, b = List.min xs, List.max xs
            b - a;

let rec divide (is : (int * int) list) : int =
    match is with
    | [] -> 0
    | (x, y)::xs -> if x % y = 0 then
                        x / y;
                    else
                        divide xs;

let divisors (is : int list) : int =
    match is with
    | [] -> 0
    | xs -> [for i in 1..is.Length -> 
                [for j in (i+1)..is.Length -> 
                    let a, b = is.[i-1], is.[j-1]
                    if a > b then (a, b) else (b, a)
                ]
            ]
            |> List.fold (fun a x -> a + (divide x)) 0;

let rec checksum (s : int list -> int) (is : int list list) : int =
    match is with
    | [] -> 0
    | x::xs -> (s x) + (checksum s xs);

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (fun x -> x.Split([| '\t' |]) |> Seq.toList)
                |> List.map (List.map (Int32.Parse));

    input
    |> checksum total
    |> printfn "Day 2, part 1: %d";

    input
    |> checksum divisors
    |> printfn "Day 2, part 2: %d";