module Day14

open System;
open System.IO;
open Day10;

let toBinary (c : char) : int list =
    let cs = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f' |];

    match cs |> Array.tryFindIndex (fun x -> x = c) with
    | None -> []
    | Some i ->  [3..-1..0]
                |> List.map (float)
                |> List.fold (fun (a, r) x -> if r >= Math.Pow(2.0, x) then (a@[1], r - Math.Pow(2.0, x)) else (a@[0], r)) ([], float i)
                |> fst;

let createDisk (k : string) : int [] [] =
    [|0..127|]
    |> Array.map (fun x ->   denseHash (k + "-" + (string x))
                            |> Seq.toList
                            |> List.fold (fun a y -> a@(toBinary y)) []
                            |> Seq.toArray);

let sumDisk (d : int [] []) : int =
    d |> Array.sumBy (fun x -> Array.sum x);

let rec coalesce (x : int) (y : int) (d : int [] []) : int * int [] [] =
    if x < 0 || x > 127 || y < 0 || y > 127 then
        (0, d)
    else if d.[x].[y] = 0 then
        (0, d)
    else
        (1, [| for i in 0..127 -> [| for j in 0..127 -> if i = x && j = y then 0 else d.[i].[j] |] |]
        |> coalesce (x+1) y |> snd
        |> coalesce (x-1) y |> snd 
        |> coalesce x (y+1) |> snd
        |> coalesce x (y-1) |> snd);

let groupDisk (d : int [] []) : int =
    [for i in 0..127 do
        for j in 0..127 -> (i, j)]
    |> List.fold (fun (i, d') (x, y) -> let (j, e) = coalesce x y d';
                                        (i+j, e)) (0, d)
    |> fst;

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file)).[0];

    let disk = createDisk input
    
    disk
    |> sumDisk
    |> printfn "Day 14, part 1: %d";
    
    disk
    |> groupDisk
    |> printfn "Day 14, part 2: %d";
