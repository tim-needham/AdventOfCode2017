module Day22

open System;
open System.Collections.Generic;
open System.IO;

type Status = 
    | Clean
    | Weakened
    | Infected
    | Flagged;;

let turn (d : int * int) (i : bool) : int * int =
    match d with
    | (1, 0) -> if i then (0, 1) else (0, -1);
    | (-1, 0) -> if i then (0, -1) else (0, 1);
    | (0, 1) -> if i then (-1, 0) else (1, 0);
    | (0, -1) -> if i then (1, 0) else (-1, 0);

let burst ((x, y) : int * int) (d : int * int) (is : IDictionary<int * int, Status>) : bool * (int * int) * (int * int) * IDictionary<int * int, Status> =
    match is.ContainsKey(x, y) with
    | false ->  let (dx, dy) = turn d false;
                is.Add((x, y), Infected);
                true, (x+dx, y+dy), (dx, dy), is;
    | true ->   let (dx, dy) = turn d true;
                is.Remove((x, y));
                false, (x+dx, y+dy), (dx, dy), is;

let burstEvolved ((x, y) : int * int) (d : int * int) (is : IDictionary<int * int, Status>) : bool * (int * int) * (int * int) =
    match is.ContainsKey(x, y) with
    | false ->  let (dx, dy) = turn d false;
                is.Add((x, y), Weakened);
                false, (x+dx, y+dy), (dx, dy);
    | true ->   match is.[(x, y)] with
                | Weakened ->   let (dx, dy) = d;
                                is.[(x, y)] <- Infected;
                                true, (x+dx, y+dy), d;
                | Infected ->   let (dx, dy) = turn d true;
                                is.[(x, y)] <- Flagged;
                                false, (x+dx, y+dy), (dx, dy);
                | Flagged ->    let (dx, dy) = d;
                                is.Remove((x, y));
                                false, (x-dx, y-dy), (-dx, -dy);
                
let rec spread (n : int) (c : int) (p : int * int) (d : int * int) (is : IDictionary<int * int, Status>) : int =
    match n with
    | 0 -> c;
    | _ ->  let i, p', d', is' = burst p d is;
            let c' = if i then c + 1 else c;
            spread (n-1) c' p' d' is';

let rec spreadEvolved (n : int) (c : int) (p : int * int) (d : int * int) (is : IDictionary<int * int, Status>) : int =
    match n with
    | 0 -> c;
    | _ ->  let i, p', d' = burstEvolved p d is;
            let c' = if i then c + 1 else c;
            spreadEvolved (n-1) c' p' d' is;

let centre (g : 'a list list) : int * int =
    (g.Length - 1) / 2, (g.[0].Length - 1) / 2

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (Seq.toList);

    let p = centre input;

    let map =   [for j in 0..input.Length-1 do
                    for i in 0..input.[j].Length-1 ->
                        (i, j, input.[j].[i])
                ]
                |> List.where (fun (_, _, x) -> x = '#')
                |> List.map (fun (x, y, _) -> ((x, y), Infected))
                |> Map.ofList;
                
    new Dictionary<int*int, Status>(map)
    |> spread 10000 0 p (0, -1)
    |> printfn "Day 22, part 1: %d";

    new Dictionary<int*int, Status>(map)
    |> spreadEvolved 10000000 0 p (0, -1)
    |> printfn "Day 22, part 2: %d";
