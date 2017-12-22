module Day22

open System;
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

let rec remove (i : int) (is : 'a list) : 'a list =
    let js, ks = is |> List.splitAt i;
    js @ (ks |> List.tail);

let rec update (i : int) (s : Status) (is : (int * int * Status) list) : (int * int * Status) list =
    is
    |> List.mapi (fun j (x, y, z) -> if i = j then (x, y, s) else (x, y, z));

let burst ((x, y) : int * int) (d : int * int) (is : (int * int * Status) list) : bool * (int * int) * (int * int) * ((int * int * Status) list) =
    match is |> List.tryFindIndex (fun (p, q, _) -> p = x && q = y) with
    | None ->   let (dx, dy) = turn d false;
                true, (x+dx, y+dy), (dx, dy), ((x, y, Infected)::is);
    | Some i -> let (dx, dy) = turn d true;
                false, (x+dx, y+dy), (dx, dy), (remove i is);

let burstEvolved ((x, y) : int * int) (d : int * int) (is : (int * int * Status) list) : bool * (int * int) * (int * int) * ((int * int * Status) list) =
    match is |> List.tryFindIndex (fun (p, q, _) -> p = x && q = y) with
    | None ->   let (dx, dy) = turn d false;
                false, (x+dx, y+dy), (dx, dy), ((x, y, Weakened)::is);
    | Some i -> match is.[i] with
                | x, y, Weakened -> let (dx, dy) = d;
                                    true, (x+dx, y+dy), d, (update i Infected is);
                | x, y, Infected -> let (dx, dy) = turn d true;
                                    false, (x+dx, y+dy), (dx, dy), (update i Flagged is);
                | x, y, Flagged ->  let (dx, dy) = d;
                                    false, (x-dx, y-dy), (-dx, -dy), (remove i is);
                
let rec spread (n : int) (c : int) (p : int * int) (d : int * int) (is : (int * int * Status) list) : int * ((int * int * Status) list) =
    match n with
    | 0 -> c, is;
    | _ ->  let i, p', d', is' = burst p d is;
            let c' = if i then c + 1 else c;
            spread (n-1) c' p' d' is';

let rec spreadEvolved (n : int) (c : int) (p : int * int) (d : int * int) (is : (int * int * Status) list) : int * ((int * int * Status) list) =
    match n with
    | 0 -> c, is;
    | _ ->  let i, p', d', is' = burstEvolved p d is;
            let c' = if i then c + 1 else c;
            spreadEvolved (n-1) c' p' d' is';

let centre (g : 'a list list) : int * int =
    (g.Length - 1) / 2, (g.[0].Length - 1) / 2

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (Seq.toList);

    let ts = [ "..#"; "#.."; "..." ]
                |> List.map (Seq.toList);
    
    let p = centre input;

    [for j in 0..input.Length-1 do
        for i in 0..input.[j].Length-1 ->
            (i, j, input.[j].[i])
    ]
    |> List.where (fun (_, _, x) -> x = '#')
    |> List.map (fun (x, y, _) -> x, y, Infected)
    |> spread 10000 0 p (0, -1)
    |> fst
    |> printfn "Day 22, part 1: %d";

    [for j in 0..input.Length-1 do
        for i in 0..input.[j].Length-1 ->
            (i, j, input.[j].[i])
    ]
    |> List.where (fun (_, _, x) -> x = '#')
    |> List.map (fun (x, y, _) -> x, y, Infected)
    |> spreadEvolved 10000000 0 p (0, -1)
    |> fst
    |> printfn "Day 22, part 2: %d";
