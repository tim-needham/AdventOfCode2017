module Day11

open System;
open System.IO;

type Direction = 
    | N
    | NE
    | NW
    | S
    | SE
    | SW;;

let parse (s : string) : Direction =
    match s with
    | "n" -> N;
    | "ne" -> NE;
    | "nw" -> NW;
    | "s" -> S;
    | "se" -> SE;
    | "sw" -> SW;

let move (c : double * double) (d : Direction) : double * double =
    let (x, y) = c;
    match d with
    | N -> (x, y+1.0);
    | NE -> (x+0.5, y+0.5);
    | NW -> (x-0.5, y+0.5);
    | S -> (x, y-1.0);
    | SE -> (x+0.5, y-0.5);
    | SW -> (x-0.5, y-0.5);

let distance (c : double * double) : int =
    let (x, y) = c;
    int ((Math.Abs x) + (Math.Abs y));

let walk (c : double * double) (ds : Direction list) : (double * double) * int =
    ds
    |> List.fold (fun (p, d) x ->   let p' = move p x;
                                    let d' = Math.Max (d, distance p');
                                    (p', d')) (c, 0);

let run (file : string) =

    let input = (Seq.toList(File.ReadLines(file)).[0]).Split([| ',' |], StringSplitOptions.RemoveEmptyEntries) 
                |> Seq.toList 
                |> List.map(parse);

    let (c, d) = input
                |> walk (0.0, 0.0);

    c
    |> distance
    |> printfn "Day 11, part 1: %d";

    d
    |> printfn "Day 11, part 2: %d";
