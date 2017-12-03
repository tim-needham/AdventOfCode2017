module Day3

open System;
open System.IO;

//
//  17  16  15  14  13
//  18   5   4   3  12
//  19   6   1   2  11
//  20   7   8   9  10
//  21  22  23---> ...
//
//  1 - Find which layer of the square we are in.
//      for any n, layer = maximum k : n <= (2k + 1) ^ 2
//  2 - Find how far we are from the centre of an edge in that square.
//  3 - Answer = layer + distance from centre

let rec layer (k : int) (n : int) : int =
    let p = pown (2*k + 1) 2;
    if n > p then
        layer (k+1) n;
    else
        k;

let centres (l : int) : int list =
    let s = pown (2*l + 1) 2;
    let e = pown (2*l - 1) 2;
    let c = (s - e) / 4;
    [for i in 1..4 -> e + i*c - c/2];

let distance (l : int) (n : int) : int =
    centres l
    |> List.map (fun c -> Math.Abs(n - c))
    |> List.min;

let manhattan (n : int) : int = 
    let l = layer 0 n;
    let d = distance l n;
    l + d;

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file)).[0]
                |> Int32.Parse;

    input
    |> manhattan
    |> printfn "Day 3, part 1: %d";

    2 
    |> printfn "Day 3, part 2: %d";