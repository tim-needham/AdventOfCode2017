module Day1

open System;
open System.IO;

let equals (x : int) (y : int) : int = 
    if x = y then
        x;
    else
        0;

let rec cyclesum (n : int) (o : int) (is : int list) : int = 
    let l = List.length is;
    
    if n >= l then
        0;
    else
        let p = (n + o) % l;
        (equals is.[n] is.[p]) + (cyclesum (n+1) o is);
    

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file)).[0]
                |> Seq.toList
                |> List.map (fun x -> Int32.Parse(x.ToString()));

    input
    |> cyclesum 0 1 
    |> printfn "Day 1, part 1: %d";

    input
    |> cyclesum 0 ((List.length input) / 2)
    |> printfn "Day 1, part 2: %d";