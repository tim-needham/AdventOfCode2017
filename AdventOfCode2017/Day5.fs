module Day5

open System;
open System.IO;

let rec oper (n : int) (p : int) (is : int list) : int * int * (int list) =
    if p >= is.Length then
        (n, p, is)
    else
        let js = List.take p is;
        let ks = List.skip p is;
        let l = List.head ks;
        let ls = List.tail ks;
        oper (n+1) (p + l) (js @ [l+1] @ ls)

let rec oper2 (n : int) (p : int) (is : int list) : int * int * (int list) =
    if p >= is.Length then
        (n, p, is)
    else
        let js = List.take p is;
        let ks = List.skip p is;
        let l = List.head ks;
        let ls = List.tail ks;
        let l' = if l >= 3 then l-1 else l+1
        oper2 (n+1) (p + l) (js @ [l'] @ ls)

let jumps (is : int list) : int =
    match is with
    | [] -> 0
    | xs -> let (n, p, q) = oper 0 0 is;
            n;

let jumps2 (is : int list) : int =
    match is with
    | [] -> 0
    | xs -> let (n, p, q) = oper2 0 0 is;
            n;

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (Int32.Parse);

    input
    |> jumps
    |> printfn "Day 5, part 1: %d";

    input
    |> jumps2
    |> printfn "Day 5, part 2: %d";
