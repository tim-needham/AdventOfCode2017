module Day5

open System;
open System.IO;

let incr (x : int) : int =
    x + 1;

let split3 (x : int) : int =
    if x >= 3 then
        x - 1;
    else
        x + 1;

let rec oper (n : int) (f : int -> int) (is : int list) (js : int list) : int * (int list) * (int list) =
    match js with
    | [] -> (n, is, js)
    | x::xs ->  let n' = n+1;
                let x' = f x;
                if x = 0 then
                    oper n' f is (x'::xs);
                else if x > 0 then
                    let ls, ms = List.take (x-1) xs, List.skip (x-1) xs
                    oper n' f (is@[x']@ls) ms;
                else
                    let l = is.Length + x;
                    let ls, ms = List.take l is, List.skip l is
                    oper n' f ls (ms@[x']@xs);

let jumps (f : int -> int) (is : int list) : int =
    match is with
    | [] -> 0
    | xs -> let (n, _, _) = oper 0 f [] xs;
            n;

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (Int32.Parse);

    input
    |> jumps incr
    |> printfn "Day 5, part 1: %d";

    input
    |> jumps split3
    |> printfn "Day 5, part 2: %d";
