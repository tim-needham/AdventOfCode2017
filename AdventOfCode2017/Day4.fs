module Day4

open System;
open System.IO;

let eq (x : string) (y : string) : bool =
    x = y;

let anagram (x : string) (y : string) : bool =
    if x.Length <> y.Length then
        false;
    else
        let p = x |> Seq.toList |> List.sort;
        let q = y |> Seq.toList |> List.sort;
        List.zip p q
        |> List.map (fun (a, b) -> a = b)
        |> List.fold (&&) true;

let rec validate (ss : string list) (ts : string list) (f : string -> string -> bool) : int =
    match ts with
    | [] -> 1
    | x::xs ->  match List.tryFindIndex (fun y -> f x y) ss with
                | Some _ -> 0
                | None -> validate (ss@[x]) xs f;

let rec scan (f : string -> string -> bool) (ss : string list list) : int =
    match ss with
    | [] -> 0
    | x::xs -> (validate [] x f) + (scan f xs);

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (fun x -> x.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) |> Seq.toList)

    input
    |> scan eq
    |> printfn "Day 4, part 1: %d";

    input
    |> scan anagram
    |> printfn "Day 4, part 2: %d";
