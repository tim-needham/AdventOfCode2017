module Day19

open System;
open System.IO;

type Direction =
    | Up
    | Down
    | Left
    | Right

let next ((x, y) : int * int) (d : Direction) : int * int =
    match d with
    | Up -> (x, y-1);
    | Down -> (x, y+1);
    | Left -> (x-1, y);
    | Right -> (x+1, y);

let turn ((x, y) : int * int) (d : Direction) (ms : char list list) : Direction option =
    let ds =    match d with
                | Up | Down -> [ Left; Right ]
                | Left | Right -> [ Up; Down ];
                |> List.where (fun c -> let p, q = next (x, y) c;
                                        p >= 0 && p < ms.[0].Length && q >= 0 && q < ms.Length && ms.[q].[p] <> ' ';)

    match ds with
    | [] -> None
    | x::_ -> Some x;

let rec walk (s : int) ((x, y) : int * int) (d : Direction) (ls : char list) (ms : char list list) : int * char list =
    match ms.[y].[x] with
    | ' ' -> s, ls;
    | '-' | '|' -> walk (s+1) (next (x, y) d) d ls ms;
    | '+' ->    match turn (x, y) d ms with
                | None -> s, ls;
                | Some d' -> walk (s+1) (next (x, y) d') d' ls ms;
    | _ -> walk (s+1) (next (x, y) d) d (ms.[y].[x]::ls) ms;

let entry (ms : char list list) : int * int =
    (ms |> List.head
        |> List.findIndex (fun x -> x = '|'),
        0);

let route (ms : char list list) : int * string = 
    walk 0 (entry ms) Down [] ms
    |> (fun (a, b) -> (a,   b
                            |> List.rev
                            |> List.fold (fun a x -> a + x.ToString()) ""));

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (Seq.toList);

    let r = input
            |> route;

    r
    |> snd
    |> printfn "Day 19, part 1: %s";

    r
    |> fst
    |> printfn "Day 19, part 2: %d";
