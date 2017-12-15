module Day15

open System;
open System.IO;

let parse (s : string) : int64 =
    match s.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) with
    | [| "Generator"; _; "starts"; "with"; x |] -> Int64.Parse x;

let rec toBinary (i : int) : string =
    match i with
    | 0 | 1 -> string i;
    | _ ->  let b = string (i%2);
            (toBinary (i/2)) + b;

let generate (f : int64) (i : int64) : int64 =
    (i * f) % 2147483647L;

let rec fussyGenerate (f : int64) (i : int64) (d : int64) : int64 =
    let o = (i * f) % 2147483647L;
    if o % d = 0L then
        o
    else
        fussyGenerate f o d;

let generator (s : int64) (f : int64) (c : int64) : seq<int64> =
    Seq.unfold (fun (x, i) ->   if i > c then
                                    None
                                else
                                    Some ((x, i), (generate f x, i+1L))) (s, 0L)
    |> Seq.map (fst);

let fussyGenerator (s : int64) (f : int64) (d : int64) (c : int64) : seq<int64> =
    Seq.unfold (fun (x, i) ->   if i > c then
                                    None
                                else
                                    Some ((x, i), (fussyGenerate f x d, i+1L))) (s, 0L)
    |> Seq.map (fst)

let judge (n : int64) (a : int64 -> seq<int64>) (b : int64 -> seq<int64>) : int =
    let gA = a n;
    let gB = b n;

    Seq.zip gA gB
    |> Seq.fold (fun t (x, y) ->    let p = toBinary (int (x%65536L));
                                    let q = toBinary (int (y%65536L));
                                    if p=q then t+1 else t) 0;


let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (parse);

    let a, b = input.[0], input.[1]

    judge 40000000L (generator a 16807L) (generator b 48271L)
    |> printfn "Day 15, part 1: %d";

    judge 5000000L (fussyGenerator a 16807L 4L) (fussyGenerator b 48271L 8L)
    |> printfn "Day 15, part 2: %d";
