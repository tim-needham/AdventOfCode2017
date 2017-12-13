module Day13

open System;
open System.IO;

type Layer = {
    Depth : int;
    Sweep : int;
    Range : int
};;

let parse (s : string) : Layer =
    match s.Split([| ": " |], StringSplitOptions.RemoveEmptyEntries) with
    | [| x; y |] -> { Depth = Int32.Parse x; Sweep = 2 * (Int32.Parse y - 1); Range = Int32.Parse y }

let caught (d : int) (l : Layer) : bool =
    (d + l.Depth) % l.Sweep = 0;

let cost (d : int) (l : Layer) : int =
    match caught d l with
    | true -> (d + l.Depth) * l.Range;
    | false -> 0;

let safe (d : int) (ls : Layer list) : bool =
    ls
    |> List.forall (fun l -> not (caught d l))

let rec safePath (d : int) (ls : Layer list) : int = 
    match safe d ls with
    | true -> d;
    | false -> safePath (d + 1) ls;

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (parse);

    input
    |> List.sumBy (cost 0)
    |> printfn "Day 13, part 1: %d";

    input
    |> safePath 0
    |> printfn "Day 13, part 2: %d";
