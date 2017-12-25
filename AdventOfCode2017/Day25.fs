module Day25

open System;
open System.IO;

type State =
    | A
    | B
    | C
    | D
    | E
    | F;;

let update (c : int) (v : int) (ts : int []) : int [] =
    ts.[c] <- v;
    ts;
let test (ts : int []) (c : int) (s : State) : int [] * int * State =
    match s with
    | A ->  if ts.[c] = 0 then
                update c 1 ts |> ignore;
                ts, c+1, B;
            else
                update c 0 ts |> ignore;
                ts, c-1, B;
    | B ->  if ts.[c] = 0 then
                update c 1 ts |> ignore;
                ts, c-1, A;
            else
                update c 1 ts |> ignore;
                ts, c+1, A;

let exec (ts : int []) (c : int) (s : State) : int [] * int * State =
    match s with
    | A ->  if ts.[c] = 0 then
                update c 1 ts |> ignore;
                ts, c+1, B;
            else
                update c 0 ts |> ignore;
                ts, c-1, F;
    | B ->  if ts.[c] = 0 then
                ts, c+1, C;
            else
                update c 0 ts |> ignore;
                ts, c+1, D;
    | C ->  if ts.[c] = 0 then
                update c 1 ts |> ignore;
                ts, c-1, D;
            else
                ts, c+1, E;
    | D ->  if ts.[c] = 0 then
                ts, c-1, E;
            else
                update c 0 ts |> ignore;
                ts, c-1, D;
    | E ->  if ts.[c] = 0 then
                ts, c+1, A;
            else
                ts, c+1, C;
    | F ->  if ts.[c] = 0 then
                update c 1 ts |> ignore;
                ts, c-1, A;
            else
                ts, c+1, A;

let rec simulate (n : int) (ts : int []) (c : int) (s : State) : int [] =
    match n with
    | 0 -> ts;
    | _ ->  let ts', c', s' = exec ts c s;
            simulate (n-1) ts' c' s';

let run (file : string) =

    //let input = Seq.toList(File.ReadLines(file));

    let mutable ts = Array.replicate 12000 0;

    simulate 12994925 ts 6000 A
    |> Array.sum
    |> printfn "Day 25, part 1: %d";