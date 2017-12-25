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

let update (c : int) (v : int) (ts : int list) : int list =
    ts
    |> List.mapi (fun i x -> if i=c then v else x);

let move (c : int) (ts : int list) (i : int) : int list * int =
    if i = 1 then
        if c >= ts.Length - 1 then ts@[0], c+1 else ts, c+1;
    else
        if c = 0 then 0::ts, 0 else ts, c-1; 

let test (ts : int list) (c : int) (s : State) : int list * int * State =
    match s with
    | A ->  let ts', c' =   if ts.[c] = 0 then
                                move c (update c 1 ts) 1;
                            else
                                move c (update c 0 ts) -1;
            ts', c', B;
    | B ->  let ts', c' =   if ts.[c] = 0 then
                                move c (update c 1 ts) -1;
                            else
                                move c (update c 1 ts) 1;
            ts', c', A;

let exec (ts : int list) (c : int) (s : State) : int list * int * State =
    match s with
    | A ->  if ts.[c] = 0 then
                let ts', c' = move c (update c 1 ts) 1;
                ts', c', B;
            else
                let ts', c' = move c (update c 0 ts) -1;
                ts', c', F;
    | B ->  if ts.[c] = 0 then
                let ts', c' = move c ts 1;
                ts', c', C;
            else
                let ts', c' = move c (update c 0 ts) 1;
                ts', c', D;
    | C ->  if ts.[c] = 0 then
                let ts', c' = move c (update c 1 ts) -1;
                ts', c', D;
            else
                let ts', c' = move c ts 1;
                ts', c', E;
    | D ->  if ts.[c] = 0 then
                let ts', c' = move c ts -1;
                ts', c', E;
            else
                let ts', c' = move c (update c 0 ts) -1;
                ts', c', D;
    | E ->  if ts.[c] = 0 then
                let ts', c' = move c ts 1;
                ts', c', A;
            else
                let ts', c' = move c ts 1;
                ts', c', C;
    | F ->  if ts.[c] = 0 then
                let ts', c' = move c (update c 1 ts) -1;
                ts', c', A;
            else
                let ts', c' = move c ts 1;
                ts', c', A;

let rec simulate (n : int) (ts : int list) (c : int) (s : State) : int list * int * State =
    match n with
    | 0 -> ts, c, s;
    | _ ->  let ts', c', s' = exec ts c s;
            simulate (n-1) ts' c' s';

let run (file : string) =

    simulate 12994925 0 [0] A
    |> List.sum
    |> printfn "Day 25, part 1: %d";