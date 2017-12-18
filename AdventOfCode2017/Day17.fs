module Day17

open System;
open System.IO;

let insert (p : int) (i : int) (is : int []) : int [] =
    if p >= is.Length then
        Array.append is [| i |]
    else
        is.[p..(is.Length-1)]
        |> Array.append [| i |]
        |> Array.append is.[0..(p-1)];

let rec spin (p : int) (s : int) (m : int) (i : int) (is : int []) : int [] =
    match i with
    | x when x = m -> insert (p+1) m is;
    | _ ->  let ls = insert (p+1) i is;
            spin ((p+1+s)%(i+1)) s m (i+1) ls;

let keyA (m : int) (t : int) (s : int) : int =
    let js = spin 0 s m 1 [| 0 |];
    let i = js |> Array.findIndex (fun x -> x = t)
    js.[(i+1)%m];

let rec quickSpin (p : int) (s : int) (m : int) (i : int) (ss : int list) : int =
    match i with
    | x when x = m -> if p = 0 then
                        m
                      else
                        ss |> List.head;
    | _ ->  let ss' =   if p = 0 then
                            i::ss;
                        else
                            ss;

            quickSpin ((p+1+s)%(i+1)) s m (i+1) ss'

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file)).[0]
                |> Int32.Parse;

    input
    |> keyA 2017 2017
    |> printfn "Day 17, part 1: %d";

    quickSpin 0 input 50000000 1 []
    |> printfn "Day 17, part 2: %d";
