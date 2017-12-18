module Day17

open System;
open System.IO;

let rec spin (p : int) (s : int) (m : int) (i : int) (is : int list) : int list =
    match i with
    | x when x = m ->   let js, ks = is |> List.splitAt (p+1);
                        js@[m]@ks;
    | _ ->  //if i%1000 = 0 then
            //    printfn "%d" i;
            let js, ks = is |> List.splitAt (p+1);
            let ls = i::(ks@js);
            spin (s%(i+1)) s m (i+1) ls;

let key (m : int) (t : int) (s : int) : int =
    let js = spin 0 s m 1 [0];
    let i = js |> List.findIndex (fun x -> x = t)
    js.[(i+1)%m];

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file)).[0]
                |> Int32.Parse;

    input
    |> key 2017 2017
    |> printfn "Day 17, part 1: %d";

    input
    |> key 50000000 0
    |> printfn "Day 17, part 2: %d";
