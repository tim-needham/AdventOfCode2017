module Day10

open System;
open System.IO;

let twist (p : int) (is : int list) (l : int) : int list =
    if l > is.Length then
        is;
    else if l = 1 then
        is;
    else
        let js, ks = List.splitAt p is;
        let ls, ms = List.splitAt l (ks@js);
        let ls' = List.rev ls;
        let js', ms' = List.splitAt (is.Length - p) (ls'@ms)
        ms'@js';

let rec twister (p : int) (s : int) (is : int list) (ls : int list) : int * int * int list =
    match ls with
    | [] -> (p, s, is);
    | x::xs ->  let js = twist p is x;
                let p' = (p+x+s) % is.Length;
                twister p' (s+1) js xs;

let hash (ls : int list) : int =
    let (p, s, is) = twister 0 0 [0..255] ls;
    let a = List.head is;
    let b = List.tail is |> List.head;
    a * b           

let rec toAscii (cs : char list) : int list =
    match cs with
    | [] -> [17; 31; 73; 47; 23]
    | x::xs ->  (int x)::(toAscii xs);

let rec sparseHash (n : int) (p : int) (s : int) (is : int list) (ls : int list) : int * int * int list =
    if n = 0 then
        (p, s, is);
    else
        let (p', s', is') = twister p s is ls;
        sparseHash (n-1) p' s' is' ls;

let rec chunk (s : int) (is : int list) : int list list =
    if is.Length > s then
        let js, ks = List.splitAt s is;
        js::(chunk s ks);
    else
        [is];

let toHex (n : int) : string =
    let hs = ["0";"1";"2";"3";"4";"5";"6";"7";"8";"9";"a";"b";"c";"d";"e";"f"];
    hs.[n/16] + hs.[n%16];

let denseHash (ss : string) : string =
    let ls = ss
            |> Seq.toList
            |> toAscii;
    let (p, s, is) = sparseHash 64 0 0 [0..255] ls;

    is
    |> chunk 16
    |> List.map (fun x -> List.fold (^^^) 0 x)
    |> List.map (toHex)
    |> List.fold (+) "";

let run (file : string) =

    let input = (Seq.toList(File.ReadLines(file)).[0]);

    input.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.toList
    |> List.map (Int32.Parse)
    |> hash
    |> printfn "Day 10, part 1: %d";

    input
    |> denseHash
    |> printfn "Day 10, part 2: %A";
