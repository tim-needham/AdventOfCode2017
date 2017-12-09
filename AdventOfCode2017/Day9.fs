module Day9

open System;
open System.IO;

type Token = 
    | None
    | Valid of Token list
    | Garbage of int;;


let rec parse (ts : Token list) (t : Token) (l : int) (cs : char list) : Token list * int * char list =
    match cs with
    | [] -> (ts, 0, []);
    | '!'::xs ->    match t with
                    | Garbage _ ->  let ys = List.tail xs;
                                    parse ts t l ys;
                    | _ -> parse ts t 0 xs;
    | '{'::xs ->    match t with
                    | Garbage  _-> parse ts t (l+1) xs;
                    | _ ->  let (ts', l', cs') = parse [] (Valid []) 0 xs;
                            parse (ts@[Valid (ts')]) t 0 cs';
    | '}'::xs ->    match t with
                    | Garbage _ -> parse ts t (l+1) xs; 
                    | _ -> (ts, 0, xs);
    | '<'::xs ->    match t with
                    | Garbage _ -> parse ts t (l+1) xs;
                    | _ ->  let (_, l', cs') = parse [] (Garbage 0) 0 xs;
                            parse (ts@[(Garbage l')]) t 0 cs';
    | '>'::xs ->    (ts, l, xs);
    | _::xs ->      parse ts t (l+1) xs;

let scan (s : string) : Token =
    let s' = Seq.toList s;
    let (r, _, _) = parse [] None 0 s';
    List.head r;

let rec score (n : int) (t : Token) : int =
    match t with
    | None -> 0;
    | Garbage _ -> 0;
    | Valid ts -> n + (List.fold (fun a x -> a + (score (n+1) x)) 0 ts); 

let rec trash (t : Token) : int =
    match t with
    | None -> 0;
    | Garbage x -> x;
    | Valid ts -> List.fold (fun a x -> a + trash x) 0 ts;

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file)).[0]
                |> scan;

    input
    |> score 1
    |> printfn "Day 9, part 1: %d";

    input
    |> trash
    |> printfn "Day 9, part 2: %d";
