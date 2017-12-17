module Day16

open System;
open System.IO;

let initials = [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p' ];

type Move = 
    | Spin of int
    | Exchange of int * int
    | Partner of char * char

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some (s.Substring(p.Length));
    else
        None;

let parse (s : string) : Move =
    match s with
    | Prefix "s" ss -> Spin (ss.ToString() |> Int32.Parse);
    | Prefix "x" xs ->  match xs.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries) with
                        | [| a; b |] -> Exchange (a.ToString() |> Int32.Parse, b.ToString() |> Int32.Parse);
    | Prefix "p" ps ->  match ps.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries) with
                        | [| a; b |] -> Partner (a.Chars(0), b.Chars(0));

let step (ps : char list) (m : Move) : char list =
    match m with
    | Spin x -> let qs, rs = ps |> List.splitAt (ps.Length - x);
                rs@qs;
    | Exchange (a, b) ->    ps
                            |> List.mapi (fun i x ->if i = a then
                                                        ps.[b];
                                                    else if i = b then
                                                        ps.[a];
                                                    else
                                                        x;);
    | Partner (a, b) -> ps
                        |> List.map (fun x ->   if x = a then
                                                    b;
                                                else if x = b then
                                                    a;
                                                else
                                                    x;);

let rec dance (ps : char list) (ms : Move list) : char list =
    match ms with 
    | [] -> ps;
    | x::xs -> dance (step ps x) xs;

let rec findCycle (n : int) (ps : char list) (ms : Move list) : int =
    let n' = n+1;
    let ps' = dance ps ms;

    if ps' = initials then
        n';
    else
        findCycle n' ps' ms;

let rec danceDanceDance (n : int) (ps : char list) (ms : Move list) : char list =
    match n with
    | 0 -> ps;
    | _ ->  let ps' = dance ps ms;
            danceDanceDance (n-1) ps' ms;

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file)).[0].Split([| ',' |], StringSplitOptions.RemoveEmptyEntries)
                |> Seq.toList
                |> List.map (parse);

    input
    |> dance initials
    |> List.fold (fun a c -> a+c.ToString()) ""
    |> printfn "Day 16, part 1: %s";

    let c = input
            |> findCycle 0 initials;
    
    input
    |> danceDanceDance (int (10000000000L%(int64 c))) initials
    |> List.fold (fun a c -> a+c.ToString()) ""
    |> printfn "Day 16, part 2: %s";
