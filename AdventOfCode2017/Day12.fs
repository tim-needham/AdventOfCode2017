module Day12

open System;
open System.IO;

type Connection = {
    Id : int;
    Ends : int list
};;

let parse (s : string) : Connection =
    match s.Split([| " <-> " |], StringSplitOptions.RemoveEmptyEntries) with
    | [| a; b |] -> let es = b.Split([| ", " |], StringSplitOptions.RemoveEmptyEntries)
                            |> Seq.toList
                            |> List.map (Int32.Parse);

                    { Id = Int32.Parse a; Ends = es };

let addUnique (i : int) (is : int list) : int list =
    match List.tryFind (fun x -> i = x) is with
    | Some _ -> is;
    | None -> (i::is) |> List.sort;

let rec graph (os : int list) (c : Connection) (cs : Connection list) : int list =
    match c.Ends with
    | [] -> addUnique c.Id os
    | xs -> let os' = addUnique c.Id os;
            let xs' = xs
                    |> List.where (fun x -> os' |> List.tryFind (fun y -> y = x) = None);
            let ys = xs'
                    |> List.map (fun x -> List.find (fun y -> y.Id = x) cs)
                    |> List.fold (fun a c -> graph a c cs) os';
            ys;

let rec graphs (cs : Connection list) : int list list =
    match cs with
    | [] -> []
    | x::xs ->  let gs = graph [] x cs;
                let cs' = cs
                        |> List.where (fun x -> gs |> List.tryFind (fun y -> y = x.Id) = None);
                gs::(graphs cs');

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (parse);

    
    graph [] (List.head input) input
    |> List.length
    |> printfn "Day 12, part 1: %d";

    input
    |> graphs
    |> List.length
    |> printfn "Day 12, part 2: %d";
