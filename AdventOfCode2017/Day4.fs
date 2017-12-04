module Day4

open System;
open System.IO;

let clist (s : string) : char list =
    Seq.toList s;

let clsort (s : string) : char list =
    s
    |> Seq.toList
    |> List.sort;

let ceq (x : char list) (y : char list) : bool =
    if x.Length <> y.Length then
        false;
    else
        List.zip x y
        |> List.map (fun (p, q) -> p = q)
        |> List.fold (&&) true;

let rec validate (f : string -> char list) (g : char list -> char list -> bool) (ss : char list list) (ts : string list) : int =
    match ts with
    | [] -> 1
    | x::xs ->  let y = f x;
                match List.tryFindIndex (fun z -> g y z) ss with
                | Some _ -> 0
                | None -> validate f g (y::ss) xs;

let rec scan (f : string -> char list) (g : char list -> char list -> bool) (ss : string list list) : int =
    match ss with
    | [] -> 0
    | x::xs -> (validate f g [] x) + (scan f g xs);

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (fun x -> x.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) |> Seq.toList)

    input
    |> scan clist ceq
    |> printfn "Day 4, part 1: %d";

    input
    |> scan clsort ceq
    |> printfn "Day 4, part 2: %d";
