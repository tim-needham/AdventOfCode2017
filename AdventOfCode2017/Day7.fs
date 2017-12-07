module Day7

open System;
open System.IO;

type Node = {
    Label : string;
    Weight : int;
    Children : Node list;
}

let parts (s : string) : (string * int) =
    match s.Split([| ' '; '('; ')' |], StringSplitOptions.RemoveEmptyEntries) with
    | [| a; b |] -> (a, Int32.Parse b);

let nodify (s : string) : Node option =
    match s.Split([| " -> " |], StringSplitOptions.RemoveEmptyEntries) with
    | [| a; b |] -> let xs = b.Split([| ", " |], StringSplitOptions.RemoveEmptyEntries);
                    let (l, w) = parts a;
                    let cs = xs |> Seq.toList |> List.map (fun x -> { Label = x; Weight = 0; Children = [] });
                    let n = { Label = l; Weight = w; Children = cs };
                    Some n
    | [| a |] ->    let (l, w) = parts a;
                    let n = { Label = l; Weight = w; Children = [] };
                    Some n
    | _ -> None

let attach (p : Node) (c : Node) : Node =
    {
        Label = p.Label;
        Weight = p.Weight;
        Children =  p.Children
                    |> List.map (fun x -> if x.Label = c.Label then c else x)
    };

let rec target (p : Node) (c : Node) : Node option =
    if p.Children.Length = 0 then
        None;
    else 
        match List.tryFindIndex (fun x -> x.Label = c.Label) p.Children with
        | Some _ -> let p' = attach p c;
                    Some p';
        | None ->   let qs = p.Children
                            |> List.map (fun x -> target x c);

                    match List.tryFindIndex (fun x -> x <> None) qs with
                    | Some i -> match qs.[i] with
                                | Some n -> let cs' = p.Children
                                                    |> List.mapi (fun j y -> if i = j then n else y);
                                            let p' = { Label = p.Label; Weight = p.Weight; Children = cs' };
                                            Some p'
                                | None -> None;
                    | None -> None;

let rec m1 (p : Node) (cs : Node list) : Node * (Node list) = 
    match cs with
    | [] -> (p ,[]);
    | x::xs ->  match target p x with 
                | Some y -> m1 y xs;
                | None ->   let (q, ds) = m1 p xs;
                            (q, x::ds);

let rec m2 (p : Node) (cs : Node list) : Node list =
    match cs with
    | [] -> [p]
    | x::xs ->  match target x p with
                | Some z -> z::xs
                | None -> x::(m2 p xs);

let merge (p : Node) (cs : Node list) : Node list =
    let (q, ds) = m1 p cs;
    m2 q ds;

let rec nodetree (ns : Node list) : Node list =
    match ns with
    | [] -> []
    | [x] -> [x]
    | x::xs ->  let xs' = nodetree xs;
                merge x xs';

let rec weight (n : Node) : int =
    (List.fold (fun a c -> a + weight c) 0 n.Children) + n.Weight;

let rec unwind (n : Node) : Node list =
    match n.Children with
    | [] -> [n]
    | xs -> n.Children 
            |> List.fold (fun a c -> (unwind c)@a) [n];

let rec collect (cs : (int * int) list) (is : int list) : (int * int) list =
    match is with
    | [] -> cs
    | x::xs ->  match List.tryFindIndex (fun (a, b) -> a = x) cs with
                | Some y -> let ds = cs
                                    |> List.mapi (fun i (p, q) -> if i = y then (p, q+1) else (p, q))
                            collect ds xs;
                | None -> collect ((x, 1)::cs) xs

let balanced (n : Node) : bool * int =
    match n.Children with
    | [] -> (true, 0)
    | xs -> let ws = List.map (fun x -> (x.Weight, weight x)) xs;
            let ys = ws
                    |> List.map (snd)
                    |> collect []
                    |> List.sortByDescending (snd);

            if ys.Length = 1 then
                (true, 0)
            else
                let d = (fst ys.[0]) - (fst ys.[1]);
                let (i, _) = List.find (fun (_, y) -> y = (fst ys.[1])) ws;

                (false, i + d);

let rec unbalanced (n : Node list) : int =
    match n with
    | [] -> 0;
    | x::xs ->  match balanced x with
                | (true, _) -> unbalanced xs;
                | (false, y) -> y;

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (nodify)
                |> List.filter (fun x -> match x with | Some y -> true | None -> false)
                |> List.map (fun (Some x) -> x);

    let t = input
            |> nodetree
            |> List.head;

    t.Label
    |> printfn "Day 7, part 1: %A";

    t
    |> unwind
    |> unbalanced
    |> printfn "Day 7, part 2: %A";
