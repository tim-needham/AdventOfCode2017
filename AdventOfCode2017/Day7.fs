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

let rec scan (p : Node) (cs : Node list) : Node list =
    match cs with
    | [] -> [p]
    | x::xs ->  match List.tryFindIndex (fun n -> n.Label = x.Label) p.Children with
                | Some _ -> (attach p x)::xs
                | None ->   match List.tryFindIndex (fun o -> o.Label = p.Label) x.Children with
                            | Some _ -> (attach x p)::xs;
                            | None -> x::(scan p xs);

let rec nodetree (ns : string list) : Node list =
    match ns with
    | [] -> []
    | x ->  match nodify x with
            | Some n -> [n]
            | None -> []
    | x::xs ->  let xs' = nodetree xs;
                match nodify x with
                | Some n -> scan n xs'
                | None -> xs'

"pbga (66)"
"xhth (57)"
"ebii (61)"
"havc (66)"
"ktlj (57)"
"fwft (72) -> ktlj, cntj, xhth"
"qoyq (66)"
"padx (45) -> pbga, havc, qoyq"
"tknk (41) -> ugml, padx, fwft"
"jptl (61)"
"ugml (68) -> gyxo, ebii, jptl"
"gyxo (61)"
"cntj (57)"



let run (file : string) =

    let input = Seq.toList(File.ReadLines(file));

    1
    |> printfn "Day 7, part 1: %d";

    2
    |> printfn "Day 7, part 2: %d";
