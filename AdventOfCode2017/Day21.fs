module Day21

open System;
open System.IO;

let initial = [| [| '.'; '#'; '.' |]; [| '.'; '.'; '#' |]; [| '#'; '#'; '#' |] |];

type Pattern = {
    Match : char [] [];
    Output : char [] [];
};;

let parse (s : string) : Pattern =
    match s.Split([| " => " |], StringSplitOptions.None) with
    | [| a; b |] -> let ms = a.Split([| '/' |], StringSplitOptions.None)
                            |> Array.map (Seq.toArray);
                    let ts = b.Split([| '/' |], StringSplitOptions.None)
                            |> Array.map (Seq.toArray);
                    { Match = ms; Output = ts }

let flip (a : char [] []) : char [] [] =
    a
    |> Array.rev;

let rotate (a : char [] []) : char [] [] =
    // transpose
    let b = [| for i in 0..a.Length-1 -> 
                [| for j in 0..a.[i].Length-1 -> 
                    a.[j].[i]
                |]
            |];
    // then reverse columns
    [| for i in 0..b.Length-1 -> 
        [| for j in 0..b.[i].Length-1 -> 
            b.[b.Length - i - 1].[j]
        |]
    |];

let rotations (a : char [] []) : (char [] []) list =
    let b = rotate a;
    let c = rotate b;
    let d = rotate c;
    [a; b; c; d];

let permutations (p : Pattern) : Pattern list =
    ((p.Match |> rotations) @ (p.Match |> flip |> rotations))
    |> List.map (fun x -> { Match = x; Output = p.Output });

let pp (a : char [] []) : string =
    a
    |> Array.fold (fun b y -> b + "/" + (y |> Array.fold (fun c x -> c+(string x)) "")) "/";

let arrayMatch (a : char [] []) (b : char [] []) : bool =
    Array.zip a b
    |> Array.forall (fun (x, y) -> x = y);

let doesMatch (p : Pattern) (i : char [] []) : bool =
    if p.Match.Length <> i.Length then
        false;
    else
        arrayMatch p.Match i;

let chunk (a : char [] []) : (char [] []) list list =
    let d = if a.Length % 2 = 0 then 2 else 3;
    [ for j in 0..(a.Length / d)-1 ->
        [ for i in 0..(a.[j].Length / d)-1 ->
            (i, j)
            let ly = d*j;
            let my = ly+d-1
            [| for y in ly..my ->
                let lx = d*i;
                let mx = lx+d-1;
                [| for x in lx..mx ->
                    a.[y].[x];
                |]
            |]
        ]
    ];

let transform (ps : Pattern list) (a : char [] []) : char [] [] =
    ps
    |> List.find (fun x -> doesMatch x a)
    |> (fun x -> x.Output);

let stitch (cs : (char [] []) list list) : char [] [] =
    let s = [| 0..cs.[0].[0].Length-1 |]
            |> Array.map (fun _ -> [| |]);
    cs
    |> List.map (List.fold (fun a y -> [| for i in 0..a.Length-1 -> Array.concat [| a.[i]; y.[i] |] |]) s )
    |> List.fold (fun a x -> Array.concat [| a; x |]) [| |];

let rec draw (n : int) (a : char [] []) (ps : Pattern list) : char [] [] =
    match n with
    | 0 -> a;
    | _ ->  let o = a
                    |> chunk
                    |> List.map (List.map (transform ps))
                    |> stitch;

            draw (n-1) o ps;

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (parse)
                |> List.collect (permutations);

    let p1 = draw 5 initial input;

    p1
    |> Array.fold (fun b y -> b + (y |> Array.fold (fun a x -> a + if x = '#' then 1 else 0) 0)) 0
    |> printfn "Day 21, part 1: %d";

    draw 13 p1 input
    |> Array.fold (fun b y -> b + (y |> Array.fold (fun a x -> a + if x = '#' then 1 else 0) 0)) 0
    |> printfn "Day 21, part 2: %d";
