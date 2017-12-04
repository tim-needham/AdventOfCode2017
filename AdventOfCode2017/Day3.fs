module Day3

open System;
open System.IO;

//  Thi is an Ulam spiral. Hooray. The diagonals are at:
//  4n^2 - 2n + 1
//  4n^2 + 1
//  4n^2 + 2n + 1
//  4n^2 - 4n + 1 (out of step by one layer)
//  
//  17  16  15  14  13
//  18   5   4   3  12
//  19   6   1   2  11
//  20   7   8   9  10
//  21  22  23---> ...
//
//  1 - Find which layer of the square we are in.
//      for any n, layer = maximum k : n <= (2k + 1) ^ 2
//  2 - Find how far we are from the centre of an edge in that square.
//  3 - Answer = layer + distance from centre

type Cell = 
    | P1    // Pre-corner 1
    | P2    // Pre-corner 2
    | P3    // Pre-corner 3
    | C1    // Corner 1
    | C2    // Corner 2
    | C3    // Corner 3
    | C4    // Corner 4
    | O1    // Post-corner 1
    | O2    // Post-corner 2
    | O3    // Post-corner 3
    | O4    // Post-corner 4
    | E1    // First edge after post-corner 4
    | Edge  // Any other edge piece
;;

let rec layer (k : int) (n : int) : int =
    let p = pown (2*k + 1) 2;
    if n > p then
        layer (k+1) n;
    else
        k;

let centres (l : int) : int list =
    let s = pown (2*l + 1) 2;
    let e = pown (2*l - 1) 2;
    let c = (s - e) / 4;
    [for i in 1..4 -> e + i*c - c/2];

let distance (l : int) (n : int) : int =
    centres l
    |> List.map (fun c -> Math.Abs(n - c))
    |> List.min;

let manhattan (n : int) : int = 
    let l = layer 0 n;
    let d = distance l n;
    l + d;

let model = [ 1; 1; 2; 4; 5; 10; 11; 23; 25; 26; 54; 57; 59; 122; 133; 142; 147; 304; 330; 351; 362; 747; 806; 880; 931]

let c1 (l : int) : int =
    (4*l*l) - (2*l) + 1;

let c2 (l : int) : int = 
    (4*l*l) + 1;

let c3 (l : int) : int =
    (4*l*l) + (2*l) + 1;

let c4 (l : int) : int = 
    (4*l*l) - (4*l) + 1;

let position (n : int) : Cell =
    let l = layer 0 n;
    let x1, x2, x3, x4, x5 = c1 l, c2 l, c3 l, c4 (l+1), c4 l;

    if n = x1 - 1 then
        P1;
    else if n = x1 then
        C1;
    else if n = x1 + 1 then
        O1;
    else if n = x2 - 1 then
        P2;
    else if n = x2 then
        C2;
    else if n = x2 + 1 then
        O2;
    else if n = x3 - 1 then
        P3;
    else if n = x3 then
        C3;
    else if n = x3 + 1 then
        O3;
    else if n = x4 then
        C4;
    else if n = x5 + 1 then
        O4;
    else if n = x5 + 2 then
        E1;
    else
        Edge;

let find (n : int) (ns : (int * int) list) : int =
    let (p, q) = List.find (fun (x, y) -> n = x) ns
    q;

let rec ringsum (n : int) : (int * int) list =
    // precompute the first ring as all hell can break loose...
    match n with
    | 1 -> [(1, 1)]
    | 2 -> (2, 1)::(ringsum 1)
    | 3 -> (3, 2)::(ringsum 2)
    | 4 -> (4, 4)::(ringsum 3)
    | 5 -> (5, 5)::(ringsum 4)
    | 6 -> (6, 10)::(ringsum 5)
    | 7 -> (7, 11)::(ringsum 6)
    | 8 -> (8, 23)::(ringsum 7)
    | 9 -> (9, 25)::(ringsum 8)
    | x ->  let l = layer 0 n;
            let ns = ringsum (n-1);
            let (a, b) = List.head ns;

            let q = match position n with
                    | P1 -> let p = c1 (l-1);
                            b + (find p ns) + (find (p-1) ns);
                    | P2 -> let p = c2 (l-1);
                            b + (find p ns) + (find (p-1) ns);
                    | P3 -> let p = c3 (l-1);
                            b + (find p ns) + (find (p-1) ns);
                    | C1 -> let p = c1 (l-1);
                            b + (find p ns);
                    | C2 -> let p = c2 (l-1);
                            b + (find p ns);
                    | C3 -> let p = c3 (l-1);
                            b + (find p ns);
                    | C4 -> let p = c4 l;
                            b + (find p ns) + (find (p+1) ns)
                    | O1 -> let p = c1 (l-1);
                            b + (find (p+1) ns) + (find p ns) + (find (n-2) ns);
                    | O2 -> let p = c2 (l-1);
                            b + (find (p+1) ns) + (find p ns) + (find (n-2) ns);
                    | O3 -> let p = c3 (l-1);
                            b + (find (p+1) ns) + (find p ns) + (find (n-2) ns);
                    | O4 -> let p = c4 (l-1);
                            b + (find (p+1) ns);
                    | E1 -> let p = n - (c4 l) + (c4 (l - 1))
                            b + (find p ns) + (find (p-1) ns) + (find (n-2) ns)
                    | Edge ->   let p = if n > c3 l then
                                            n - (c3 l) + (c3 (l - 1))
                                        else if n > c2 l then
                                            n - (c2 l) + (c2 (l - 1))
                                        else if n > c1 l then
                                            n - (c1 l) + (c1 (l - 1))
                                        else
                                            n - (c4 l) + (c4 (l - 1));
                                b + (find p ns) + (find (p-1) ns) + (find (p-2) ns);
            (n, q)::ns;

let rec scan (n : int) (t : int) : int =
    let ns = ringsum n;
    let (p, q) = List.head ns;
    if q >= t then
        q
    else
        scan (n+1) t;

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file)).[0]
                |> Int32.Parse;

    input
    |> manhattan
    |> printfn "Day 3, part 1: %d";

    input
    |> scan 1
    |> printfn "Day 3, part 2: %d";