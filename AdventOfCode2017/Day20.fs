module Day20

open System;
open System.IO;

type Particle = {
    Id : int;
    Position : int64 * int64 * int64;
    Velocity : int64 * int64 * int64;
    Acceleration : int64 * int64 * int64;
};;

let parseTriple (s : string) : int64 * int64 * int64 =
    match s.Substring(3, s.Length - 4).Split([| ',' |], StringSplitOptions.None) with
    | [| x; y; z; |] -> (Int64.Parse x, Int64.Parse y, Int64.Parse z);

let parse (i : int) (s : string) : Particle =
    match s.Split([| ", " |], StringSplitOptions.RemoveEmptyEntries) with
    | [| a; b; c |] -> { Id = i; Position = parseTriple a; Velocity = parseTriple b; Acceleration = parseTriple c };

let absolute3 ((x, y, z) : int64 * int64 * int64) : int64 * int64 * int64 =
    (Math.Abs x, Math.Abs y, Math.Abs z);

let magnitude3 ((x, y, z) : int64 * int64 * int64) : int64 =
    let (p, q, r) = absolute3 (x, y, z);
    p + q + r;

let longTermClosest (ps : Particle list) : int =
    ps
    |> List.sortBy(fun x -> magnitude3 x.Acceleration, magnitude3 x.Velocity, magnitude3 x.Position)
    |> List.head
    |> (fun x -> x.Id);

let solveLinear (b : float) (c : float) : float list =
    [-c / b];

let solveQuadratic (a : float) (b : float) (c : float) : float list =
    match Math.Pow (b, 2.0) - (4.0 * a * c) with
    | n when n <= 0.0 -> []
    | r ->  [ (b - sqrt r) / (2.0*a); (b + sqrt r) / (2.0*a)];

let intersect (x : Particle) (y : Particle) (f : (int64 * int64 * int64) -> int64) : int list =
    let a = f x.Acceleration - f y.Acceleration;
    let b = f x.Velocity - f y.Velocity;
    let c = f x.Position - f y.Position;

    // Slightly concerned that this doesn't handle the linear case, i.e. identical accel and velocity for a given coordinate...!
    let rs = if a = 0L then
                solveLinear (float b) (float c)
             else
                solveQuadratic (float a) (float b) (float c);

    rs
    |> List.where (fun x -> Math.Ceiling x = x)
    |> List.where (fun x -> x > 0.0)
    |> List.map (fun x -> int x);

let collide (a : Particle) (b : Particle) : (int * int * int) list =
    let xs = intersect a b (fun (x, _, _) -> x);
    let ys = intersect a b (fun (_, y, _) -> y);
    let zs = intersect a b (fun (_, _, z) -> z);
    xs@ys@zs
    |> List.distinct
    |> List.where (fun x -> xs |> List.contains (x) 
                            && ys |> List.contains (x)
                            && zs |> List.contains (x))
    |> List.map (fun x -> (x, a.Id, b.Id));

let remove (ps : int list) (is : (int * int * int) list list) : (int * int * int) list list =
    is
    |> List.map (List.where (fun (_, a, b) -> ps |> List.tryFindIndex (fun x -> x = a || x = b) = None));

let rec collisions (cs : int list) (p : int) (is : (int * int * int) list list) : int =
    match is with
    | [] -> p - cs.Length;
    | x::xs ->  let ps = x
                         |> List.fold (fun a (_, b, c) -> a@[b; c]) []
                         |> List.sort
                         |> List.distinct;
                let ys = remove ps xs;
                collisions (cs@ps) p ys;

let remainder (ps : Particle list) : int =
    let is = ps
             |> List.allPairs ps
             |> List.where (fun (x, y) -> x.Id <> y.Id)
             |> List.distinctBy (fun (x, y) -> (Math.Min (x.Id, y.Id), Math.Max (x.Id, y.Id)))
             |> List.collect (fun (x, y) -> collide x y)
             |> List.distinct
             |> List.groupBy (fun (t, _, _) -> t)
             |> List.map (snd);

    collisions [] ps.Length is;

let distance ((ox, oy, oz) : int64 * int64 * int64) ((px, py, pz) : int64 * int64 * int64) : int64 =
    Math.Abs(px - ox) + Math.Abs(py - oy) + Math.Abs(pz - oz);

let add3 ((ox, oy, oz) : int64 * int64 * int64) ((px, py, pz) : int64 * int64 * int64) : int64 * int64 * int64 =
    (px + ox, py + oy, pz + oz);

let aligned3 ((ox, oy, oz) : int64 * int64 * int64) ((px, py, pz) : int64 * int64 * int64) : bool =
    Math.Sign ox = Math.Sign px
    && Math.Sign oy = Math.Sign py
    && Math.Sign oz = Math.Sign pz;

let aligned (p : Particle) : bool =
    aligned3 p.Velocity p.Acceleration;

let move (p : Particle) : Particle =
    let v' = add3 p.Velocity p.Acceleration;
    let p' = add3 p.Position v';
    { Id = p.Id; Position = p'; Velocity = v'; Acceleration = p.Acceleration };

let frame (ps : Particle list) : Particle list =
    let ps' = ps |> List.map (move);
    ps';

let rec simulate (ds : int list) (ps : Particle list) : int =
    let ps' = frame ps;
    let ds'=    ps' 
                |> List.map (fun x -> x.Id, distance x.Position (0L, 0L, 0L))
                |> List.sortBy (snd)
                |> List.map (fst);

    match ps' |> List.forall (aligned) with
    | true ->   match List.zip ds ds' |> List.fold (fun a (x, y) -> a && (x=y)) true with
                | true ->   ds'
                            |> List.head;
                | false -> simulate ds' ps';
    | false -> simulate ds' ps';

let rec simulate2 (n : int) (ps : Particle list) : int =
    match n with
    | 0 -> ps.Length;
    | _ ->  let ps' = frame ps
                    |> List.groupBy (fun x -> x.Position)
                    |> List.where (fun x -> (snd x).Length = 1)
                    |> List.collect (fun x -> snd x);
            simulate2 (n-1) ps';


let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.mapi (parse);

    let ts = [ "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>";
                "p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>";
                "p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>";
                "p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>" ]
                |> List.mapi (parse);

    //input
    //|> remainder
    //|> printfn "%A"

    input
    |> longTermClosest
    |> printfn "Day 20, part 1: %d";

    input
    |> simulate2 1000
    |> printfn "Day 20, part 2: %d";
