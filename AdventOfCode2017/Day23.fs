module Day23

open System;
open System.IO;

type Reg = {
    Id : string;
    Value : int64;
};;

type Val = 
    | Reg of string
    | Lit of int64;;

type Instruction =
    | Set of string * Val
    | Sub of string * Val
    | Mul of string * Val
    | Jnz of Val * Val;;

let parseVal (s : string) : Val =
    match Int64.TryParse s with
    | false, _ -> Reg s;
    | true, x -> Lit x;

let parse (s : string) : Instruction =
    match s.Split([| ' ' |], StringSplitOptions.None) with
    | [| "set"; a; b; |] -> Set (a, parseVal b);
    | [| "sub"; a; b; |] -> Sub (a, parseVal b);
    | [| "mul"; a; b; |] -> Mul (a, parseVal b);
    | [| "jnz"; a; b; |] -> Jnz (parseVal a, parseVal b);

let eval (v : Val) (rs : Reg list) : int64 =
    match v with
    | Reg x ->  rs
                |> List.find (fun r -> r.Id = x)
                |> (fun r -> r.Value);
    | Lit x -> x;

let set (r : string) (x : int64) (rs : Reg list) : Reg list =
    rs
    |> List.map (fun y -> if y.Id = r then { Id = r; Value = x } else y);

let rec exec (p : int) (rs : Reg list) (ds : Reg list) (is : Instruction list) : Reg list  * Reg list =
    if p >= is.Length then
        rs, ds;
    else
        match is.[p] with
        | Set (a, b) -> let ds' = set "Set" ((eval (Reg "Set") ds)+1L) ds;
                        let r = eval b rs;
                        let rs' = set a r rs;
                        exec (p+1) rs' ds' is; 
        | Sub (a, b) -> let ds' = set "Sub" ((eval (Reg "Sub") ds)+1L) ds;
                        let r, s = eval (Reg a) rs, eval b rs;
                        let rs' = set a (r-s) rs;
                        exec (p+1) rs' ds' is;
        | Mul (a, b) -> let ds' = set "Mul" ((eval (Reg "Mul") ds)+1L) ds;
                        let r, s = eval (Reg a) rs, eval b rs;
                        let rs' = set a (r*s) rs;
                        exec (p+1) rs' ds' is;
        | Jnz (a, b) -> let ds' = set "Jnz" ((eval (Reg "Jnz") ds)+1L) ds;
                        let r, s = eval a rs, eval b rs;
                        if r<>0L then
                            exec (p + int s) rs ds' is;
                        else
                            exec (p+1) rs ds' is;

let rec facs (n : int) (k : int) (l : int) (fs : int list) : int list =
    if k > l then
        fs;
    else
        let fs' = if n%k = 0 then k::fs else fs;
        facs n (k+1) l fs';

let factors (n : int) : int list =
    facs n 1 (float n |> sqrt |> Math.Floor |> int) [];

let isComposite (n : int) : bool =
    match (factors n).Length with
    | 1 -> false;
    | _ -> true;

let rec precalculate (b : int) (c : int) (h : int) : int =
    if b > c then
        h;
    else
        let h' = if isComposite b then h+1 else h;
        precalculate (b+17) c h';

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (parse);

    let ds = [ "Set"; "Sub"; "Mul"; "Jnz" ]
                |> List.map (fun x -> { Id = x; Value = 0L });

    let rs = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]
                |> List.map (fun x -> { Id = x; Value = 0L });

    input
    |> exec 0 rs ds
    |> snd
    |> eval (Reg "Mul")
    |> printfn "Day 23, part 1: %d";

    // Setting a = 1 initialises b to 105700 and c to b + 17000
    // The code then basically increments in 17s from b to c and
    // increments h if b' is a composite.

    precalculate 105700 122700 0
    |> printfn "Day 23, part 2: %d";
