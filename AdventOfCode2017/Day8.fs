module Day8

open System;
open System.IO;

type Predicate =
    | Gt of string * int
    | Lt of string * int
    | Geq of string * int
    | Leq of string * int
    | Eq of string * int
    | Neq of string * int;;

type Instruction =
    | Inc
    | Dec;;

type Line = {
    Reg : string;
    Inst : Instruction;
    Amount : int;
    Pred : Predicate
}

type Reg = {
    Id : string;
    Val : int
};;

let instruction (s : string) : Instruction =
    match s with
    | "inc" -> Inc;
    | "dec" -> Dec;

let predicate (p : string) (a : string) (b : string) : Predicate =
    let c = Int32.Parse b;
    match p with
    | ">" -> Gt (a, c);
    | "<" ->  Lt (a, c);
    | ">=" -> Geq (a, c);
    | "<=" -> Leq (a, c);
    | "==" -> Eq (a, c);
    | "!=" -> Neq (a, c);

let parse (s : string) : Line =
    match s.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) with
    | [| a; b; c; "if"; d; e; f|] ->    let i = instruction b;
                                        let p = predicate e d f;
                                        let n = Int32.Parse c;
                                        {
                                            Reg = a;
                                            Inst = i;
                                            Amount = n;
                                            Pred = p
                                        };

let peek (rs : Reg list) (a : string) : int option =
    match List.tryFind (fun x -> x.Id = a) rs with
    | Some r -> Some r.Val;
    | None -> None

let target (p : Predicate) : string =
    match p with
    | Gt (a, _) -> a;
    | Lt (a, _) ->  a;
    | Geq (a, _) -> a;
    | Leq (a, _) -> a;
    | Eq (a, _) ->  a;
    | Neq (a, _) -> a;

let check (rs : Reg list) (p : Predicate) : bool =
    match p with
    | Gt (a, b) ->  match peek rs a with
                    | Some c -> c > b;
                    | None -> false;
    | Lt (a, b) ->  match peek rs a with
                    | Some c -> c < b;
                    | None -> false;
    | Geq (a, b) -> match peek rs a with
                    | Some c -> c >= b;
                    | None -> false;
    | Leq (a, b) -> match peek rs a with
                    | Some c -> c <= b
                    | None -> false;
    | Eq (a, b) ->  match peek rs a with
                    | Some c -> c = b;
                    | None -> false;
    | Neq (a, b) -> match peek rs a with
                    | Some c -> c <> b
                    | None -> false;
 
let apply (n : int) (i : Instruction) (a : int) : int =
    match i with
    | Inc -> n + a;
    | Dec -> n - a;

let grab (r : string) (rs : Reg list) : Reg list =
    match List.tryFindIndex (fun x -> x.Id = r) rs with
    | Some _ -> rs;
    | None -> rs @ [{ Id = r; Val = 0}];

let eval (rs : Reg list) (l : Line) : Reg list =
    let rs' = grab (target l.Pred) (grab l.Reg rs);
    if check rs' l.Pred then
        match List.tryFind (fun x -> x.Id = l.Reg) rs' with
        | Some r -> let r' = { Id = r.Id; Val = apply r.Val l.Inst l.Amount };
                    List.map (fun x -> if x.Id = r.Id then r' else x) rs';
        | None -> rs' @ [{ Id = l.Reg; Val = apply 0 l.Inst l.Amount }];
    else
        rs';

let rec proc (rs : Reg list) (m : int) (ls : Line list) : int * Reg list =
    match ls with
    | [] -> (m, rs);
    | x::xs ->  let rs' = eval rs x;
                let n = List.maxBy (fun x -> x.Val) rs';
                let o = Math.Max (m, n.Val);
                proc rs' o xs;

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (parse);

    let (m, rs) = proc [] 0 input;
    let r = rs
            |> List.maxBy (fun x -> x.Val)

    r.Val
    |> printfn "Day 8, part 1: %d";

    m
    |> printfn "Day 8, part 2: %d";
