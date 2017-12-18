module Day18

open System;
open System.IO;

type Register = {
    Name : string;
    Val : int64;
};;

type Target =
    | Reg of string
    | Val of int64;;

type Instruction =
    | Snd of Target
    | Set of Target * Target
    | Add of Target * Target
    | Mul of Target * Target
    | Mod of Target * Target
    | Rcv of Target 
    | Jgz of Target * Target;;

type Cpu = {
    Counter : int;
    Registers : Register list;
    Pipe : int64 list;
    Io : int64;
    Waiting : bool;
};;

let parseTarget (s : string) : Target =
    match Int64.TryParse s with
    | (true, v) -> Val v;
    | (false, _) -> Reg s;

let parse (s : string) : Instruction =
    match s.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) with
    | [| "snd"; x |] -> Snd (parseTarget x);
    | [| "set"; x; y |] -> Set (parseTarget x, parseTarget y);
    | [| "add"; x; y |] -> Add (parseTarget x, parseTarget y);
    | [| "mul"; x; y |] -> Mul (parseTarget x, parseTarget y);
    | [| "mod"; x; y |] -> Mod (parseTarget x, parseTarget y);
    | [| "rcv"; x |] -> Rcv (parseTarget x);
    | [| "jgz"; x; y |] -> Jgz (parseTarget x, parseTarget y);

let registers (i : int64) : Register list =
    ['a'..'z'] 
    |> List.map (fun x -> { Name = x.ToString(); Val = i });


let eval (t : Target) (rs : Register list) : int64 =
    match t with
    | Val x -> x;
    | Reg r ->  (rs |> List.find (fun x -> x.Name = r)).Val;

let set (r : string) (v : int64) (rs : Register list) : Register list =
    rs
    |> List.map (fun x ->   if x.Name = r then
                                { Name = r; Val = v };
                            else
                                x;)

let recover (x : Target) (c : Cpu) : Cpu =
    if eval x c.Registers <> 0L then
        {
            Counter = c.Counter+1;
            Registers = c.Registers;
            Pipe = c.Pipe;
            Io = c.Io;
            Waiting = true
        };
    else
        {
            Counter = c.Counter+1;
            Registers = c.Registers;
            Pipe = c.Pipe;
            Io = c.Io;
            Waiting = false
        };

let receive (x : Target) (c : Cpu) : Cpu =
    match x with
    | Reg r ->  match c.Pipe |> List.rev with
                | [] -> {
                            Counter = c.Counter;
                            Registers = c.Registers;
                            Pipe = c.Pipe;
                            Io = c.Io;
                            Waiting = true
                        };
                | p::ps ->  {
                                Counter = c.Counter+1;
                                Registers = set r p c.Registers;
                                Pipe = ps |> List.rev;
                                Io = c.Io;
                                Waiting = false
                            };

let rec exec (c : Cpu) (f : Target -> Cpu -> Cpu) (is : Instruction list) : Cpu * int64 option =
    match is.[c.Counter] with
    | Snd x ->  {
                    Counter = c.Counter+1;
                    Registers = c.Registers;
                    Pipe = c.Pipe;
                    Io = c.Io+1L;
                    Waiting = false
                },
                Some (eval x c.Registers)
    | Set (Reg x, y) -> {
                            Counter = c.Counter+1;
                            Registers = set x (eval y c.Registers) c.Registers;
                            Pipe = c.Pipe;
                            Io = c.Io;
                            Waiting = false
                        },
                        None
    | Add (Reg x, y) -> let x' = (eval (Reg x) c.Registers) + (eval y c.Registers);
                        {
                            Counter = c.Counter+1;
                            Registers = set x x' c.Registers;
                            Pipe = c.Pipe;
                            Io = c.Io;
                            Waiting = false
                        },
                        None
    | Mul (Reg x, y) -> let x' = (eval (Reg x) c.Registers) * (eval y c.Registers);
                        {
                            Counter = c.Counter+1;
                            Registers = set x x' c.Registers;
                            Pipe = c.Pipe;
                            Io = c.Io;
                            Waiting = false
                        },
                        None
    | Mod (Reg x, y) -> let x' = (eval (Reg x) c.Registers) % (eval y c.Registers);
                        {
                            Counter = c.Counter+1;
                            Registers = set x x' c.Registers;
                            Pipe = c.Pipe;
                            Io = c.Io;
                            Waiting = false
                        },
                        None
    | Rcv x ->  f x c, None;
    | Jgz (x, y) -> if eval x c.Registers > 0L then
                        {
                            Counter = c.Counter + int (eval y c.Registers);
                            Registers = c.Registers;
                            Pipe = c.Pipe;
                            Io = c.Io;
                            Waiting = false
                        },
                        None
                    else
                        {
                            Counter = c.Counter+1;
                            Registers = c.Registers;
                            Pipe = c.Pipe;
                            Io = c.Io;
                            Waiting = false
                        },
                        None

let rec singleThread (c : Cpu) (ss : int64 list) (is : Instruction list) : Cpu * int64 list=
    if c.Counter >= is.Length then
        c, ss;
    else
        let c', s = exec c recover is;
        let ss' =   match s with
                    | Some x -> x::ss;
                    | None -> ss;
        if c'.Waiting then
            c', ss';
        else
            singleThread c' ss' is;

let rec doubleThread (a : Cpu) (b : Cpu) (is : Instruction list) : Cpu * Cpu =
    if (a.Waiting || a.Counter >= is.Length) && (b.Waiting || b.Counter >= is.Length) then
        a, b;
    else
        let a', qA = if a.Counter < is.Length then
                        exec a receive is;
                     else
                        a, None;
        let b', qB = if b.Counter < is.Length then
                        exec b receive is;
                     else
                        b, None;
        let c = {
            Counter = a'.Counter;
            Registers = a'.Registers;
            Pipe =  match qB with
                    | None -> a'.Pipe
                    | Some x -> x::a'.Pipe;
            Io = a'.Io;
            Waiting = a'.Waiting
        }
        let d = {
            Counter = b'.Counter;
            Registers = b'.Registers;
            Pipe =  match qA with 
                    | None -> b'.Pipe 
                    | Some x -> x::b'.Pipe;
            Io = b'.Io;
            Waiting = b'.Waiting
        }

        doubleThread c d is;



let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (parse);

    let cpu = {
        Counter = 0;
        Registers = registers 0L;
        Pipe = [];
        Io = 0L;
        Waiting = false
    }

    input
    |> singleThread cpu []
    |> snd
    |> List.head
    |> printfn "Day 18, part 1: %d";

    let cpu0 = {
        Counter = 0;
        Registers = registers 0L;
        Pipe = [];
        Io = 0L;
        Waiting = false
    }

    let cpu1 = {
        Counter = 0;
        Registers = registers 1L;
        Pipe = [];
        Io = 0L;
        Waiting = false
    }

    input
    |> doubleThread cpu0 cpu1
    |> snd
    |> (fun x -> x.Io)
    |> printfn "Day 18, part 2: %d";
