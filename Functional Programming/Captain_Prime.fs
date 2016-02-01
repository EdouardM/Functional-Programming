//--------------------
//Edouard Moureaux
//January 2016
//
// Problem URL: 
// https://www.hackerrank.com/challenges/captain-prime
//--------------------
namespace FunctionalProgramming.AdHoc

module Captain_Prime =
    open System
                  
    let noFactorOf n s = s |> Seq.exists(fun i -> n % i = 0) |> not

    ///Tells if a number is a Prime number
    let isPrime n = 
        if n = 1 then 
            false
        else
            let bound = int (sqrt(float n))
            seq{2..bound} |> noFactorOf n

    ///Active Pattern Prime numbers
    let (|Prime|_|) (str:string) = 
        let n = int str 
        if isPrime n then 
            Some str 
        else 
            None 

    let (|ZeroDigit|_|) (str:string) = 
        if str.Contains("0") then 
            Some(str) 
        else 
            None

    let (|PrimeNoZero|_|) (str:string) =
        match str with
            | ZeroDigit s -> None
            | Prime s -> Some s
            | _ -> None

    let reverse (str:string) =
        str.ToCharArray()
        |> Array.rev
        |> Array.map string
        |> String.concat ""

    let leftPart (str:string) =
        let n = str.Length
        [0..(n-1)] 
        |> List.map (fun i -> str.Substring(i))

    let rightPart (str:string) =
        let n = str.Length
        let str' = reverse str
        [0..(n-1)] 
        |> List.map (fun i -> str'.Substring(i))
        |> List.map reverse
    
    let (|LeftPart|_|) str =
        let test = 
            leftPart str 
            |> List.forall (fun s -> s |> int |> isPrime)
        if test then 
            Some str 
        else None

    let (|RightPart|_|) str =
        let test = 
            rightPart str 
            |> List.forall (fun s -> s |> int |> isPrime)
        if test then 
            Some str 
        else None

    let (|LEFT|RIGHT|CENTER|DEAD|) =
        function
            | PrimeNoZero s -> 
                match s with
                    | LeftPart s -> 
                        match s with
                            | RightPart s -> CENTER
                            | _ -> LEFT
                    | RightPart s -> RIGHT
                    | _ -> DEAD
            | _ -> DEAD

    let solution = 
        function 
            | LEFT -> "LEFT"
            | RIGHT -> "RIGHT"
            | CENTER -> "CENTRAL"
            | DEAD -> "DEAD"

    //[<EntryPoint>]
    let main argv =
        let t = Console.ReadLine() |> int
        List.init t (fun _ ->  Console.ReadLine())
        |> List.map solution
        |> List.map (printfn "%s")
        |> ignore
        0