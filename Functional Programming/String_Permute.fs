//--------------------
//Edouard Moureaux
//December 2015
//
// Problem URL: 
// https://www.hackerrank.com/challenges/string-o-permute
//--------------------


namespace FunctionalProgramming.Recursion

module String_Permute =
    open System

    ///Permutre items in a list: Recursive
    let permute = 
        let rec loop acc = 
            function
                | a::b::xs -> 
                        let newacc = (string a)::(string b)::acc
                        loop newacc xs
                | [] -> acc |> List.rev
                | [_] -> acc |> List.rev
        loop []
    
    ///Permute characters in string + concat string optimization
    let solution (str:string) =
        str 
        |> List.ofSeq
        |> permute
        |> String.concat ""

    //[<EntryPoint>]
    let main argv = 
        let t = Console.ReadLine() |> int
        Array.init t (fun i -> Console.ReadLine() )
        |> Array.map solution
        |> Array.map (printfn "%s")
        |> ignore
        0

    let test = 
        Array.init 100000 (fun i -> "a")
        |> Array.reduce (+)
