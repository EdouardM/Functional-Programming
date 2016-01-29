//--------------------
//Edouard Moureaux
//January 2016
//
// Problem URL: 
// https://www.hackerrank.com/challenges/sequence-full-of-colors/
//--------------------
namespace FunctionalProgramming.Recursion

module Sequence_Colors =
    open System.IO
    open System

    //validate rule 3 & 4 with prefix:
    let validPrefix (r, g, b, y) = abs (r - g) <= 1 && abs (b - y) <=1 

    let colors (str:string) = 
        str.ToCharArray()
        |> Array.fold(
            fun (test, (r, g, y, b)) c -> 
                let state = 
                    match c with
                        | 'R' -> (r + 1, g, y, b)
                        | 'G' -> (r, g + 1, y, b)
                        | 'Y' -> (r, g, y + 1, b)
                        | 'B' -> (r, g, y, b + 1)
                        | _ -> (r, g, y, b)
                (test && validPrefix state, state)
        ) (true, (0, 0, 0, 0))
    
    // final validation
    let validate (test, (r, g, y, b)) = test && (r = g) && (y = b) 


    let printBool = function
                        | true -> "True"
                        | false -> "False"

    let solution = colors >> validate >> printBool

    //[<EntryPoint>]
    let main argv =
        let T = Console.ReadLine() |> int
        List.init T (fun _ -> 
        Console.ReadLine()
        |> solution
        |> printfn "%s"
        ) |> ignore
        0