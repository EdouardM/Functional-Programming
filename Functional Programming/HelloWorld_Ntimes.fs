//--------------------
//Edouard Moureaux
//December 2015
//--------------------
namespace FunctionalProgramming

open System

module Program = 
    
    let solution() = 
        let n = Console.ReadLine() |> int
        [1..n] |> List.iter (fun i -> printfn "Hello World")

    //[<EntryPoint>]
    let main argv = 

        solution()
        0

