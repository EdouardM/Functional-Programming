//--------------------
//Edouard Moureaux
//December 2015
//--------------------
namespace FunctionalProgramming

open System

module SolveMeFirst =
    //[<EntryPoint>]
    let main argv = 
    
        let a = Console.ReadLine() |> int
        let b = Console.ReadLine() |> int
        printfn "%d" (a+b)
        0