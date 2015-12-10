//--------------------
//Edouard Moureaux
//December 2015
//--------------------
namespace FunctionalProgramming

open System

module Program =
    //[<EntryPoint>]
    let main argv = 
    
        let a = Console.ReadLine() |> int
        let b = Console.ReadLine() |> int
        printfn "%d" (a+b)
        0