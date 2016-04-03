//--------------------
//Edouard Moureaux
//December 2015
//--------------------
namespace FunctionalProgramming

module FilterPosition =
    open System

    let consoleReadInt() = Console.ReadLine() |> int

    let solution() = 
        let n = consoleReadInt()
        List.init n (fun i -> (i, consoleReadInt()) )
        |> List.filter(fun (i, x) -> i % 2 = 0)



