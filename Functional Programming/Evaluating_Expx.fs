//--------------------
//Edouard Moureaux
//December 2015
//--------------------
namespace FunctionalProgramming


module Solution =
    open System

    /// Power
    let pow x n = x ** n

    /// Factorial
    let rec fact n = match n with
                        | n when n <= 1.0 -> 1.0
                        | n -> n * fact (n - 1.0)

    /// Exponential expension
    let exp x = 
        [0.0 .. 9.0] 
        |> List.map(fun i -> (pow x i)  / (fact i) )
        |> List.fold(fun acc elem -> acc + elem) 0.0


    //[<EntryPoint>]
    let main argv = 
        let N = Console.ReadLine() |> int
        [1..N] 
        |> List.map(fun i -> Console.ReadLine() |> float)
        |> List.map(fun x -> exp x)
        |> List.iter (printfn "%.4f")
        0
    
