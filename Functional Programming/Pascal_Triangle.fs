//--------------------
//Edouard Moureaux
//December 2015
//
// Problem URL: 
// https://www.hackerrank.com/challenges/functional-programming-warmups-in-recursion---gcd
//--------------------
namespace FunctionalProgramming.Recursion


module Pascal_Triangle =
    open System
    
    /// Factorial function
    let fact = function
        | 0 -> 1
        | n -> [1..n] |> List.reduce (*)

    /// Binomial Coefficient
    let binomial n r = (fact n) / (fact r * fact (n - r) )

    /// Row of Pascal triangle 
    let triangleRow n = [0..n] |> List.map(fun i -> binomial n i)

    /// Print list with elem separated by space
    let printRow l = 
        l 
        |> List.map string 
        |> List.reduce(fun acc elem -> acc + " " + elem)

    //[<EntryPoint>]
    let main argv =
        let k = Console.ReadLine() |> int
        [0..(k - 1)] 
        |> List.map triangleRow
        |> List.map printRow
        |> List.map (printfn "%s")
        |> ignore
        0



