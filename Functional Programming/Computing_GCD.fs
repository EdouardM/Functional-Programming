//--------------------
//Edouard Moureaux
//December 2015
//
// Problem URL: 
// https://www.hackerrank.com/challenges/functional-programming-warmups-in-recursion---gcd
//--------------------
namespace FunctionalProgramming.Recursion

module Computing_GCD =
    open System 

    /// Greatest common divisor
    let rec gcd x y = 
        if x = y then x
        elif x > y then gcd (x-y) y
        else gcd x (y - x)

    //[<EntryPoint>]
    let main argv =
        //input 2 integers separated by space
        let [|x; y|]= Console.ReadLine().Split[|' '|] |> Array.map int64
        printfn "%d" (gcd x y)
        0
