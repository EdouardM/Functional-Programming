//--------------------
//Edouard Moureaux
//December 2015
//
// Problem URL: 
// https://www.hackerrank.com/challenges/string-mingling/submissions/code/15741105
//--------------------
namespace FunctionalProgramming.Recursion

module String_Mingling = 
    open System

    let mingling (p:string) (q:string) =
        let arr1 = p |> Seq.toArray
        let arr2 = q |> Seq.toArray
        
        Array.fold2(fun acc a b -> acc + string a + string b) "" arr1 arr2

    //[<EntryPoint>]
    let main argv = 
        let p = Console.ReadLine()
        let q = Console.ReadLine()
        mingling p q |> printfn "%s"
        0
