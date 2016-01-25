//--------------------
//Edouard Moureaux
//January 2016
//
// Problem URL: 
// https://www.hackerrank.com/challenges/missing-numbers-fp
//--------------------
namespace FunctionalProgramming.AdHoc

module Missing_Numbers = 
    open System

    let findMissing l1 l2 =
        let m1 = l1 |> Array.countBy id |> Map.ofArray
        let m2 = l2 |> Array.countBy id |> Map.ofArray

        m1 |> Map.filter ( fun k v -> 
                            match m2 |> Map.tryFind(k) with
                                | Some v' when v' > v -> true
                                | _                   -> false)
           |> Map.toList
           |> List.sortBy(fun (k,v) -> k)
           |> List.map (fst >> string)
           |> String.concat " "
        
    let readInput() = 
        let n = Console.ReadLine() |> int
        let l1 = Console.ReadLine().Split [| ' ' |] |> Array.map int
        let m = Console.ReadLine() |> int
        let l2 = Console.ReadLine().Split [| ' ' |] |> Array.map int
        (l1 ,l2)
    
    //[<EntryPoint>]
    let main argv = 
        let l1 , l2 = readInput()
        findMissing l1 l2 
        |> printfn "%s"
        |> ignore
        0