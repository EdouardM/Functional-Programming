//--------------------
//Edouard Moureaux
//January 2016
//
// Problem URL: 
// https://www.hackerrank.com/challenges/functional-programming-the-sums-of-powers
//--------------------
namespace FunctionalProgramming.Recursion

module Sum_Powers =
    open System

    let powers n x = Seq.unfold(fun i -> 
                            let pow = pown i n 
                            if pow <= x then
                                Some (pow, i + 1)
                            else None ) 1

    //http://www.geeksforgeeks.org/dynamic-programming-subset-sum-problem/
    let rec subsetSum l target acc = 
        if target = 0 then
            //printfn "subset found: %A" acc
            1
        elif l = [] && target <> 0 then
            0
        else
            match l with
                | [] -> 0
                | x::xs -> 
                    if x > target then
                        subsetSum xs target acc
                    else
                        let acc' = x::acc
                        subsetSum xs target acc + (subsetSum xs (target - x)  acc')
                        
    let solution n target =
        let pows = powers n target |> Seq.toList
        subsetSum pows target []

    //[<EntryPoint>]
    let main argv =
        let target = Console.ReadLine() |> int
        let n = Console.ReadLine() |> int
        solution n target
        |> printfn "%d"
        0