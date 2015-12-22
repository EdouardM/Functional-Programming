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

    ///Permutre items in a list: Recursive
    let mingling l1 l2 = 
        let rec loop acc l1 l2 = 
            match l1 , l2 with
                | x::xs , y::ys -> 
                        let newacc = (string y)::(string x)::acc
                        loop newacc xs ys 
                | [], [] -> acc |> List.rev
                | _ , [] -> acc |> List.rev
                | [] , _ -> acc |> List.rev
        loop [] l1 l2
    
    let solution (a:string) (b:string) =
        let l1 = a |> List.ofSeq
        let l2 = b |> List.ofSeq
        mingling l1 l2
        |> String.concat ""

    ///Fold Solution: too slow: Need String concat optimization
    let mingling' (p:string) (q:string) =
        let arr1 = p |> Seq.toArray
        let arr2 = q |> Seq.toArray
        
        Array.fold2(fun acc a b -> acc + string a + string b) "" arr1 arr2

    //[<EntryPoint>]
    let main argv = 
        let p = Console.ReadLine()
        let q = Console.ReadLine()
        solution p q |> printfn "%s"
        0
