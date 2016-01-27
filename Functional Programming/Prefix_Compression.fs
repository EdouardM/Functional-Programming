//--------------------
//Edouard Moureaux
//January 2016
//
// Problem URL: 
// https://www.hackerrank.com/challenges/prefix-compression
//--------------------
namespace FunctionalProgramming.Recursion

module Prefix_Compression =
    open System

    let charArrays (str1:string) (str2:string) = 
        let l1 = str1.ToCharArray().Length
        let l2 = str2.ToCharArray().Length
        if l1 <= l2 then
            (str1.ToCharArray() , str2.ToCharArray().[0..(l1 - 1)])
        else
            (str1.ToCharArray().[0..(l2 - 1)] , str2.ToCharArray())
    
    let prefix (str1 :string) (str2 : string) = 
        charArrays str1 str2
        ||> Array.zip
        |> Array.takeWhile( fun (c1, c2) -> c1 = c2 )
        |> Array.map (fun (c, _) -> string c)
        |> String.concat ""
        
    let sub n (str1: string) =
        let l1 = str1.ToCharArray().Length - n
        l1, str1.Substring(n)

    let solution str1 str2 = 
        let p = prefix str1 str2
        let n = p.ToCharArray().Length
        [(n , p) ; sub n str1 ; sub n str2]

    //[<EntryPoint>]
    let main argv =
        let str1 = Console.ReadLine()
        let str2 = Console.ReadLine()
        solution str1 str2
        |> List.map (fun (n, w) -> printfn "%d %s" n w)
        |> ignore
        0