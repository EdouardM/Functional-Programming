//--------------------
//Edouard Moureaux
//January 2016
//
// Problem URL: 
// https://www.hackerrank.com/challenges/string-reductions
//--------------------
namespace FunctionalProgramming.Recursion

module String_Reduction =
    open System

    ///Add count to char if equals last char in compr list
    ///Else add char with count 1
    let reduction (str :string)= 
        str.ToCharArray()
        |> Array.fold(fun (acc: Map<char,int>) char -> 
                                if acc.ContainsKey(char) then
                                    acc
                                else
                                    let pos = acc.Count
                                    acc.Add(char ,pos)
                                ) Map.empty
        |> Map.toArray
        |> Array.sortBy(fun (_, pos) -> pos)
        |> Array.map (fun (char, _) -> string char)
        |> String.concat ""

    //[<EntryPoint>]
    let main argv =
        Console.ReadLine()
        |> reduction
        |> printfn "%s"
        |> ignore
        0