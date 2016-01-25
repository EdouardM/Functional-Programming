//--------------------
//Edouard Moureaux
//December 2015
//
// Problem URL: 
// https://www.hackerrank.com/challenges/string-compression
//--------------------
namespace FunctionalProgramming.Recursion

module String_Compression =
    open System

    ///Add count to char if equals last char in compr list
    ///Else add char with count 1
    let compress (str :string)= 
        str.ToCharArray()
        |> Array.fold(fun compr char -> 
                                match compr with
                                    | (ch, ct)::tl ->
                                        if char = ch then 
                                            (ch, ct + 1)::tl
                                        else
                                            (char, 1)::(ch, ct)::tl
                                    | [] -> (char, 1)::[] ) []
        |> List.rev
        |> List.map( fun (ch, ct) -> 
                            let char = string ch 
                            let count = if ct > 1 then string ct else ""
                            char + count)
        |> String.concat ""

    //[<EntryPoint>]
    let main argv =
        Console.ReadLine()
        |> compress
        |> printfn "%s"
        |> ignore
        0