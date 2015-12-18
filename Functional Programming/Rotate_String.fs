//--------------------
//Edouard Moureaux
//December 2015
//
// Problem URL: 
// https://www.hackerrank.com/challenges/rotate-string
//--------------------
namespace FunctionalProgramming.AdHoc

module Rotate_String =
    open System

    ///Rotate array
    let rotate arr = 
        let h = arr |> Array.head
        let tail = arr |> Array.tail
        
        Array.concat [tail; [|h|] ]

    /// String rotation
    let rotation (s: string) = 
        let arr = s.ToCharArray() |> Array.map string
        
        let res= ref arr
        //rotation
        [1..arr.Length]
        |> List.map(fun i -> 
                        res := rotate !res
                        !res)
        //compose string again
        |> List.map(fun l -> l |> Array.reduce (+))

    /// Print list members on one line
    let printlist = List.reduce(fun elem1 elem2 -> elem1 + " " + elem2)

    ///Main program:
    //[<EntryPoint>]
    let main argv = 
        let n = Console.ReadLine() |> int
        Array.init n (fun i -> 
                            let str = Console.ReadLine()
                            rotation str)
        |> Array.map printlist
        |> Array.map (printfn "%s")
        |> ignore
        0
       
        
