//--------------------
//Edouard Moureaux
//January 2016
//
// Problem URL: 
// https://www.hackerrank.com/challenges/jumping-bunnies/
//--------------------
namespace FunctionalProgramming.AdHoc

module Jumping_Bunnies =
    open System

    ///Even or Odd pattern
    let (|Even|Odd|) (n: bigint) = 
                        match n with
                          | n when n.IsEven -> Even n
                          | _ -> Odd n

    /// Greatest common divisior: Binary Method
    /// -- https://en.wikipedia.org/wiki/Greatest_common_divisor
    let gcdBin x y =
        let rec loop d x y =
            if x = y then (x , d)
            else
                match x, y with
                    | Even a, Even b -> loop (d + 1) (a/2I) (b/2I)
                    | Even a, Odd b -> loop d (a/ 2I) b
                    | Odd a, Even b -> loop d a (b/2I)
                    | Odd a, Odd b -> 
                            if a > b then loop d <| (a - b)/2I <| b
                            else loop d a <| (b - a)/2I
        let g, d = loop 0 x y
        g * (pown 2I d)

    let tobigint (x:int64) = bigint x


    // Smallest Common multiple
    let ppcm x y = abs (x * y) / (gcdBin x y) 

    let readInput () =
        let n = Console.ReadLine()
        let str = Console.ReadLine()
        str.Split[|' '|]
        |> Array.map (int64 >> bigint)
        
    //[<EntryPoint>]
    let main argv = 
        readInput()
        |> Array.sort
        |> Array.reduce(fun elem1 elem2 -> ppcm elem1 elem2)
        |> printfn "%A"
        |> ignore
        0