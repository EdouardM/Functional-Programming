//--------------------
//Edouard Moureaux
//December 2015
//
// Problem URL: 
// https://www.hackerrank.com/challenges/huge-gcd-fp
//--------------------

namespace FunctionalProgramming.AdHoc
module Huge_GCD =
    open System

    ///Even or Odd pattern
    let (|Even|Odd|) (n: bigint) = 
                        match n with
                          | n when n.IsEven -> Even n
                          | _ -> Odd n


    /// Greatest common divisor: Euclidian Method - too slow
    let rec gcd x y = 
        if x = y then x
        elif x = 1I || y = 1I then 1I
        elif x > y then gcd (x-y) y
        else gcd x (y - x)

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

    ///Compute number out of product terms space separated
    let readNumber() = 
            let arr = Console.ReadLine()
            arr.Split[|' '|]
            |> Array.map (int64 >> tobigint)
            |> Array.reduce (*)

    //[<EntryPoint>]
    let main argv =
        let n = Console.ReadLine() |> int
        let A = readNumber()
        let m = Console.ReadLine() |> int
        let B = readNumber()
        let g = gcdBin A B
        
        printfn "%A" <| (g % (pown 10I 9 + 7I))
        0