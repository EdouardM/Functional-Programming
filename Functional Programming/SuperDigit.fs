//--------------------
//Edouard Moureaux
//March 2016
//
// Problem URL:
// https://www.hackerrank.com/challenges/super-digit 
//--------------------
namespace FunctionalProgramming.Recursion

module SuperDigit =
    open System
    open Checked 

    let charToint = 
        function 
            | '1' -> 1L | '2' -> 2L | '3' -> 3L
            | '4' -> 4L | '5' -> 5L | '6' -> 6L | '7' -> 7L
            | '8' -> 8L | '9' -> 9L
            | '0' -> 0L
            | c -> failwith <| sprintf "incorrect input: %c" c
   
    let (|OneDigit|SeveralDigits|) (input:string) =
        let arr = input.ToCharArray() |> Array.map charToint
        if arr.Length = 1 then OneDigit arr
        else SeveralDigits arr 

    let rec superDigit = 
            function
                | OneDigit arr -> arr |> Array.head
                | SeveralDigits arr -> 
                        let next = arr |> Array.map int64 |> Array.reduce (+) |> string
                        superDigit next


    let readInput() = 
        let [|input;k|] = Console.ReadLine().Split [| ' ' |]
        input, int64 k

    //[<EntryPoint>]
    let main argv = 
        readInput()
        |> (fun (input, k) -> (superDigit input) * k)
        |> string
        |> superDigit
        |> printfn "%d"
        |> ignore
        0

//3546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736 100000
//5