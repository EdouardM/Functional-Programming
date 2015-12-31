//--------------------
//Edouard Moureaux
//December 2015
//
// Problem URL: 
// https://www.hackerrank.com/challenges/area-under-curves-and-volume-of-revolving-a-curv
//--------------------
namespace FunctionalProgramming.Recursion

module Area_Under_Curve =
    open System

    //Split interval:
    let l = 0.001

    //Split interval:
    //Gives a list of points
    let split L R = 
            let n = ( (R - L) / l ) |> int
            List.init n (fun i -> 
                let x = float (i + 1) * l
                L + x , l
            )
        
    ///Read Console input: int array space separated
    let consoleRead() =  
            let str = Console.ReadLine()
            str.Split([|' '|])
            |> Array.map float

    ///Combines two function by summing their expression
    let inline sumFunc f g = (fun x -> f x + g x)

    ///Read input from challenge to build polynomial function
    let buildFunc() = 
        let coefs = consoleRead()
        let pows = consoleRead()
        Array.map2 (fun coef pow -> (fun x -> coef * (Math.Pow(x, pow)))) coefs  pows
        |> Array.reduce sumFunc 

    ///Area of disc of radius r
    let area r = Math.PI * r * r

    ///Computes integral over [L; R] of function f
    let integral f L R =
        let points = split L R
        points
        |> List.map (fun (p , dist) -> (f p) * dist)
        |> List.reduce (+)
    
    let volume f L R =
        let points = split L R
        points
        |> List.map (fun (p , dist) -> (f p |> area) * dist)
        |> List.reduce (+)
    
    
    //[<EntryPoint>]
    let main argv =
        let f = buildFunc()
        let [|L;R|] = consoleRead()
        printfn "%f" <| (integral f L R)
        printfn "%f" <| (volume f L R)
        0

    let sp = split 1. 4.
    let f = (fun x -> x * x * 1.)
    (integral f 1.0 4.0) |> printfn "%A";;
