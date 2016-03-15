//--------------------
//Edouard Moureaux
//March 2016
//
// Problem URL:
// https://www.hackerrank.com/challenges/super-queens-on-a-chessboard
//--------------------
namespace FunctionalProgramming.Recursion

module Super_Queens =
    open System

    let issafe (xQ, yQ) (x ,y) =
        let difX = (x - xQ) |> float |> abs
        let difY = (y - yQ) |> float |> abs

        //Vertical row
        (x <> xQ)
        // Horizontal row
        && (y <> yQ)
        //Square around
        && (difX > 2. || difY > 2.)
        //Diagnoals
        && (difX <> difY)

    /// <summary>
    /// Given a list of super queens positions
    /// tells if position is in danger of safe
    /// </summary>
    /// <param name="queens">list of queens positions</param>
    /// <param name="x">position X coord</param>
    /// <param name="y">position Y coord</param>
    let (|Danger|) queens (x, y) =
        queens 
        |> List.forall(fun (xQ, yQ) -> issafe (xQ, yQ) (x, y))
        |> not
        
    let buildGrid n =  [ for i in [1 .. n] do for j in [1 .. n] -> (i, j) ]
    
    let findpositions n =
        let grid = buildGrid n
           
        let rec loop grid n (acc: (int * int) list) =
            let newGrid = 
                grid |> List.filter(function | Danger acc true -> false | _ -> true)
            
            //We want to place queen on every row
            //In acc there is current positions list 1 by row
            let nextPos = newGrid |> List.filter(fun (x,y) -> x = acc.Length + 1) 

            if List.isEmpty nextPos then
                //Add one if there are as many queens positions as rows & columns
                if acc.Length = n then 1 else 0
            else
                nextPos
                |> List.map(fun (x,y) -> 
                    let newAcc = (x,y)::acc               
                    loop newGrid n newAcc )
                |> List.reduce (+)
        grid
        //We start at row 1 and browse only row 1:
        |> List.filter(fun (x, y) -> x = 1)
        |> List.map(fun (x,y) -> loop grid n [(x,y)])
        |> List.reduce (+)

    let solution = 
        Console.ReadLine
        >> int
        >> findpositions
        >> printfn "%d"
        >> ignore

    //[<EntryPoint>]
    let main argv =
        solution()
        0