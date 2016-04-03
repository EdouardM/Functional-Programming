//--------------------
//Edouard Moureaux
//March 2016
//
// Algo URL: 
// https://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Convex_hull/Monotone_chain
//--------------------
namespace FunctionalProgramming.Recursion

module Convex_Hull =
    open System

    type Point = 
        { X : int; Y : int }
        with
            static member (+) (a: Point, b: Point) = { X = a.X + b.X; Y = a.Y + b.Y }
            static member (-) (a: Point, b: Point) = { X = a.X - b.X; Y = a.Y - b.Y }
            static member (<*>) (a: Point, b: Point) = a.X * b.X + a.Y * b.Y
            //Z coordinate of cross product
            static member (<^>) (a: Point, b: Point) = a.X * b.Y - a.Y * b.X
            member x.Norm() = (x.X * x.X + x.Y * x.Y) |> float |> sqrt
            member x.Tuple() = x.X, x.Y
       
    let clockwise (p1, p2, p3) = (p2 - p1)<^>(p3 - p1) <= 0
    

    let rec chain (hull: Point list) (candidates: Point list) =
       match candidates with
       | [ ] -> hull
       | c :: rest ->
          match hull with
          | [ ] -> chain [ c ] rest
          | [ start ] -> chain [c ; start] rest
          | b :: a :: tail ->          
             if clockwise(a,b,c) then chain (c :: hull) rest else
             chain (a :: tail) candidates

    let convexHull (points: Point list) =
       match points with
       | [ ] -> points
       | [ _ ] -> points
       | _ ->
           let sorted = List.sort points
           let upper = chain [ ] sorted
           let lower = chain [ ] (List.rev sorted)
           List.append upper lower

    let readInput() =
        let n = Console.ReadLine() |> int
        List.init n (fun _ -> 
            let [|x;y|] = 
                Console.ReadLine().Split [|' '|]
                |> Array.map (string >> int)
            { X = x ; Y = y}
        )

    let perimeter (points: Point list) =
        points
        //Reverse list to stop folding at the same point as initial state
        |> List.rev
        |> List.fold(fun state pos2 -> 
            let (pos1,perimeter) = state
            let v = (pos1 - pos2)
            pos2, perimeter + v.Norm()
        ) (points.Head, 0.)

    let solution =
        readInput
        >> convexHull
        >> perimeter
        >> snd
        >> printfn "%.1f"

