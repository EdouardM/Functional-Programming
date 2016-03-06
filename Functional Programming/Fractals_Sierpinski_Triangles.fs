//--------------------
//Edouard Moureaux
//February 2016
//
// Problem URL:
// https://www.hackerrank.com/challenges/functions-and-fractals-sierpinski-triangles
//--------------------
namespace FunctionalProgramming.Recursion

module Triangles =
    open System

    type Rectangle = { Origin : int * int ; Width : int ; Height : int }

    let drawTriangle rect =

        let left, bottom = rect.Origin
        let right, top = left + rect.Width , bottom + rect.Height

        [|   for j in [bottom .. top] do
                let k = j - bottom
                for i in [left .. right] do
                    if i < (left + k) || i > (right - k) then
                        yield (i,j) , "_"
                    else
                        yield (i,j) , "1"|]

    let splitRect rect =
        let width' = rect.Width / 2 - 1
        let height' = rect.Height / 2

        let o1 = rect.Origin
        let r1 = { Origin = o1 ; Width = width' ; Height = height'}

        let o2 = fst rect.Origin + width' + 2 , snd rect.Origin
        let r2 = { Origin = o2 ; Width = width' ; Height = height'}

        let o3 = fst rect.Origin + width' / 2  + 1, snd rect.Origin + height' + 1
        let r3 = { Origin = o3 ; Width = width' ; Height = height'}

        [|r1; r2; r3|]


    let rec fractalize (l: Rectangle [] ) =
        function
            | 0 -> l
            | n ->
                let newList = l |> Array.collect splitRect
                fractalize newList (n - 1)

    let printgrid map =
            let grid = Array2D.init 63 32 (fun i j -> i, j)
            let str =
                Array2D.map (fun (x, y) ->
                        match Map.tryFind(x,y) map with
                          | None -> "_"
                          | Some v -> v ) grid

            for j in [0.. 31 ] do
                printfn "%s" <| String.concat "" str.[*, (31 - j)]

    let iterate n =
        let rect = { Origin = (0, 0) ; Width = 62 ; Height = 31 }

        fractalize [|rect|] n
        |> Array.collect drawTriangle
        |> Map.ofArray
        |> printgrid

    //[<EntryPoint>]
    let main argv = 
        let n = Console.ReadLine() |> int
        iterate n
        0