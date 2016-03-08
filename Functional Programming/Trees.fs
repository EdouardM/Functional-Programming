//--------------------
//Edouard Moureaux
//March 2016
//
// Problem URL:
// https://www.hackerrank.com/challenges/functions-and-fractals-sierpinski-triangles
//--------------------
namespace FunctionalProgramming.Recursion

module Trees = 
    open System

    type Rectangle = { Origin : int * int ; Width : int ; Height : int }
    type Tree = { Origin: int * int ; Length : int }

    let drawTree line =
        let middle, bottom = line.Origin
        let node = bottom + line.Length - 1
        let left, right = middle - line.Length , middle + line.Length
        let top = node + line.Length
        [|
            for j in [bottom .. top ] do
                for i in [left .. right ] do
                    //From bottom to node, vertical line in middle
                    if j < node && i = middle then yield (i , j) , "1"
                    //From node to top, 1 diagonal lines to the left
                    if j >= node && i = middle - (j - node) then yield (i, j) , "1"
                    //From node to top, 1 diagonal lines to the right
                    if j >= node && i = middle + (j - node) then yield (i , j), "1" |]

    let growTree tree =
        let xO, yO = tree.Origin
        let topleft , topright = 
            (xO - tree.Length, yO + tree.Length * 2) , (xO + tree.Length, yO + tree.Length * 2)
        let tree1 = { Origin = topleft; Length = tree.Length / 2}
        let tree2 = { Origin = topright; Length = tree.Length / 2}

        [|tree; tree1; tree2|]

    let rec fractalize (l: Tree [] ) =
        function
            | 0 -> l
            | n ->
                let grown = l |> Array.collect growTree
                fractalize grown (n - 1)

    let printgrid map =
            let grid = Array2D.init 100 63 (fun i j -> i, j)
            let str =
                Array2D.map (fun (x, y) ->
                        match Map.tryFind(x,y) map with
                          | None -> "_"
                          | Some v -> v ) grid

            for j in [0.. 62] do
                printfn "%s" <| String.concat "" str.[* , 62 - j]

    let iterate n =
        let tree = { Origin = (49, 0) ; Length = 16}

        fractalize [|tree|] n
        |> Array.collect drawTree
        |> Map.ofArray
        |> printgrid

    //[<EntryPoint>]
    let main argv =
        let n = Console.ReadLine() |> int
        iterate n
        0