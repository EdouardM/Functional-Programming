//--------------------
//Edouard Moureaux
//March 2016
//
// Problem URL: 
// https://www.hackerrank.com/challenges/swap-nodes
//--------------------
namespace FunctionalProgramming.Structures

module Swap_Nodes =
    open System 

    type Tree<'a> =
        | Leaf
        | Node of 'a * Tree<'a> * Tree<'a>

    //Inorder traversal & depth
    let rec traverse tree =
        seq{
            match tree with
                | Leaf -> ()
                | Node (hd, l, r) -> yield! traverse l; yield hd; yield! traverse r
            }
        
    //Swap one node or leaf
    let swap tree = 
        match tree with
            | Leaf -> Leaf
            | Node (hd, l, r) -> Node(hd, r, l)

    let swapNodes k tree =
        let rec loop d k tree = 
            //d is multiple of k
            if d % k = 0 then swap tree
            else
               match tree with
                    | Leaf -> Leaf
                    | Node (hd, l, r) -> Node( hd, loop (d + 1) k l , loop (d + 1) k r)
        loop 1 k tree
        
    let buildTree (arr: (int * int) [])  = 
        let rec loop (arr: (int * int) []) k =
            let l, r = arr.[k]
            match l, r with
                | -1, -1    -> Node(k + 1, Leaf, Leaf)
                | l, -1     -> Node(k + 1 , loop arr (l - 1) , Leaf)
                | -1, r     -> Node(k + 1 , Leaf, loop arr (r - 1))
                | l , r     -> Node(k + 1 , loop arr (l - 1) , loop arr (r - 1))
        loop arr 0

    let readInput() =
        let n = Console.ReadLine() |> int
        Array.init n (fun i -> 
            let [|l; r|] = Console.ReadLine().Split [|' '|] 
            int l, int r
        )
