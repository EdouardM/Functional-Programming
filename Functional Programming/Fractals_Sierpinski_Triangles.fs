//--------------------
//Edouard Moureaux
//December 2015
//
// Problem URL: 
// https://www.hackerrank.com/challenges/functions-and-fractals-sierpinski-triangles
//--------------------
namespace FunctionalProgramming.Recursion

module Fractals_Sierpinski_Triangles =
    open System

    type Position = { X : float ; Y : float }

    ///Get Middle of 2 positions
    let getmiddle p1 p2 =
        let xm = ( p1.X + p2.X )/ 2.0 
        let ym = ( p1.Y + p2.Y )/ 2.0
        { X = xm ; Y = ym}
    
    ///Control over signs
    type Caractere = | UnderScore | One
        with
            override x.ToString() = 
                    match x with
                        | UnderScore -> "_" | One -> "1"
    
    ///One point is one position and only one sign
    type Point = 
        {
            Pos : Position
            Car : Caractere
        }
        with
            override x.ToString() = x.Car.ToString()
           
    type Triangle = 
        { 
            Vertices : Position list 
            Car: Caractere
        }
        with
            member x.Area() =
                    let {Vertices = [A;B;C]} = x
                    ( A.X * (B.Y - C.Y) + B.X * (C.Y - A.Y) + C.X * (A.Y - B.Y) ) / 2.0 |> Math.Abs

            member x.Contains P =
                let {Vertices = [A;B;C]} = x
                let a = x.Area()
                let a1 = {x with Vertices = [P; B; C]}.Area()
                let a2 = {x with Vertices = [P; A; C]}.Area()
                let a3 = {x with Vertices = [P; A; B]}.Area()
                a = (a1 + a2 + a3)
            ///Compute 3 triangles out of one:
            member x.Fractalize() =
                match x.Car with
                    | One -> 
                        let [A; B; C] = x.Vertices
                        let m1 = getmiddle A B
                        let m3 = getmiddle A C
                        let m2 = getmiddle B C
        
                        //Triangle downward with UnderScore:
                        let t  = {Vertices = [m1; m2; m3] ; Car = UnderScore }
                
                        //3 smaller upward triangles with One:
                        let T1 = {x with Vertices = [A; m1; m3] }
                        let T2 = {x with Vertices = [m1; B; m2] }
                        let T3 = {x with Vertices = [C; m3; m2] }
                            
                        //Return list of triangle
                        [T1; T2; T3; t]
                    | UnderScore -> [x]
            ///Draw one triangle
            ///Returns grid of points
            member x.Draw grid =
                Array2D.map(fun p -> 
                        if x.Contains( p.Pos ) then
                            { p with Car = x.Car}
                        else p
                    ) grid
       
    ///Grid Size
    let width = 63
    let height = 32 

    
    ///Modify the grid for every Triangle:
    let rec drawTriangles grid (l: Triangle list) =
        match l with
            | [] -> grid
            | h::tl -> 
                let newgrid = h.Draw grid
                drawTriangles newgrid tl

    ///Fractalize triangle: 
    let rec fractalize (l : Triangle list) =
        function
            | 0 -> l
            | n -> 
                let newlist = 
                    l 
                    |> List.collect(fun T -> T.Fractalize())
                    |> List.sortByDescending (fun T -> T.Car)
                fractalize newlist (n - 1) 
        
    ///Print grid:
    let printgrid grid =
        let lim = ( Array2D.length2 grid ) - 1
        let str = grid |> Array2D.map (fun elem -> elem.ToString() )
        
        for j in [0.. lim ] do 
            printfn "%s" <| String.concat "" str.[*, (lim - j)]

    let iterate n =
        
        let A = { X = 0.0; Y = 0.0 }
        let B = { X = 62.0; Y = 0.0 }
        let C = { X = 31.0; Y = 31.0 }
        
        //initial triangle
        let T = { Vertices = [A;B;C] ; Car = One}

        //initial grid
        let grid = Array2D.init width height (fun x y -> { Pos = { X = float x ; Y = float y } ; Car = UnderScore} )

        match n with
            | 0 -> 
                   drawTriangles grid [T] |> printgrid
            | n' when n > 0 -> 
                   n |> fractalize [T] |> drawTriangles grid |> printgrid
            | _ -> printfn "Error: n must be positive" 
        

    //[<EntryPoint>]
    let main argv =
        let n = Console.ReadLine() |> int
        iterate n  |> ignore
        0
