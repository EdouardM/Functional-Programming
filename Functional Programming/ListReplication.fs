namespace ListReplication

    module Solution = 
        open System

        /// <summary>
        /// Function Repeat array N times
        /// </summary>
        /// <param name="n">nb of times</param>
        /// <param name="arr">array to repeat</param>
        let repeat n arr = arr |> Array.collect(fun i -> Array.create n i)
        
        let consoleReadInt() = Console.ReadLine() |> int

        let solution() =
            let n = consoleReadInt()
            Array.init 10 (fun i -> consoleReadInt())
            |> repeat n 
            |> printfn "%A"
            |> ignore

                
    module Program =
        open Solution
        
        //[<EntryPoint>]
        let main argv = 
            solution()
            0