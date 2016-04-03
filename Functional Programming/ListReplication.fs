namespace ListReplication

    module ListReplication = 
        open System

        /// <summary>
        /// Function Repeat array N times
        /// </summary>
        /// <param name="n">nb of times</param>
        /// <param name="arr">array to repeat</param>
        let repeat n arr = arr |> Array.collect(fun i -> Array.create n i)
        
        let consoleReadInt() = 
            try
                Console.ReadLine() |> int |> Some
            with
                _ -> None
        
        let solution() =
            let n = match consoleReadInt() with
                        | Some v -> v
                        | None   -> 0

            Array.init 10 (fun i -> consoleReadInt())
            |> Array.choose id
            |> repeat n 
            |> Array.map (fun i -> printfn "%d" i)

                    
        //[<EntryPoint>]
        let main argv = 
            solution()
            0