namespace ListReplication

    module RepeatList = 
        open System

        let repetition nb l = 
                let rec repet nb l acc = 
                        match l with
                        | i::is ->
                            let acc = acc@[ for j in [0..nb-1] do
                                            yield i
                                        ]
                            repet nb is acc
                        | [] -> acc
                repet nb l []

    module Program =
        open System
        open RepeatList

        let consoleRead() = try
                                Some(Console.ReadLine() |> int)
                            with
                                ex-> 
                                        printfn "Erreur Entrée clavier: %s" ex.Message
                                        None

        let readInput() =
            let nb = consoleRead()
            let length = consoleRead()
            match (length, nb) with
            | Some(ln), Some(n) when (n<=100 && n>=0) -> Some(n, List.init ln (fun index -> Console.ReadLine() |> int))
            | _ -> None

        //[<EntryPoint>]
        let main argv = 
            let input =  readInput()
            match input with 
                | Some(n, l) -> 
                            let newList = repetition n l
                            newList |> List.map( fun i -> printfn "%d" i) |> ignore
                | None -> printfn "Bad Input"
            0