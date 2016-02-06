//--------------------
//Edouard Moureaux
//December 2015
//
// Problem URL: 
// https://www.hackerrank.com/challenges/crosswords-101
//--------------------
namespace FunctionalProgramming.Recursion

module CrossWord_101 =
    open System
    open System.Collections.Generic

    type CrossLetter = { Position : int; Letter : char option; Words : Word list }
    and Word = 
        {   
            Text : string option; 
            Length: int; 
            CrossLetters: CrossLetter []; 
            WordPositions : (int * int) [] }
    and Direction = | Right | Down   
    
    ///Check if all elements are consecutive
    let consecArray l =
        let rec loop acc expect =
            function
                | [] -> 
                    acc 
                    |> List.map (List.rev)
                | x::xs -> 
                    match expect with
                        | None -> 
                                let expect' = Some (x + 1)
                                loop ([x]::acc) expect' xs
                        | Some v -> 
                                if x = v then
                                  let expect' = Some (x + 1)
                                  let consec = x::acc.Head
                                  loop (consec::acc.Tail) expect' xs
                                else
                                  let expect' = Some (x + 1)
                                  loop ([x]::acc) expect' xs
        loop [] None l
        |> List.filter(fun l -> l.Length > 1)
        |> List.map(fun l -> List.toArray l)
        |> List.toArray

    ///Reads the grid input with '-' for empty slot
    let readInput () = 
            Array.init 10 (
                fun i -> 
                    let str = Console.ReadLine()
                    str.ToCharArray()
                    |> Array.mapi(fun j c -> (i, j), c)
                    |> Array.filter(fun (_ , c) -> c = '-')
                    |> Array.map fst )
            |> Array.collect id
       
    let filterCoord = 
        function
            | Right -> snd
            | Down -> fst
    
    let rebuildCoord key dir coord  =
        match dir with
            | Right -> Array.map(fun c -> (key, c) ) coord
            | Down -> Array.map(fun c -> (c, key) ) coord

    let filterGroup = 
        function
            | Right -> fst
            | Down -> snd

    let filterWords direction grid =
        grid
        |> Array.groupBy (filterGroup direction)
        //Split into groups of consecutive elements
        |> Array.collect(fun (key, grp) -> 
                            grp
                            |> Array.map (filterCoord direction)
                            |> Array.toList
                            |> consecArray
                            |> Array.map (rebuildCoord key direction)
                            )
    
    let buildWord grp =
        {
            Text = None;
            Length = grp |> Array.length;
            CrossLetters = [||];
            WordPositions = grp
        }

    ///Scan grid Rows and build Words:
    let rowsToWord grid = 
        filterWords Right grid
        |> Array.map buildWord
    
    ///Scan grid Cols and build Words:    
    let colsToWord grid = 
        filterWords Down grid
        |> Array.map buildWord
       
    ///Concat Rows words & Cols words
    let allWords grid = Array.concat [ rowsToWord grid; colsToWord grid ]

    ///Add Cross Letters based on Positions
    let addCrossPositions (words: Word list) (w: Word)=
        let wpos = w.WordPositions |> Array.indexed
        let letters = 
            [| for (i, p) in wpos do
                    //Look if one position exist in another word of the list
                    let l = 
                        words 
                        |> List.filter 
                            (fun word -> word <> w && word.WordPositions |> Array.contains p)
                    //Return Cross Letter:
                    if l.Length  > 0 then 
                        yield { Position = i ; Letter = None ; Words = l}
            |]
        //Add letters to word
        { w with CrossLetters = letters }


    ///Builds dictionnary of letters and words containing the letter
    let letterDict (words:Word list) =
       ['A'..'Z'] 
       |> List.map 
            (fun char -> 
                char, 
                    words 
                    |> List.filter
                        (fun word -> 
                            match word.Text with
                                | Some t -> t.Contains (string char)
                                | None -> false))
       |> Map.ofList    

    //Transform string to Word
    let strToWord (words:#seq<string>) =
        [
            for word in words do
               yield {
                        Text = word |> Some; 
                        Length = word.Length; 
                        CrossLetters = [||] ; 
                        WordPositions = [||]
                }
        ]

    ///Add Cross Letters based on Letters
    let addCrossLetters (w: Word) (dict:Map<char, Word list>) =
        let txt = Option.get(w.Text)
        let chars = txt.ToCharArray() |> Array.indexed
        let letters = 
            [| for (i, c) in chars do 
                    //Look if one position exist in another word of the list
                    match dict.TryFind(c) with
                        | Some l ->
                            //remove txt:
                            let l' = l |> List.filter(fun x -> x <> w)
                            if l'.Length > 0 then 
                                yield { Position = i ; Letter = Some c ; Words  = l' }
                        | None -> ()
            |]
        //Add letters to word
        { w with CrossLetters = letters }


    
    let extractCriteria (word : Word) =
        word.CrossLetters
        |> Array.map(fun cL -> 
            cL.Letter, cL.Position, cL.Words.Head)

    let checkLetter l l' = 
        match l' with
            | Some c -> l = l'
            | None -> true

    //Check the cross based on pos & lengths of Words
    let checkCrossLetter criteria (word: Word) =
        let cL = word.CrossLetters
        criteria
        |> Array.forall
            (fun (letter, pos, word) -> 
                cL |> Array.exists(fun cl -> 
                    (checkLetter cl.Letter letter) && cl.Position = pos && cl.Words |> List.exists(fun x -> x.Length = word.Length))
            )

    //Assign Text to Solution: Breaks tie:
    let uniqueText (words: Word list) =
            words
            |> List.groupBy(fun w -> Option.get w.Text)
            |> List.filter(fun (k, wl) -> wl.Length = 1)
            |> List.map(fun (k,wl) -> wl.Head.WordPositions, k)
            |> Map.ofList
    
    //Assign Text to CrossLetter Word & char to CrossLetter Letter
    let assignText (words: Word list) (word: Word) =
        let txtMap = uniqueText words

        let cross = 
            word.CrossLetters
            |> Array.map(
                fun cL -> 
                    let pos = word.WordPositions.[cL.Position]
                    let w = cL.Words.Head
                    let index = w.WordPositions |> Array.findIndex(fun p -> p = pos)

                    let txtopt = txtMap.TryFind(w.WordPositions)

                    //Extraire
                    let letter = match txtopt with
                                    | Some txt -> txt.ToCharArray().[index] |> Some
                                    | None -> None 
                                     
                    {cL with Letter = letter; Words = [{w with Text = txtopt}]})

        {word with CrossLetters = cross}

    //Check if CrossLetter char is the same in both words
    let validateCrossLetter word =
        match word.Text with
            | Some txt -> 
                let arr = txt.ToCharArray()
                word.CrossLetters
                |> Array.forall(fun cL -> 
                    match cL.Letter with
                        | Some l -> l = arr.[cL.Position]
                        | None -> true)
            | None -> true
   
    //Check if position is not already taken in valid solutions:
    let validatePosition (map: Map<(int * int) [], string>) word =
        match map.TryFind(word.WordPositions) with 
            | Some txt -> txt = Option.get word.Text
            | None -> true

    let validWords map word = 
        (validateCrossLetter word) && (validatePosition map word)

    //Helper to see conflict between words on a given position
    let visu words =
        words
        |> List.groupBy(fun w -> w.Text)
        |> List.map(fun (k, wl) -> k, wl |> List.map(fun w -> w.WordPositions) )

    //
    let findWords (wpos: Word list) (wlet: Word list) =       
            wpos
            |> List.collect
                (fun w -> 
                   let criteria = extractCriteria w
                   wlet
                   //1st criteria: length
                   |> List.filter(fun wl -> wl.Length = w.Length)
                   //2nd criteria: Has CrossLetters in Same position
                   |> List.filter(fun wl -> checkCrossLetter criteria wl)
                   |> List.map(fun wl -> { w with Text = wl.Text})
            )
        
    let findSolutions wpos wlet =
        let Ws = findWords wpos wlet
        let unique = uniqueText Ws
        List.map(fun w -> assignText Ws w) Ws
        |> List.filter (fun w -> validWords unique w)


    let wordToCoord (word: Word) =
        let txt = Option.get word.Text
        txt.ToCharArray()
        |> Array.mapi(fun i c ->  word.WordPositions.[i], c)
        |> Array.toList

           
    let writeOutput (grid : Map<(int * int),char>) =
        for i in 0..9 do
            let line = [| for j in 0..9 do
                            match grid.TryFind(i,j) with
                                | Some c -> yield string c
                                | None -> yield "+" |]
                       |> String.concat ""
                   
            printfn "%s" line
          
    //[<EntryPoint>]   
    let main argv =
        let g = readInput()
        let l = Console.ReadLine().Split [| ';' |]
        let w = allWords g |> Array.toList
        let w' = List.map(fun x -> addCrossPositions w x) w
        let W = strToWord l
        let D = letterDict W
        let W' = List.map (fun x -> addCrossLetters x D) W
        findSolutions w' W'
        |> List.collect wordToCoord
        |> Map.ofList
        |> writeOutput
        0        
