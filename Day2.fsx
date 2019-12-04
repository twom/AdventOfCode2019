open System.IO
open System.Collections.Generic




let intCode: int list = File.ReadAllText("./Day2_input.txt").Split ","
                        |> Array.map int
                        |> Array.toList


// This one shouts for mutability




let calculateOutput (initialState: int list) (noun: int) (verb: int): int = 
    let mutableIntCode: List<int> = new List<int>(initialState)
    mutableIntCode.[1] <- noun
    mutableIntCode.[2] <- verb

    let mutable index = 0
    while (mutableIntCode.[index * 4] < 3 )  do

        let baseIndex = index * 4
        match mutableIntCode.[baseIndex] with
            | 1 ->  mutableIntCode.[mutableIntCode.[baseIndex + 3]] <- mutableIntCode.[mutableIntCode.[baseIndex + 1]] + mutableIntCode.[mutableIntCode.[baseIndex + 2]]
            | 2 ->  mutableIntCode.[mutableIntCode.[baseIndex + 3]] <- mutableIntCode.[mutableIntCode.[baseIndex + 1]] * mutableIntCode.[mutableIntCode.[baseIndex + 2]] 
            | _ ->  failwith(string mutableIntCode.[baseIndex])
        index <- index + 1
    mutableIntCode.[0]



let findNounAndVerb targetValue initialState = 
    let mutable matchNotFound = true
    let combinations = seq {
        for i in 0..99 do
            for j in 0..99 do
               [calculateOutput initialState i j;i;j]
    }

    let winner = combinations
                    |> Seq.find (fun x-> x.[0] = targetValue)

    printf "result: %A%A" winner.[1] winner.[2]


findNounAndVerb 19690720 intCode