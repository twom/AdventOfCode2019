
/// naive pairwise check to see if we have pairs
let hasTwoTheSameAdjacent (candidate: int): bool = 
    candidate
    |> string
    |> Seq.toList
    |> Seq.pairwise
    |> Seq.where (fun (a,b) -> a = b)
    |> Seq.isEmpty
    |> not

/// basic check to for condition that numbers are not decreasing
let hasNoDecreasingNumbers (candidate:int): bool = 
    candidate
    |> string
    |> Seq.toList
    |> Seq.pairwise
    |> Seq.where (fun (a,b) -> a > b)
    |> Seq.isEmpty

/// better way to check that we only have groups of 2
let hasTwoTheSameAdjacentNotPartOfLargerGroup (candidate:int): bool =
        candidate
        |> string
        |> Seq.toList
        |> Seq.countBy id
        |> Seq.where (fun (number,count) -> count = 2 )
        |> Seq.isEmpty
        |> not

// combined validation
let validatePassword (candidate: int): bool  = 
    hasNoDecreasingNumbers candidate && hasTwoTheSameAdjacentNotPartOfLargerGroup candidate

let numberOfValidPasswords min max = 
    [| for i in min .. max -> validatePassword i |]
                    |> Array.toSeq
                    |> Seq.filter id
                    |> Seq.length

printf "Result 1: %i" (numberOfValidPasswords 372037 905157)
    




