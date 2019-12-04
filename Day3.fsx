open System.IO
open System.Collections.Generic




let wires: string [][] = File.ReadAllLines("./Day3_input.txt")
                            |> Array.map (fun x -> x.Split ",")


let getNewCoordinates ((x, y): int * int ) (instruction: string): list<int*int> = 
     let moves = int instruction.[1..]
     match instruction.Chars(0) with
        | 'U' -> seq { for i = 1 to moves do  (x, y+i) }
        | 'D' -> seq { for i = 1 to moves do  (x, y-i) }
        | 'R' -> seq { for i = 1 to moves do  (x+i, y) }
        | 'L' -> seq { for i = 1 to moves do  (x-i, y) }
        | _ -> Seq.empty
    |> Seq.toList

// create coordinate map
let createCoordinateMap (wire: string []): List<int * int> =
    let coordinates = new List<int*int>()
    let mutable currentPosition: int * int = (0,0)
    for i = 0 to Array.length wire - 1 do
        coordinates.AddRange(getNewCoordinates currentPosition wire.[i])
        currentPosition <- Seq.last coordinates
    coordinates


let coordinateMapA = createCoordinateMap wires.[0]
let coordinateMapB = createCoordinateMap wires.[1]

let intersect = Set.intersect (Set.ofArray (coordinateMapA.ToArray())) (Set.ofArray (coordinateMapB.ToArray()))

let result1 = intersect
                |> Set.map (fun (a,b) -> abs a + b)
                |> Set.toList
                |> List.min

let result2 = intersect
                |> Set.map (fun coord -> coordinateMapA.IndexOf(coord) + coordinateMapB.IndexOf(coord))
                |> Set.toList
                |> List.min


printf "THE RESULT 1: %i" result1
printf "THE RESULT 2: %i" result2