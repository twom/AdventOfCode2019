open System.IO

let orbits: string [] = File.ReadAllLines("./Day6_input.txt")

// count all direct and indirect orbits
// Create list of all planets
// Iterate through input and create a first map:
  // planet: orbits
  // go through each planet of the map and create the branch
  // count length of all branches
// Create chain for all planets

// Create a map:
// Key: Planet
// Value: Mass around which it is orbitting
let planetMap: Map<string,string> = orbits  
                                      |> Array.map (( fun el -> el.Split ')' ) >> ( fun el -> (el.[1], el.[0])))
                                      |> Map.ofArray
                                      

let createBranchForPlanet (planetMap: Map<string,string>) (planet: string): string [] = 
    

    let rec buildBranch (planet: string) (orbits: string []): string [] =
        if Map.containsKey planet planetMap
            then Array.concat [(buildBranch planetMap.[planet] [|planetMap.[planet]|]); orbits] 
            else orbits

    let result: string [] = buildBranch planet [||]
    printfn "%A" result
    result


let orbitBranches = planetMap
                        |> Map.map (fun planet mass -> createBranchForPlanet planetMap planet )

let countPart1 = orbitBranches
                    |> Map.fold ( fun state planet orbits -> state + Array.length orbits) 0 

let san = orbitBranches.["SAN"]
let you = orbitBranches.["YOU"]

let sanLength = Array.length san
printfn "SAN length %i" sanLength
let youLength = Array.length you
printfn "YOU length %i" youLength


let shortestLength = min sanLength youLength

let sanNormalized = san.[0..shortestLength-1]
let youNormalized = you.[0..shortestLength-1]

let commonIndex = Array.zip sanNormalized youNormalized
                    |> Array.findIndex (fun (a,b) -> a <> b)

printfn "commonIndex %i" commonIndex


let pathLength = (sanLength - commonIndex) + (youLength - commonIndex)

printfn "Result 1: %i" countPart1
printfn "Result 2: %i" pathLength