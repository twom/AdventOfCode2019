open System.IO

let intCode = File.ReadAllLines("./Day1_input.txt")
                |> Array.map float

let requiredFuel (mass: float) =  floor (mass/3.0) - 2.0

// PART 1
let result = intCode
                |> Array.sumBy requiredFuel

// PART 2
let rec requiredFuel2 (mass: float) = 
    let fuel = floor (mass/3.0) - 2.0 
    if (fuel > 0.0) then  fuel + (requiredFuel2 fuel) else 
        if fuel < 0.0 then 0.0 else fuel

let result2 = intCode
                |> Array.sumBy requiredFuel2

printf "%f" result2