open System.IO
open System.Collections.Generic


let intCode: int [] = File.ReadAllText("./Day5_input.txt").Split ','
                        |> Array.map int


// This one shouts for mutability
type ParameterMode = 
    | PositionMode
    | ImmediateMode

type Instruction = { 
    Opcode: int;
    Mode1: ParameterMode;
    Mode2: ParameterMode;
    Mode3: ParameterMode
    }

let getInstruction (input: int): Instruction = 
    let modeA =  if (int (floor (float input / float 10000)) = 1) then ImmediateMode else PositionMode
    let rest1 = input % 10000
    let modeB =  if (int (floor (float rest1 / float 1000)) = 1 ) then ImmediateMode else PositionMode
    let rest2 = rest1 % 1000
    let modeC =  if (int (floor (float rest2 / float 100)) = 1 ) then ImmediateMode else PositionMode
    // printfn "rest1 %i" rest1
    // printfn "1st mode %A" modeC
    // printfn "rest2 %i" rest2

    let instruction = rest1 % 100


    {
        Opcode=instruction;
        Mode1=modeC;
        Mode2=modeB;
        Mode3=modeA
    }

let getValue (mode: ParameterMode) (programState: int []) pointer =
    match mode with
        | PositionMode -> programState.[programState.[pointer]]
        | ImmediateMode -> programState.[pointer]

let getStorageLocation (mode: ParameterMode) (programState: int []) (locationValue) = 
        match mode with
        | PositionMode -> programState.[locationValue]
        | ImmediateMode -> locationValue


// Lets execute the program recursively with a moving pointer
let rec runProgram (pointer: int) (lastOpcode: int) (programState : int []) (input: int) (output: string) =
    let instruction = getInstruction(programState.[pointer])
    printfn "instruction %i" instruction.Opcode
    let mutable newPointer = pointer
    let mutable newOutput = ""
    match instruction.Opcode with
        | 1 ->  
            let destination = getStorageLocation instruction.Mode3 programState (pointer + 3)
            programState.[destination] <- (getValue instruction.Mode1  programState  (pointer + 1) + getValue instruction.Mode2  programState (pointer + 2))
            newPointer <- pointer + 4
        | 2 ->  
            let destination = getStorageLocation instruction.Mode3 programState (pointer + 3)
            programState.[programState.[pointer + 3]] <- (getValue instruction.Mode1  programState  (pointer + 1) * getValue instruction.Mode2  programState (pointer + 2))
            newPointer <- pointer + 4
        | 3 ->  
            let destination = getStorageLocation instruction.Mode3 programState (pointer + 1)
            programState.[destination] <-  input
            newPointer <- pointer + 2
        | 4 ->  
            newOutput <- output + string programState.[programState.[pointer + 1]]
            newPointer <- pointer + 2
        | 5 ->
            if getValue instruction.Mode1  programState  (pointer + 1)  <> 0 
                then newPointer <- getValue instruction.Mode2  programState  (pointer + 2) 
                else newPointer <- pointer + 3
        | 6 ->
            if getValue instruction.Mode1  programState  (pointer + 1) = 0 
                then newPointer <- getValue instruction.Mode2  programState  (pointer + 2) 
                else newPointer <- pointer + 3
        | 7 ->
            let destination = getStorageLocation instruction.Mode3 programState (pointer + 3)
            printfn "%A" instruction.Mode3
            if getValue instruction.Mode1  programState  (pointer + 1) < getValue instruction.Mode2  programState  (pointer + 2)  
                then programState.[destination] <- 1 
                else programState.[destination] <- 0
            newPointer <- pointer + 4
        | 8 -> 
            let destination = getStorageLocation instruction.Mode3 programState (pointer + 3)
            printfn "%A" instruction.Mode3
            if getValue instruction.Mode1  programState  (pointer + 1) = getValue instruction.Mode2  programState  (pointer + 2) 
                then programState.[destination] <- 1 
                else programState.[destination] <- 0   
            newPointer <- pointer + 4
        | 99 ->
            printfn "End of program"
            printfn "%A" output
        | _ ->  failwith(string programState.[pointer])
    
    if (instruction.Opcode = 99) then programState else runProgram newPointer programState.[pointer] programState input newOutput
   
let test1 = [|1002;4;3;4;33|]
let test2 = [|1101;100;-1;4;0|]
let test3 = [|1002;4;3;4;33;1101;100;-1;4;0|]
let test4 = [|3;9;8;9;10;9;4;9;99;-1;8|]
let test5 = [|3;9;7;9;10;9;4;9;99;-1;8|]
runProgram 0 0 intCode 5 ""