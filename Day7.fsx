open System.IO
open System.Collections.Generic

type Amplifier = (int * int) -> int


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

let getStorageLocation (mode: ParameterMode) (programState: int []) (locationValue: int) = 
        match mode with
        | PositionMode -> programState.[locationValue]
        | ImmediateMode -> locationValue


// Lets execute the program recursively with a moving pointer
let rec runProgram (pointer: int) (programState : int []) (inputs: int[]) (output: string): string =
    let instruction = getInstruction(programState.[pointer]) 
    let mutable newPointer = pointer
    let mutable newOutput = output;
    let mutable newInputs = inputs;
    printfn "Instruction opcode %i" instruction.Opcode
    printfn "Instruction mode: %A" instruction.Mode1
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
            let destination = getStorageLocation instruction.Mode1 programState (pointer + 1)
            programState.[destination] <- inputs.[0]
            printfn "%A" inputs
            match Array.length inputs with
                | 1 -> newInputs <- [||]
                | _ -> newInputs <- inputs.[1..(Array.length inputs - 1)]         
            newPointer <- pointer + 2
        | 4 ->  
            // Did I forget before?
            newOutput <- output + string (getValue instruction.Mode1  programState  (pointer + 1))
            newPointer <- pointer + 2        
        | 5 ->
            if getValue instruction.Mode1  programState  (pointer + 1)  <> 0 
                then newPointer <- getValue instruction.Mode2  programState  (pointer + 2) 
                else newPointer <- pointer + 3
        | 6 ->
            if getValue instruction.Mode1  programState  (pointer + 1) = 0 
                then newPointer <- getValue instruction.Mode2  programState  (pointer + 2) 
                else newPointer <- pointer +  3
        | 7 ->
            let destination = getStorageLocation instruction.Mode3 programState (pointer + 3)
            if getValue instruction.Mode1  programState  (pointer + 1) < getValue instruction.Mode2  programState  (pointer + 2)  
                then programState.[destination] <- 1 
                else programState.[destination] <- 0
            newPointer <- pointer + 4
        | 8 -> 
            let destination = getStorageLocation instruction.Mode3 programState (pointer + 3)
            if getValue instruction.Mode1  programState  (pointer + 1) = getValue instruction.Mode2  programState  (pointer + 2) 
                then programState.[destination] <- 1 
                else programState.[destination] <- 0   
            newPointer <- pointer + 4
        | 99 -> printfn "End Program"     
        | _ ->  failwith(string programState.[pointer])
    
    if (instruction.Opcode = 99) then newOutput else runProgram newPointer programState newInputs newOutput
   



// Diagnostics test program based on day 5


let runTest (testCode: int []) (input: int []) (expectedOutput: string) = 
    let resultOutput = runProgram 0 testCode input ""                      
    if (resultOutput <> expectedOutput) then failwith "Test failed" else printfn "Test succeeded"


let intCodeDay5Test1 = [|3;9;8;9;10;9;4;9;99;-1;8|]
runTest intCodeDay5Test1 [|8|] "1"            
runTest intCodeDay5Test1 [|7|] "0"    
runTest intCodeDay5Test1 [|9|] "0"      

let intCodeDay5Test5: int [] = File.ReadAllText("./Day5_input_test5.txt").Split ','
                                    |> Array.map int
runTest intCodeDay5Test5 [|4|] "999"
runTest intCodeDay5Test5 [|8|] "1000"
runTest intCodeDay5Test5 [|10|] "1001"
                    
let intCodeDay5: int [] = File.ReadAllText("./Day5_input.txt").Split ','
                        |> Array.map int

runTest intCodeDay5 [|5|] "12111395"                  

let amplifyInput (program: int []) (phase: int) (input: int): int =
    // initialize amplifier
    let instructionPointer:int = 0;
    let output = runProgram instructionPointer program [|phase;input|] ""
    int output


let intCode: int [] = File.ReadAllText("./Day7_input_sample1.txt").Split ','
                        |> Array.map int
let phases: int [] = [|4;3;2;1;0|]
let initialInput = 0

phases
    |> Array.fold (fun input phase -> amplifyInput intCode phase input ) initialInput
    |> printfn "hurray %A"