open System.IO
open System.Collections.Generic

// utility for creating permutations
// Cheating I just copy pasted this stuff from stack overflow
let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

///
/// Let's define some types up front
type ParameterMode = 
    | PositionMode
    | ImmediateMode

type Instruction = { 
    Opcode: int;
    Mode1: ParameterMode;
    Mode2: ParameterMode;
    Mode3: ParameterMode
    }

type Amplifier = {
    phase: int
    programState: int []
    initialized: bool
    pointer: int
    completed: bool
    output: int
}

/// Get the instruction details for an opCode input
let getInstruction (input: int): Instruction = 
    let modeA =  if (int (floor (float input / float 10000)) = 1) then ImmediateMode else PositionMode
    let rest1 = input % 10000
    let modeB =  if (int (floor (float rest1 / float 1000)) = 1 ) then ImmediateMode else PositionMode
    let rest2 = rest1 % 1000
    let modeC =  if (int (floor (float rest2 / float 100)) = 1 ) then ImmediateMode else PositionMode
    let instruction = rest1 % 100
    {
        Opcode=instruction;
        Mode1=modeC;
        Mode2=modeB;
        Mode3=modeA
    }

/// get a value specified by an instruction. Take care of PositionMode vs ImmediateMode
let getValue (mode: ParameterMode) (programState: int []) pointer =
    match mode with
        | PositionMode -> programState.[programState.[pointer]]
        | ImmediateMode -> programState.[pointer]

// get the storage location
// This is actually always PositionMode :)
let getStorageLocation (mode: ParameterMode) (programState: int []) (locationValue: int) = 
        match mode with
        | PositionMode -> programState.[locationValue]
        | ImmediateMode -> locationValue


// recurse recurse recurse
// Given an input and an Amplifier, return the new state of the amplifier including it's output
// take special care of initialization with the phase, halting when there is output and final completion
let rec runProgram (amplifier: Amplifier) (input: int): Amplifier =
    let instruction = getInstruction(amplifier.programState.[amplifier.pointer]) 
    let { pointer=pointer; programState=programState } = amplifier
    printfn "Instruction code %i" instruction.Opcode
    let newAmplifierState =
        match instruction.Opcode with
            | 1 ->  
                let destination = getStorageLocation instruction.Mode3 programState (pointer + 3)
                programState.[destination] <- (getValue instruction.Mode1  programState  (pointer + 1) + getValue instruction.Mode2  programState (pointer + 2))
                { amplifier with pointer=pointer + 4; programState=programState}
            | 2 ->  
                let destination = getStorageLocation instruction.Mode3 programState (pointer + 3)
                programState.[programState.[pointer + 3]] <- (getValue instruction.Mode1  programState  (pointer + 1) * getValue instruction.Mode2  programState (pointer + 2))
                { amplifier with pointer=pointer + 4; programState=programState}
            | 3 ->  
                let destination = getStorageLocation instruction.Mode1 programState (pointer + 1)
                match amplifier.initialized with
                    | false -> 
                        programState.[destination] <- amplifier.phase
                        runProgram { amplifier with pointer=pointer + 2; programState=programState; initialized=true} input
                    | true -> 
                        programState.[destination] <- input
                        { amplifier with pointer=pointer + 2; programState=programState}
                
            | 4 ->  
                let output = getValue instruction.Mode1  programState  (pointer + 1)
                {amplifier with pointer=pointer + 2; output=output}        
            | 5 ->
                printfn "%A" instruction.Mode1
                printfn "%A" instruction.Mode2
                let newPointer =
                    match getValue instruction.Mode1  programState  (pointer + 1) with
                        | value when value <> 0 -> getValue instruction.Mode2  programState  (pointer + 2) 
                        | _ -> pointer + 3
                { amplifier with pointer=newPointer}    
            | 6 ->
                let newPointer =
                    match getValue instruction.Mode1  programState  (pointer + 1) with
                        | value when value = 0 -> getValue instruction.Mode2  programState  (pointer + 2) 
                        | _ -> pointer +  3
                { amplifier with pointer=newPointer}    

            | 7 ->
                let destination = getStorageLocation instruction.Mode3 programState (pointer + 3)
                if getValue instruction.Mode1  programState  (pointer + 1) < getValue instruction.Mode2  programState  (pointer + 2)  
                    then programState.[destination] <- 1 
                    else programState.[destination] <- 0
                { amplifier with pointer=pointer + 4; programState=programState}  
            | 8 -> 
                let destination = getStorageLocation instruction.Mode3 programState (pointer + 3)
                if getValue instruction.Mode1  programState  (pointer + 1) = getValue instruction.Mode2  programState  (pointer + 2) 
                    then programState.[destination] <- 1 
                    else programState.[destination] <- 0   
                { amplifier with pointer=pointer + 4; programState=programState}  
            | 99 -> 
                printfn "End Program"     
                { amplifier with completed=true }
            | _ ->  failwith("invalid instruction: " + string programState.[pointer])

    match instruction.Opcode with
        | 99 -> {newAmplifierState with completed=true}
        | 4  -> 
            printfn "Reached opcode 4"
            newAmplifierState
        | _  -> runProgram newAmplifierState input
   

// Diagnostics test program based on day 5

/// Single amplifier test
let runTest (amplifier: Amplifier) (input: int) (expectedOutput: int) = 
    let { output=output }= runProgram amplifier input                      
    if ( output <> expectedOutput) then failwith "Test failed" else printfn "Test succeeded"


let intCodeDay5Test1 = [|3;9;8;9;10;9;4;9;99;-1;8|]
let testAmplifier: Amplifier = { programState=intCodeDay5Test1; pointer=0; initialized=true; phase=0; completed=false; output=0}
runTest testAmplifier 8 1            
runTest testAmplifier 7 0    
runTest testAmplifier 9 0      

let intCodeDay5Test5: int [] = File.ReadAllText("./Day5_input_test5.txt").Split ','
                                    |> Array.map int
let testAmplifier5: Amplifier = { testAmplifier with programState=intCodeDay5Test5}
runTest testAmplifier5 4 999
runTest testAmplifier5 8 1000
runTest testAmplifier5 10 1001
                    
let intCodeDay5: int [] = File.ReadAllText("./Day5_input.txt").Split ','
                        |> Array.map int

let testAmplifier6 = { testAmplifier with programState=intCodeDay5}
runTest testAmplifier6 5 12111395                  


// Day 7 part one
let intCode: int [] = File.ReadAllText("./Day7_input_sample1.txt").Split ','
                        |> Array.map int
let phases: int list = [4;3;2;1;0]
  
let runThrusterTest1 (program: int[]) (phases: int list) (expectedOutput: int) = 
    let output: int = phases
                     |> List.map (fun phase -> {phase=phase;programState=program;initialized=false;pointer=0;completed=false;output=0})
                     |> List.fold (fun input amplifier -> 
                                        let {output=output} = runProgram amplifier input
                                        output 
                                  ) 0
                    
    match output with 
        | value when expectedOutput = value ->  printfn "Test succeeded with output: %i" output
        | _ -> printf "Test failed, got %i" output

runThrusterTest1 intCode phases 43210



let intCode2: int [] = File.ReadAllText("./Day7_input.txt").Split ','
                        |> Array.map int
let calculateThrusterInput (program: int[]) (phases: int list) =
    phases
     |> List.map (fun phase -> {phase=phase;programState=program;initialized=false;pointer=0;completed=false;output=0})
     |> List.fold (fun input amplifier -> 
                        let {output=output} = runProgram amplifier input
                        output 
                  ) 0            
let permutations = permute [0;1;2;3;4]
// Day 7: final calculation and verification
let day7output = permutations
                    |> List.map (fun phase -> calculateThrusterInput intCode2 phase)
                    |> List.max
match day7output with 
    | 212460 ->  printfn "Test succeeded with output: %i" day7output
    | _ -> printf "Test failed"



// Day7: part 2

// This loop has to iterate over all amplifiers
// - first time, the phase is the initial parameter
// - when we get the output we feed  it into the next one
// - the final one feeds back to the first one
// - the moment the software halts, we use the last output
let createInitialAmplifiers (phases: int list) (program: int []): Amplifier list =
    let baseAmplifier = { phase=phase;programState=[];initialized=false;pointer=0;completed=false;output=0}
    phases
        |> List.map (fun phase -> {baseAmplifier with programtState=program}
            

type LoopState = {amplifiers: Amplifier list; output:int; completed:bool}

let rec loopAmplifiers (_amplifiers: Amplifier list) (input: int): int =
    printfn "START A NEW LOOP"
    let previousState: LoopState = {amplifiers=[];output=input;completed=false}
    let lastLoop= _amplifiers
                        |> List.fold (fun previousState nextAmplifier -> 
                                printfn "RUN AMPLIFIER"
                                printfn "%A" nextAmplifier
                                printfn "%i" previousState.output
                                let amplifierState = runProgram nextAmplifier previousState.output
                                printfn "OUTPUT %A" amplifierState                          
                                {amplifiers=List.append previousState.amplifiers [amplifierState];output=amplifierState.output;completed=amplifierState.completed}                          
                            ) previousState
    
    match lastLoop with
        | {completed=false} ->  
            printfn "END OF NORMAL LOOP"
            printfn "%i" lastLoop.output
            loopAmplifiers lastLoop.amplifiers lastLoop.output
        | {completed=true} -> 
            printfn "DONE DONE DONE"
            printfn "%i" lastLoop.output
            lastLoop.output

// Test 1
let testPhase = [9;8;7;6;5]
let intCodeTest2: int [] = File.ReadAllText("./Day7_input_sample2.txt").Split ','
                        |> Array.map int    


let test = createInitialAmplifiers testPhase intCodeTest2
let testResult2 = loopAmplifiers (createInitialAmplifiers testPhase intCodeTest2) 0

let permutationsPart2 = permute [5..9]

permutationsPart2
    |> List.map (fun phase -> loopAmplifiers (createInitialAmplifiers  phase intCode2) 0)
    |> List.max