#load "deciders.fsx"
open Deciders

// Structure for a process
type Process<'e,  'c, 's> =
    { evolve: 's -> 'e -> 's
      resume: 's -> 'c list
      react: 's -> 'e -> 'c list
      initialState: 's
      isTerminal: 's -> bool }

// this process wakes up the cat when the bulb is switched on
// (of course the can will no be got to sleep when the bulb is switched off...)
module CatLight =

    type Event =
    | SwitchedOn
    | WokeUp

    type Command =
    | WakeUp
    
    type State =
    | Idle
    | WakingUp


    let proc =
        { evolve =
            fun state event ->
                match event with
                | SwitchedOn -> WakingUp
                | WokeUp -> Idle

          resume = fun state -> 
            match state with
            | WakingUp -> [WakeUp]
            | _ -> []

          react = fun state event ->
            match state, event with
            | WakingUp, SwitchedOn -> [WakeUp]
            | _ -> []          

          initialState = Idle
          
          isTerminal = fun s -> s = Idle
        }

module Process =
    // change the input events and output commands types of the process
    let adapt selectEvent convertCommand (p: Process<_,_,_>) =
        { evolve =
            fun state event ->
                match selectEvent event with
                | Some e -> p.evolve state e
                | None -> state
          resume =
            fun state ->
                p.resume state |> List.map convertCommand

          react =
             fun state event ->
                match selectEvent event with
                | Some e -> 
                    p.react state e |> List.map convertCommand
                | None -> []
          initialState = p.initialState

          isTerminal = p.isTerminal
        }

    // fold state and collect commands returned by the react function
    let collectFold (proc: Process<'e,'c,'s>) (state: 's) (events: 'e list) : 'c list =
        let rec loop state events allCommands =
            match events with
            | [] -> allCommands
            | event :: rest ->
                let newState = proc.evolve state event 
                let cmds = (proc.react newState event) 
                loop newState rest (allCommands @ cmds)
        loop state events []

    // combine a process with a decider
    let combineWithDecider (proc: Process<'e,'c,'ps>) (decider: Decider<'c,'e,'ds>) : Decider<'c,'e,'ds * 'ps> =
        { decide =
            fun cmd (ds, ps) ->
                let rec loop cmds allEvents =
                    match cmds with
                    | [] -> allEvents
                    | cmd :: rest ->
                        let events = decider.decide cmd ds
                        let newCmds = collectFold proc ps events 
                        loop (rest @ newCmds) (allEvents @ events)

                loop [cmd] []
          evolve =
            fun (ds, ps) event ->
                (decider.evolve ds event), proc.evolve ps event  

          initialState = decider.initialState, proc.initialState

          isTerminal = fun (ds, ps) -> decider.isTerminal ds && proc.isTerminal ps
        }
