#load "types.fsx"
open Types

// the decider data structure
type Decider<'c, 'e, 'si, 'so> =
    { decide: 'c -> 'si -> 'e list
      evolve: 'si -> 'e -> 'so
      initialState: 'so
      isTerminal: 'si -> bool
    }

type Decider<'c,'e,'s> = Decider<'c,'e,'s,'s>


// The bulb decider (this is the domain code)
module Bulb =
    type Command =
    | Fit of Fit
    | SwitchOn
    | SwitchOff
    and Fit =
        { MaxUses: int }

    type Event =
    | Fitted of Fitted
    | SwitchedOn
    | SwitchedOff
    | Blew
    and Fitted =
        { MaxUses: int }

    type Status = On | Off

    type State =
        | NotFitted
        | Working of Working
        | Blown
    and Working =
        { Status: Status
          RemainingUses: int }


    let decide command state =
        match command, state with
        | Fit cmd, NotFitted                 -> [ Fitted { MaxUses = cmd.MaxUses} ]
        | Fit _ , _                          -> failwith "Bulb has already been fitted" 
        | SwitchOn, Working ({ Status = Off } as s)
                when s.RemainingUses > 0     -> [ SwitchedOn ]
        | SwitchOn, Working { Status = Off } -> [ Blew ]
        | SwitchOff, Working { Status= On }  -> [ SwitchedOff ]
        | _                                  -> []

    let evolve state event =
        match state, event with
        | NotFitted, Fitted e    -> Working { Status = Off; RemainingUses = e.MaxUses}
        | Working s, SwitchedOn  -> Working { Status = On; RemainingUses = s.RemainingUses - 1}
        | Working s, SwitchedOff -> Working { s with Status = Off}
        | Working _, Blew        -> Blown
        | _                      -> state
    
    let initialState = NotFitted

    let isTerminal state = state = Blown

    let decider = {
        decide = decide
        evolve = evolve
        initialState = initialState
        isTerminal = isTerminal }

// The cat decider
module Cat =
    type Command =
    | WakeUp
    | GetToSleep

    type Event =
    | WokeUp
    | GotToSleep

    type State =
    | Awake
    | Asleep

    let decide command state =
        match command, state with
        | WakeUp, Asleep    -> [ WokeUp]
        | GetToSleep, Awake -> [ GotToSleep ]
        | _                 -> []

    let evolve state event =
        match state, event with
        | Awake, GotToSleep -> Asleep
        | Asleep, WokeUp    -> Awake 
        | _                 -> state

    let initialState = Awake

    let isTerminal state = false 

    let decider = {
        decide = decide
        evolve = evolve
        initialState = initialState
        isTerminal = isTerminal }










module Decider =

    // compose two deciders as a single decider
    let compose (dx: Decider<'cx,'ex,'sx>) (dy: Decider<'cy,'ey,'sy>)
        : Decider<Either<'cx,'cy>, Either<'ex,'ey>, 'sx * 'sy>  =
        { decide = 
            fun cmd (sx,sy) ->
                match cmd with
                | Left cx -> dx.decide cx sx |> List.map Left
                | Right cy -> dy.decide cy sy |> List.map Right

          evolve =
            fun (sx,sy) event ->
                match event with
                | Left ex -> dx.evolve sx ex, sy
                | Right ey -> sx, dy.evolve sy ey

          initialState = dx.initialState, dy.initialState
          isTerminal = fun (sx,sy) -> dx.isTerminal sx && dy.isTerminal sy

        }
  
    // The neutral decider (for demonstration only)
    // It accepts no command, emits no events, and has a single state
    // when combined with another decider, the result decider is
    // equivalent to the original one (up to isomorphism)
    let neutral : Decider<Void, Void, unit> =
        { decide = fun _event _state -> []
          evolve = fun state _event -> state
          initialState = ()
          isTerminal = fun _state -> true }

    // takes a decider, and create a decider that can manage many instances
    // indexed by a string
    let many (decider: Decider<'c,'e,'s>) : Decider<string * 'c, string * 'e, Map<string,'s>> =
        { decide =
            fun (id, cmd) states ->
                let state =
                    match Map.tryFind id states with
                    | Some s -> s
                    | None -> decider.initialState
                let events = decider.decide cmd state
                events |> List.map (fun e -> id, e)

          evolve =
            fun states (id, event) ->
                let state =
                    match Map.tryFind id states with
                    | Some s -> s
                    | None -> decider.initialState
                let newState =decider.evolve state event
                Map.add id newState states

          initialState = Map.empty

          isTerminal =
            fun states ->
                Map.forall (fun _ s -> decider.isTerminal s) states
        }

    // adapt a decider to different commands, events and state
    // fci: a function to convert commands from the new command type to the current one 
    //        (None if the command is not for this decider)
    // fei: a function to convert events from the new event type to the current one
    //        (None if the event is not for this decider)
    // feo: a function to convert events from the current event type to the new one
    // fsi: a function to extract the state from the new state structure
    // the returned state type is still the one of this decider
    let adapt fci fei feo fsi decider =
        { decide =
            fun cmd state ->
                match fci cmd with
                | Some c -> decider.decide c (fsi state) |> List.map feo
                | None -> []
          evolve =
            fun state event ->
                match fei event with
                | Some e -> decider.evolve (fsi state) e
                | None -> fsi state
          initialState = decider.initialState
          isTerminal = fun state -> decider.isTerminal (fsi state)
                }

    // use the f function to convert the decider return state type to a different type 
    let map (f: 'sa -> 'sb) (d: Decider<_,_,'si,'sa>) : Decider<_,_,'si,'sb> =
        { decide = d.decide
          evolve =
            fun state event ->
                d.evolve state event |> f
          initialState = f d.initialState
          isTerminal = d.isTerminal
        }

    // use the f function to combine the two deciders return states in a single type
    let map2 (f: 'sx  -> 'sy -> 's)  (dx: Decider<'c,'e,'si, 'sx>) (dy: Decider<'c,'e,'si, 'sy>) :  Decider<'c,'e,'si, 's> =
        { decide =
            fun cmd state ->
                dx.decide cmd state @ dy.decide cmd state
          evolve =
            fun state event ->
                let sx = dx.evolve state event
                let sy = dy.evolve state event
                f sx sy
          initialState = f dx.initialState dy.initialState
          isTerminal = fun state -> dx.isTerminal state && dy.isTerminal state
        }

    // create an applicative on deciders return types
    let apply f x = map2 (fun f x -> f x) f x

// Opertators for the applicative
let (<!>) f x = Decider.map f x
let (<*>) f x = Decider.apply f x
