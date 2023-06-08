#load "types.fsx" "deciders.fsx" "procs.fsx" "infra.fsx" "serialization.fsx"

open Types
open Procs
open Deciders
open Infra
open Serialization

let title = """Aggregates composition:
                  A new view on Aggregates"""

let speaker = "Jérémie Chassaing 🤟"
let company = "D-EDGE"

let blog = "https://thinkbeforecoding.com"
let mastodon = "https://mastodon.social/@thinkb4coding"
let github = "https://github.com/thinkbeforecoding"






// Some test for the deciders
module Tests =

    // implement the Given/When operartor for any decider
    let givenWhen decider = 
        fun events command ->
            events
            |> List.fold decider.evolve decider.initialState
            |> decider.decide command

    // this is an operator for Assert equal (equal bang!)
    let (=!) actual expected =
        if actual = expected then
            printfn "✅"
        else
            printfn "❌ actual:"
            printfn "%A" actual
            printfn "expected:"
            printfn "%A" expected

    module Bulb =
        // defines => for the bulb decider
        let (=>) = givenWhen Bulb.decider

        []
        => Bulb.Fit { MaxUses = 5}
        =! [Bulb.Fitted { MaxUses = 5}]

        [ Bulb.Fitted { MaxUses = 5}]
        => Bulb.SwitchOn 
        =! [Bulb.SwitchedOn]

        [ Bulb.Fitted { MaxUses = 5}
          Bulb.SwitchedOn]
        => Bulb.SwitchOn 
        =! []

        [ Bulb.Fitted { MaxUses = 5}
          Bulb.SwitchedOn]
        => Bulb.SwitchOff 
        =! [Bulb.SwitchedOff]

        [ Bulb.Fitted { MaxUses = 5}
          Bulb.SwitchedOn
          Bulb.SwitchedOff ]
        => Bulb.SwitchOff 
        =! []

        [ Bulb.Fitted { MaxUses = 1}
          Bulb.SwitchedOn
          Bulb.SwitchedOff ]
        => Bulb.SwitchOn
        =! [Bulb.Blew]

    module Cat =
        // defines => for the bulb decider
        let (=>) = givenWhen Cat.decider

        []
        => Cat.GetToSleep
        =! [ Cat.GotToSleep ]

        [ Cat.GotToSleep]
        => Cat.WakeUp
        =! [ Cat.WokeUp ]

        [ Cat.GotToSleep ]
        => Cat.GetToSleep
        =! []

        [ Cat.GotToSleep
          Cat.WokeUp ]
        => Cat.WakeUp
        =! []



// run the deciders without persistance
module InMemory =
    let b = inMemory Bulb.decider
    b (Bulb.Fit { Bulb.MaxUses = 5} )

    b Bulb.SwitchOn
    b Bulb.SwitchOff

    let c = inMemory Cat.decider
    c Cat.WakeUp
    c Cat.GetToSleep

// run the deciders persisting state
module State =
    let b = State.run Bulb.decider Bulb.State.serializer "bulb" "1"

    b (Bulb.Fit { Bulb.MaxUses = 5} )

    b Bulb.SwitchOn
    b Bulb.SwitchOff

    let c= State.run Cat.decider Cat.State.serializer "cat" "1"

    c Cat.WakeUp
    c Cat.GetToSleep

// run the deciders persisting events
module EventSourcing =
    let b = EventSourcing.run Bulb.decider Bulb.Event.serializer "bulb 1"
    b (Bulb.Fit { Bulb.MaxUses = 5} )

    b Bulb.SwitchOn
    b Bulb.SwitchOff

    let c = EventSourcing.run Cat.decider Cat.Event.serializer "cat"

    c Cat.WakeUp
    c Cat.GetToSleep

// compose cat and bulb and run it in memory
module CatAndBulb =
    let catAndBulb = Decider.compose Cat.decider Bulb.decider

    let cnb = inMemory catAndBulb 
    cnb (Left Cat.WakeUp)
    cnb (Left Cat.GetToSleep)

    cnb (Right (Bulb.Fit { Bulb.MaxUses = 5}))
    cnb (Right Bulb.SwitchOn)
    cnb (Right Bulb.SwitchOff)

// compose cat and bulb and run it persisting state
module StateCatAndBulb =
    let decider = Decider.compose Cat.decider Bulb.decider
    let catAndBulb id =
        State.run
            decider
            (Compose.State.serializer
                Cat.State.serializer
                Bulb.State.serializer)
            id

    let cnb = catAndBulb "cat n bulb" "2"

    cnb (Right (Bulb.Fit { Bulb.MaxUses = 5 }))
    cnb (Left Cat.GetToSleep)
    cnb (Left Cat.WakeUp)
    cnb (Right Bulb.SwitchOn)
    cnb (Right Bulb.SwitchOff)


// compose cat and bulb and run it persisting events
module EsCatAndBulb =
    let catAndBulb = Decider.compose Cat.decider Bulb.decider
    let esCatAndBulb id =
        let serializer =
            Compose.Event.serializer
                Cat.Event.serializer
                Bulb.Event.serializer
        EventSourcing.run catAndBulb serializer id

    let cnb = esCatAndBulb "catbulb 3"

    cnb (Right (Bulb.Fit { Bulb.MaxUses = 5 }))
    cnb (Left Cat.GetToSleep)
    cnb (Left Cat.WakeUp)
    cnb (Right Bulb.SwitchOn)
    cnb (Right Bulb.SwitchOff)

module StateCatAnd2Bulbs =
    let decider = 
        Decider.compose (
            Decider.compose Cat.decider Bulb.decider
        ) Bulb.decider
    let run id = 
        let serializer = 
            Compose.State.serializer 
                (Compose.State.serializer
                    Cat.State.serializer
                    Bulb.State.serializer) 
                Bulb.State.serializer
        State.run decider serializer id
    let cn2b = run "cat n 2 bulbs" "1"

    cn2b (Left (Right (Bulb.Fit { Bulb.MaxUses = 5 })))
    cn2b (Left (Left Cat.GetToSleep))
    cn2b (Left (Left Cat.WakeUp))
    cn2b (Left (Right Bulb.SwitchOff))
    cn2b (Left (Right Bulb.SwitchOn))

    cn2b (Right (Bulb.Fit { Bulb.MaxUses = 5 }))
    cn2b (Right Bulb.SwitchOn)
    cn2b (Right Bulb.SwitchOff)

// compose a cat and two bulbs, and run it persisting events
module EsCatAnd2Bulbs =
    let decider = 
        Decider.compose (
            Decider.compose Cat.decider Bulb.decider
        ) Bulb.decider

    let cn2b = 
        EventSourcing.run
            decider (
                Compose.Event.serializer
                    (Compose.Event.serializer
                        Cat.Event.serializer
                        Bulb.Event.serializer)
            Bulb.Event.serializer)
            "cat and 2 bulb"

    cn2b (Left (Left Cat.WakeUp))
    cn2b (Left (Left Cat.GetToSleep))

    cn2b (Left (Right (Bulb.Fit { Bulb.MaxUses = 5})))
    cn2b (Left (Right Bulb.SwitchOn))
    cn2b (Left (Right Bulb.SwitchOff))

    cn2b (Right (Bulb.Fit { Bulb.MaxUses = 5}))
    cn2b (Right Bulb.SwitchOn)
    cn2b (Right Bulb.SwitchOff)

// make a single decider for many cats and run it in memory
module InMemoryManyCats =
    let cats = inMemory (Decider.many Cat.decider)

    cats ("boulette", Cat.WakeUp)
    cats ("boulette", Cat.GetToSleep)

    cats ("guevara", Cat.WakeUp)
    cats ("guevara", Cat.GetToSleep)


// make a single decider for many cats and run it persting state
module StateManyCats =
    let cats = 
        State.run
            (Decider.many Cat.decider)
            (Many.State.serializer Cat.State.serializer)
            "manycats" "1"
                
    cats ("boulette", Cat.WakeUp)
    cats ("boulette", Cat.GetToSleep)

    cats ("guevara", Cat.WakeUp)
    cats ("guevara", Cat.GetToSleep)

// run many cats as a single decider persisting events
module Cats =
    let cs = 
        EventSourcing.run
            (Decider.many Cat.decider)
            (Many.Event.serializer Cat.Event.serializer)
            "cats"

    cs ("Boulette", Cat.WakeUp)
    cs ("Boulette", Cat.GetToSleep)
    cs ("Guevara", Cat.WakeUp)
    cs ("Guevara", Cat.GetToSleep)



// compose a cat and a bulb and a process
module ComposeProcess =
    let catAndBulb = Decider.compose Cat.decider Bulb.decider

    let adaptedProcess =
        Process.adapt (function 
            | Left Cat.WokeUp -> Some CatLight.WokeUp
            | Right Bulb.SwitchedOn -> Some CatLight.SwitchedOn
            | _ -> None)
            (function | CatLight.WakeUp -> Left Cat.WakeUp )
            CatLight.proc


    let catBulb = Process.combineWithDecider adaptedProcess catAndBulb

    let catB = inMemory catBulb

    catB (Right (Bulb.Fit { Bulb.MaxUses = 5}))
    catB (Left Cat.GetToSleep)
    catB (Left Cat.WakeUp)
    catB (Right Bulb.SwitchOn)
    catB (Right Bulb.SwitchOff)

// compose a cat and a bulb and a process using specific structures
// it uses an applicative to combine deciders together
module ApplicativeCompose =

    type ComboState = { Cat: Cat.State; Bulb: Bulb.State }
    type Combo<'c,'b> = Cat of 'c | Bulb of 'b

    let combo =
        (fun c b -> { Cat = c; Bulb = b})
        <!> Decider.adapt 
                (function Cat cmd -> Some cmd | _ -> None)
                (function Cat e -> Some e | _ -> None)
                Cat
                (fun s -> s.Cat)
                Cat.decider
        <*> Decider.adapt
            (function Bulb cmd -> Some cmd | _ -> None)
            (function Bulb e -> Some e | _ -> None)
            Bulb
            (fun s -> s.Bulb)
            Bulb.decider

    let combo' = inMemory combo

    combo' (Bulb (Bulb.Fit { Bulb.MaxUses = 5} ))
    combo' (Bulb Bulb.SwitchOn)
    combo' (Bulb Bulb.SwitchOff)
    combo' (Cat Cat.WakeUp)
    combo' (Cat Cat.GetToSleep)

    let comboProc =
        Process.combineWithDecider
            (Process.adapt 
                (function
                    | Bulb Bulb.SwitchedOn -> Some CatLight.SwitchedOn 
                    | Cat Cat.WokeUp -> Some CatLight.WokeUp 
                    | _ -> None)
                (function CatLight.WakeUp -> Cat Cat.WakeUp)
                CatLight.proc)
            combo

    let combo'' = inMemory comboProc

    combo'' (Bulb (Bulb.Fit { Bulb.MaxUses = 5} ))
    combo'' (Bulb Bulb.SwitchOn)
    combo'' (Bulb Bulb.SwitchOff)
    combo'' (Cat Cat.WakeUp)
    combo'' (Cat Cat.GetToSleep)
        
    open Tests
    let (=>) = givenWhen comboProc

    [Bulb (Bulb.Fitted { Bulb.Fitted.MaxUses = 5})
     Cat Cat.GotToSleep ]
    => Bulb Bulb.SwitchOn
    =! [ Bulb Bulb.SwitchedOn
         Cat Cat.WokeUp ]
