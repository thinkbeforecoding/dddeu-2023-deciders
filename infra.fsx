#I @"C:\dev\eskv\bin\eskv.client\"
#r "eskv.client.dll"
#load "deciders.fsx" "procs.fsx" 

open Deciders
open eskv

// this is the client API for eskv
// https://github.com/thinkbeforecoding/eskv
let store = EskvClient()

/// Runs a decider in memory, 
/// storing state in a mutable variable
let inMemory decider =
    let mutable state = decider.initialState
    fun command ->
        let events = decider.decide command state
        state <- List.fold decider.evolve state events
        events


/// loads a value by key from eskv
/// return defaultValue if the key doesn't exist
let tryLoad deserialize defaultValue container key =
    let result = store.TryLoad(container,key)
    let value =
        if result.KeyExists then
            deserialize result.Value
        else
            defaultValue

    value, result.ETag
        

/// Save a value under a key in eskv
/// raises an exception if the etag doesn't match
let save serialize container key state etag =
    match store.TrySave(container, key, serialize state, etag ) with
    | null -> failwith "Could not save state"
    | _ -> ()


/// Load events for given stream from eskv
/// and get the current expected version
let loadEvents deserialize stream =
    let slice = store.ReadStreamForward(stream, 0)
    let events = 
        slice.Events 
        |> Seq.collect (fun r -> deserialize (r.EventType, r.Data)) 
        |> Seq.toList
    events, slice.ExpectedVersion

/// Append events to specified stream
/// raises an exception if the expectedVersion doesn't match
let appendEvents serialize key events expectedVersion =
    let result = store.TryAppend(key, expectedVersion, [ for (typ, data) in List.map serialize events -> { EventType = typ; Data = data }  ])
    if not result.Success then
        failwith "Could not append events"

module State =
    // run a decider, loading and saving state
    let run decider (serialize, deserialize) container key =

        fun command ->
            let state, etag = tryLoad deserialize decider.initialState container key
            let events = decider.decide command state 
            let newState = List.fold decider.evolve state events
            save serialize container key newState etag
            events


module EventSourcing =
    // run a decider, loading and saving events
    let run decider (serialize, deserialize) stream =

        fun command ->
            let events, expectedVersion = loadEvents deserialize stream
            let state = List.fold decider.evolve decider.initialState events
            let newEvents = decider.decide command state 
            appendEvents serialize stream newEvents expectedVersion
            newEvents






        

