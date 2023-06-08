#load "types.fsx" "deciders.fsx" 
open Types

// super basic serialization for the bulb 
module Bulb =
    open Deciders.Bulb

    // state serialization
    module State =
        let serialize  = 
            function
            | NotFitted -> "NotFitted"
            | Working { Status = On; RemainingUses = r} -> $"On {r}"
            | Working { Status = Off; RemainingUses = r} -> $"Off {r}"
            | Blown -> "Blown"
            
        let deserialize (text: string) =
            match String.split ' ' text with
            | [|"NotFitted"|] ->
                NotFitted

            | [| "On"; Int usages |] -> 
                Working { Status = On
                          RemainingUses = usages}

            | [| "Off"; Int usages |] ->
                Working { Status = Off
                          RemainingUses = usages }
            | [| "Blown" |] -> Blown
            | _ -> failwith $"Unknown bulb state {text}"

        let serializer = serialize, deserialize

    // event serialization
    module Event =
        let serialize =
            function
            | Fitted e -> "Fitted", string e.MaxUses
            | SwitchedOn -> "SwitchedOn", ""
            | SwitchedOff -> "SwitchedOff", ""
            | Blew -> "Blew", ""

        let deserialize  =
            function
            | "Fitted", Int maxUsages -> [Fitted { MaxUses = maxUsages}]
            | "SwitchedOn", "" -> [SwitchedOn]
            | "SwitchedOff", "" -> [SwitchedOff]
            | "Blew", "" -> [Blew]
            | _ -> []


        let serializer = serialize, deserialize
    
// super basic serialization for the cat decider
module Cat =
    open Deciders.Cat
    
    // state serialization
    module State =
        let serialize  = 
            function
            | Awake -> "Awake"
            | Asleep -> "Asleep"

        let deserialize (text: string) =
            match text with
            | "Awake" -> Awake
            | "Asleep" -> Asleep
            | _ -> failwith $"Unknown cat state {text}"

        let serializer = serialize, deserialize

    // event serialization
    module Event =
        let serialize =
            function
            | WokeUp -> "WokeUp", ""
            | GotToSleep -> "GotToSleep", ""

        let deserialize  =
            function
            | "WokeUp", "" -> [WokeUp]
            | "GotToSleep", "" -> [GotToSleep]
            | _ -> []

        let serializer = serialize, deserialize
    

module Compose =
    module State =
        let serialize sx sy (x,y) =
            sx x + "|" + sy y 

        let deserialize dx dy (text: string) =
            let i = text.LastIndexOf('|')
            if i >= 0 then
                let tx = text.Substring(0,i)
                let ty = text.Substring(i+1) 
                dx tx, dy ty
            else
                failwith $"Invalide composed state '{text}'"
        
        let serializer (sx,dx) (sy, dy) = serialize sx sy, deserialize dx dy


    module Event =
        let serialize sx sy event =
            match event with
            | Left e ->
                let typ,data = sx e
                typ, "L|"+data 
            | Right e ->
                let typ, data = sy e
                typ, "R|"+data

        let deserialize dx dy (typ:string, data: string) =
            if data.StartsWith("L|") then
                dx (typ, data.Substring(2)) |> List.map Left
            else
                dy (typ, data.Substring(2)) |> List.map Right

        let serializer (sx,dx) (sy,dy) = serialize sx sy, deserialize dx dy

module Many =
    module State =
        let serialize serialize (state: Map<string, 'a>) =
            state
            |> Map.toSeq
            |> Seq.map (fun (k, v) -> $"{k}:{serialize v}")
            |> String.concat ";"

        let deserialize deserialize (text: string) =
            String.splitNonEmpty ';' text
            |> Seq.map (fun t -> 
                match String.split ':' t with
                | [|k;v|] -> k, deserialize v
                | _ -> failwith "Invalid format")
            |> Map.ofSeq
            

        let serializer (s,d) = serialize s, deserialize d 

    module Event =
        let serialize serialize (id,event) =
            let typ,data = serialize event
            typ, $"{id}:{data}"

        let deserialize deserialize (typ:string, data: string) =
            match String.split ':' data with
            | [|id; data|] ->  
                deserialize (typ, data)
                |> List.map (fun event -> id, event)
            | _ -> failwith "Invalid format"

            
        let serializer (s,d) = serialize s, deserialize d
 

