open System

// The either type used for standard composition
type Either<'l,'r> =
| Left of 'l 
| Right of 'r

// the Void type has no value at all
type Void = Void of Void

// string splitting utility functions
module String =
    let split (separator: char) (s: string) =
        s.Split(separator)
    
    let splitNonEmpty (separator: char) (s: string) =
        s.Split(separator, StringSplitOptions.RemoveEmptyEntries )

// string -> integer matching pattern
let (|Int|_|) (s: string) =
    match Int32.TryParse(s, Globalization.CultureInfo.InvariantCulture) with
    | true, v -> Some v
    | false, _ -> None