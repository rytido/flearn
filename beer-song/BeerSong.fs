module BeerSong

let firstVerse (bottles: int) =
    match bottles with
    | 0 -> "No more bottles of beer on the wall, no more bottles of beer."
    | 1 -> "1 bottle of beer on the wall, 1 bottle of beer."
    | _ -> sprintf "%d bottles of beer on the wall, %d bottles of beer." bottles bottles

let secondVerse (bottles: int) =
    let pronoun = if bottles = 1 then "it" else "one"
    let plural = if bottles = 2 then "" else "s"
    let count =
        match bottles with
        | 1 -> "no more"
        | _ -> string (bottles - 1)
    match bottles with
    | 0 -> "Go to the store and buy some more, 99 bottles of beer on the wall."
    | _ -> sprintf "Take %s down and pass it around, %s bottle%s of beer on the wall." pronoun count plural

let recite (startBottles: int) (takeDown: int): string list =
    [startBottles..(-1)..0]
    |> List.take takeDown
    |> List.collect (fun x -> [""; firstVerse x; secondVerse x])
    |> List.skip 1
