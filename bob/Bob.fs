module Bob
open System

let isAllUpper (s: string): bool =
    let noLower = s |> String.exists Char.IsLower |> not
    let hasUpper = s |> String.exists Char.IsUpper
    noLower && hasUpper

let response (input: string): string =
    match input.Trim() with
    | s when (isAllUpper s) && (s.EndsWith("?")) -> "Calm down, I know what I'm doing!"
    | s when s.EndsWith("?") -> "Sure."
    | s when (isAllUpper s) -> "Whoa, chill out!"
    | "" -> "Fine. Be that way!"
    | _ -> "Whatever."
