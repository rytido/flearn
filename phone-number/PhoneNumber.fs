module PhoneNumber
open System

let intok (s:string) = s |> uint64 |> Ok

let ten (s:string) =
    match s with
    | s when s.[0] = '0' -> Error "area code cannot start with zero"
    | s when s.[0] = '1' -> Error "area code cannot start with one"
    | s when s.[3] = '0' -> Error "exchange code cannot start with zero"
    | s when s.[3] = '1' -> Error "exchange code cannot start with one"
    | s -> intok s

let eleven (s:string) =
    match s with
    | s when s.[0] <> '1' -> Error "11 digits must start with 1"
    | s -> s.[1..10] |> ten

let hasalpha (s:string) = s |> String.exists (fun c -> (Char.IsLower c) || (Char.IsUpper c))

let haspunc (s:string) = s |> String.exists (fun c -> ",@:!".Contains(c))

let clean (input:string): Result<uint64,string> =
    let stripped = input.Replace("(", "").Replace(")", "").Replace("-", "").Replace(".", "").Replace(" ", "").Trim('+')
    match stripped with
    | s when s.Length <= 9 -> Error "incorrect number of digits"
    | s when s.Length > 11 -> Error "more than 11 digits"
    | s when hasalpha s -> Error "alphanumerics not permitted"
    | s when haspunc s -> Error "punctuations not permitted"
    | s when s.Length = 11 -> eleven s
    | s -> ten s
