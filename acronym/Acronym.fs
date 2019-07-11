module Acronym

let abbreviate (phrase:string) =
    " " + phrase.Replace("-", " ")
    |> Seq.windowed 2
    |> Seq.filter (fun x -> (x.[0] = ' ') && (x.[1] <> ' '))
    |> Seq.map (fun x -> (string x.[1]).ToUpper())
    |> String.concat ""
