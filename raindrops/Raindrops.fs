module Raindrops

let convert (number: int): string =
    let divisors = [3; 5; 7]
    let words = ["Pling"; "Plang"; "Plong"]
    List.map2 (fun d w -> if number % d = 0 then w else "") divisors words
    |> String.concat ""
    |> (fun x -> if x = "" then sprintf "%i" number else x)