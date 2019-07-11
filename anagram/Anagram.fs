module Anagram

let counter (input: string) =
    input.ToLower()
    |> Seq.countBy id
    |> Seq.sortBy fst

let compare s1 s2 =
    if (Seq.length s1) <> (Seq.length s2) then
        false
    else
        Seq.exists2 ( <> ) s1 s2 |> not

let findAnagrams (sources: string list) (target: string): string list =
    let t = counter target
    sources
    |> List.filter (fun s -> if s.ToLower() = target.ToLower() then false else compare (counter s) t)
