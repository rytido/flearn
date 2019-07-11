module SumOfMultiples

let sum (numbers: int list) (upperBound: int): int =
    numbers
    |> List.map (fun n -> if n = 0 then [] else [n .. n .. (upperBound - 1)])
    |> Seq.concat
    |> Seq.distinct
    |> Seq.sum
