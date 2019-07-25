module ArmstrongNumbers

let numDigits = function
    | 0 -> 1.
    | n -> n |> float |> log10 |> ceil

let exp10 (x: int) = 10. ** (float x) |> int

let digiti (n: int) (i: int) =
    (n / ((i - 1) |> exp10)) % 10 |> float

let isArmstrongNumber (number: int) =
    let d = number |> numDigits
    let dgt = digiti number
    let asum = seq {1 .. (int d)} |> Seq.sumBy (fun i -> (i |> dgt) ** d)
    int asum = number
