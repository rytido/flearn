module CollatzConjecture

let rec collatz n i =
    match n with
    | 1 -> Some i
    | n when n < 1 -> None
    | _ ->
        let next = if n % 2 = 0 then n / 2 else 3 * n + 1
        collatz next (i + 1)

let steps (number: int): int option = collatz number 0
