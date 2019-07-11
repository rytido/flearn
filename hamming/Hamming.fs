module Hamming


let hamme (s1: string) (s2: string): int =
    Seq.fold2 (fun x elem1 elem2 -> x + if elem1 = elem2 then 0 else 1) 0 s1 s2


let distance (strand1: string) (strand2: string): int option =
    if (String.length strand1) = (String.length strand2) then
        Some (hamme strand1 strand2)
    else
        None
