module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School =
    match school with
    | s when s.ContainsKey(grade) ->
        let students = student :: school.[grade] |> List.sort
        school.Add(grade, students)
    | _ -> school.Add(grade, [student])

let roster (school: School): string list =
    school
    |> Map.toList
    |> List.collect snd

let grade (number: int) (school: School): string list =
    if school.ContainsKey(number) then school.[number] else []
