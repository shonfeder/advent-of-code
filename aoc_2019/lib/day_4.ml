open! Core

let (%) f g x = f (g x)

let int_to_digits i =
  Int.to_string i
  |> String.to_list
  |> List.map ~f:(Int.of_string % Char.to_string)

let low = 108457
let high = 562041

module Puzzle_1 = struct
  let satisfies_invarients pw =
    let rec check_digits two_adjects_are_same digits =
      match digits with
      | (x :: x' :: xs) ->
        let two_adjects_are_same = two_adjects_are_same || x = x' in
        x <= x' && check_digits two_adjects_are_same (x'::xs)
      | _ -> two_adjects_are_same
    in
    check_digits false pw

  let meets_criteria n =
    int_to_digits n |> satisfies_invarients

  let%test "ex 1 satisfies invarients"       = meets_criteria 111111
  let%test "ex 2 doesn't satisfy invarients" = not (meets_criteria 223450)
  let%test "ex 3 doesn't satisfy invarients" = not (meets_criteria 123789)

  let solution () =
    let candidates = List.range ~start:`inclusive ~stop:`inclusive low high in
    let count_candidate tally candidate =
      if meets_criteria candidate then tally + 1 else tally
    in
    List.fold ~f:count_candidate ~init:0 candidates
    |> Int.to_string
end

module Puzzle_2 = struct
  let count_runs ns =
    List.group ~break:(<>) ns |> List.map ~f:List.length

  let%test "count runs" =
    List.equal Int.equal (count_runs [1;1;1;0;2;2]) [3;1;2]

  let satisfies_invarients pw =
    let rec check_digits digits =
      match digits with
      | (x :: x' :: xs) -> x <= x' && check_digits (x'::xs)
      | _ -> List.mem (count_runs pw) 2 ~equal:Int.equal
    in
    check_digits pw

  let meets_criteria n =
    let digits = int_to_digits n in
    satisfies_invarients digits

  let%test "puzzle 1, ex 1 doesn't satisfies invarients" = not (meets_criteria 111111)
  let%test "ex 1 meets criteria" = meets_criteria 112233
  let%test "ex 2 doesn't satisfy invarients" = not (meets_criteria 123444)
  let%test "ex 3 does satisfy invarients" = meets_criteria 111122

  let solution () =
    let candidates = List.range ~start:`inclusive ~stop:`inclusive low high in
    let count_candidate tally candidate =
      if meets_criteria candidate then tally + 1 else tally
    in
    List.fold ~f:count_candidate ~init:0 candidates
    |> Int.to_string
end
