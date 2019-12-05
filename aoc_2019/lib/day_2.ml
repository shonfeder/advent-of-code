open! Core

open Intcode
module Comp = Comp ()

let%test "ex 1" =
  (Program.of_string "1,0,0,0,99" |> Comp.run) = 2
let%test "ex 2" =
  (Program.of_string "2,3,0,3,99" |> Comp.run ~res_loc:3)  = 6
let%test "ex 3" =
  (Program.of_string "2,4,4,5,99,0" |> Comp.run ~res_loc:5) = 9801
let%test "ex 4" =
  (Program.of_string "1,1,1,4,99,5,6,0,99" |> Comp.run) = 30

let program () =
  Input.read "day_2.txt"
  |> Program.of_string

module Puzzle_1 = struct
  let restore_program () =
    program ()
    |> Program.with_noun 12
    |> Program.with_verb 2

  let solution () =
    restore_program () |> Comp.run |> Int.to_string
end

module Puzzle_2 = struct
  let set_inputs program noun verb =
    program
    |> Program.with_noun noun
    |> Program.with_verb verb

  let run_program program (noun, verb) =
    set_inputs program noun verb |> Comp.run

  let solution () =
    let program = program () in
    let possible_input_valus =
      let range =
        let len = Program.length program in
        Sequence.range ~start:`inclusive ~stop:`exclusive 1 len
      in
      Sequence.cartesian_product range range
    in
    let computes_magic_number x =
      run_program program x = 19690720
    in
    match Sequence.find ~f:computes_magic_number possible_input_valus with
    | Some (noun, verb) -> 100 * noun + verb |> Int.to_string
    | None -> "not found!"
end
