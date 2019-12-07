open! Core

(* From http://rosettacode.org/wiki/Permutations#OCaml *)
let rec permutations l =
  let n = List.length l in
  if n = 1 then [l] else
    let rec sub e = function
      | [] -> failwith "sub"
      | h :: t -> if h = e then t else h :: sub e t in
    let rec aux k =
      let e = List.nth_exn l k in
      let subperms = permutations (sub e l) in
      let t = List.map ~f:(fun a -> e::a) subperms in
      if k < n-1 then List.rev_append t (aux (k+1)) else t in
    aux 0

let amplifier program phase_setting input =
  let module Comp = Intcode.Comp () in
  let Intcode.{outputs; _} =
    Comp.Virtual.run ~inputs:[phase_setting; input] program
  in
  List.hd_exn outputs


module Puzzle_1 = struct
  let configure_amplifiers program phase_settings =
    let make_amp = amplifier program in
    let amps = List.map ~f:make_amp phase_settings in
    let chain_amp input amp = amp input in
    List.fold ~init:0 ~f:chain_amp amps

  let possible_phase_settings () =
    List.range ~start:`inclusive ~stop:`inclusive 0 4
    |> permutations

  let find_optimal_phase_settings program =
    Option.value_exn (possible_phase_settings ()
                      |> List.map ~f:(configure_amplifiers program)
                      |> List.max_elt ~compare:Int.compare
                     )

  let solution () =
    Input.read "day_7.txt"
    |> Intcode.Program.of_string
    |> find_optimal_phase_settings
    |> Int.to_string

  open Intcode
  let%test "ex1" =
    Program.of_string "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
    |> find_optimal_phase_settings
       =
       43210
end

module Puzzle_2 = struct
  let run_amplifiers program phase_settings =
    let make_amp = amplifier program in
    let amps = List.map ~f:make_amp phase_settings in
    let chain_amp input amp = amp input in
    List.fold ~init:0 ~f:chain_amp amps

  let possible_phase_settings () =
    List.range ~start:`inclusive ~stop:`inclusive 5 9
    |> permutations

  let find_optimal_feedback_phase_settings program =
    let amps = possible_phase_settings ()
        Option.value_exn (
        |> List.map ~f:(configure_amplifiers program)
        |> List.max_elt ~compare:Int.compare
      )

end
