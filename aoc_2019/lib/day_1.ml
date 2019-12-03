open! Core

let module_fuel_requirements () : int list =
  In_channel.read_lines "./input/day_1.txt"
  |> List.map ~f:Int.of_string

let solve requirement_calculator =
  let f accum x = requirement_calculator x + accum in
  module_fuel_requirements ()
  |> List.fold ~f ~init:0
  |> Int.to_string

module Puzzle_1 = struct
  let fuel_required : int -> int =
    fun mass -> (mass / 3) - 2

  let solution () =
    solve fuel_required
end

module Puzzle_2 = struct
  let rec fuel_required : int -> int =
    fun mass ->
      let req = Puzzle_1.fuel_required mass in
      if req < 0 then
        0
      else
        req + fuel_required req

  let solution () =
    solve fuel_required
end
