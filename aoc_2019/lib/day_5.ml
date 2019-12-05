module Puzzle = struct
  open Intcode
  module Comp = Comp ()

  let solution () =
    let _ = Input.read "day_5.txt" |> Program.of_string |> Comp.run in
    "done"
end
