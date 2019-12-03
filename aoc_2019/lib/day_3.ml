open! Core

let (%) f g x = f (g x)
let bimap (f, g) (x, y) = (f x, g y)

module Move = struct
  type dir = U | D | L | R
  [@@deriving show { with_path = false }]

  type t = dir * int
  [@@deriving show]

  let dir_of_char = function
    | 'U' -> Some U
    | 'D' -> Some D
    | 'L' -> Some L
    | 'R' -> Some R
    | _   -> None

  let delta : t -> (int -> int) * (int -> int) = function
    | (U, n) -> (Fn.id, fun y -> y + n)
    | (D, n) -> (Fn.id, fun y -> y - n)
    | (L, n) -> ((fun x -> x - n), Fn.id)
    | (R, n) -> ((fun x -> x + n), Fn.id)

  let to_string (dir, dist) = show_dir dir ^ (Int.to_string dist)
end

module Path =  struct
  type t = Move.t list
  [@@deriving show]

  let (let*) = Angstrom.(>>=)
  let (let+) = Angstrom.(>>|)

  let parse : string -> (t list, string) Result.t = fun input ->
    let open Angstrom in
    let integer = take_while1 (function '0'..'9' -> true | _ -> false) in
    let dir  =
      let* c = satisfy (Option.is_some % Move.dir_of_char) in
      Option.value_exn (Move.dir_of_char c)
      |> return
    in
    let dist = integer >>= return % Int.of_string in
    let move =
      let* dir'  = dir in
      let* dist' = dist in
      return (dir', dist')
    in
    let comma = char ',' in
    let nl = char '\n' in
    let path = sep_by comma move in
    parse_string (sep_by nl path) input

  let%test "can parse paths" =
    let paths_str =
      "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83
"
    in
    match parse paths_str with
    | Error str -> print_endline str; false
    | Ok (((R,75)::(D,30)::_) ::
          ((U,62)::(R,66)::_) :: _) -> true
    | _ -> false
end

module Coord = struct
  module T = struct
    include Tuple.Make (Int) (Int)
    include Tuple.Comparable (Int) (Int)
  end
  include T

  let distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

  let range start stop =
    if Int.(stop < start) then
      List.range ~start:`inclusive ~stop:`inclusive stop start |> List.rev
    else
      List.range ~start:`inclusive ~stop:`inclusive start stop

  let line_points (x1, y1) (x2, y2) =
    let xs = range x1 x2 in
    let ys = range y1 y2 in
    let xs, ys = match List.length xs, List.length ys with
      | 1, len_y when Int.(len_y > 0) -> (List.init ~f:(Fn.const x1) len_y, ys)
      | len_x, 1 when Int.(len_x > 0) -> (xs, List.init ~f:(Fn.const y1) len_x)
      | 0, _ | _, 0 -> raise (Failure "zeroes")
      | _, _ -> xs, ys
    in
    List.zip_exn xs ys

  (* the points of the line and the new position *)
  let trace position move : (t list * t) =
    let position' = bimap (Move.delta move) position in
    (line_points position position', position')

  let set_of_path ?(origin=(0,0)) path =
    let init = (Set.empty, origin) in
    let f (coords, position) move =
      let (points, position') = trace position move in
      let coords' = points |> List.fold ~init:coords ~f:Set.add
      in
      (coords', position')
    in
    List.fold ~init ~f path |> fst

  let get_incr r =
    let x = !r in
    incr r; x

  let set_of_path_with_step_count ?(origin=(0,0)) path =
    let steps = ref 1 in
    let init = (Set.empty, Map.empty, origin) in
    let f (coords, step_count, position) move =
      let (points, position') = trace position move in
      let step_count' =
        let add_step map key =
          match Map.add map ~key ~data:(get_incr steps) with
          | `Duplicate -> map
          | `Ok map' -> map'
        in
        List.fold ~init:step_count ~f:add_step (List.tl_exn points)
      in
      let coords' = List.fold ~init:coords ~f:Set.add points
      in
      (coords', step_count', position')
    in
    let (coords, step_counts, _) = List.fold ~init ~f path in
    (coords, step_counts)
end

(* Solutions *)

let parse_paths_exn input =
  match Path.parse input |> Result.ok_or_failwith with
  | [a; b] -> a, b
  | _  -> raise (Failure "invalid paths parsing")

module Puzzle_1 = struct
  let distance_to_closest_intersection a_coords b_coords =
    let dists = Coord.Set.inter a_coords b_coords
                |> (fun s -> Coord.Set.remove s (0, 0))
                |> Set.map ~f:(Coord.distance (0, 0)) (module Int)
    in
    Set.min_elt_exn dists

  let find_nearest_intersection input =
    let a, b = parse_paths_exn input in
    let a_coords = Coord.set_of_path a in
    let b_coords = Coord.set_of_path b in
    distance_to_closest_intersection a_coords b_coords

  let%test "ex 0" =
    let input =
      "R8,U5,L5,D3
U7,R6,D4,L4"
    in
    find_nearest_intersection input = 6

  let%test "ex 1" =
    let input = "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83"
    in
    find_nearest_intersection input = 159

  let%test "ex 2" =
    let input =
      "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
    in
    find_nearest_intersection input = 135

  let solution () =
    Input.read "day_3.txt" |> find_nearest_intersection |> Int.to_string
end

module Puzzle_2 = struct

  let (let+) = Option.Monad_infix.(>>|)
  let (and+) = Option.both

  let distance_to_quickest_intersection (a_coords, a_steps) (b_coords, b_steps) =
    let combined_steps coord =
      let steps =
        let+ a = Coord.Map.find a_steps coord
        and+ b = Coord.Map.find b_steps coord
        in
        a + b
      in Option.value_exn steps
    in
    let intersections = Set.inter a_coords b_coords |> (fun s -> Coord.Set.remove s (0, 0)) in
    let steps = Set.map ~f:combined_steps (module Int) intersections in
    Set.min_elt_exn steps

  let find_quickest_intersection input =
    let a, b = parse_paths_exn input in
    let a_coords = Coord.set_of_path_with_step_count a in
    let b_coords = Coord.set_of_path_with_step_count b in
    distance_to_quickest_intersection a_coords b_coords

  let%test "ex 0" =
    let input =
      "R8,U5,L5,D3
U7,R6,D4,L4"
    in
    let actual = find_quickest_intersection input in
    actual = 30

  let%test "ex 1" =
    let input =
      "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83"
    in
    let actual = find_quickest_intersection input in
    actual = 610

  let%test "ex 1" =
    let input =
      "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
    in
    let actual = find_quickest_intersection input in
    actual = 410

  let solution () =
    Input.read "day_3.txt" |> find_quickest_intersection |> Int.to_string
end
