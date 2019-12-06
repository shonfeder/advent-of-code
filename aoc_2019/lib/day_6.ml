open! Core

module Map = String.Map
module Set = String.Set

let parse_orbit_map input =
  String.strip input
  |> String.split_lines
  |> List.map ~f:String.(split ~on:')')

module Puzzle_1 = struct
  let orbit_graph input =
    let g = Map.empty in
    let add_edge g edge = match edge with
      | [] -> g (* Empty lines *)
      | [key; data] -> Map.add_multi g ~key ~data
      | _ -> raise (Failure "Invalid entry")
    in
    parse_orbit_map input |> List.fold ~f:add_edge ~init:g

  let count_orbits input =
    let orbits = ref 0 in
    let visited = Set.empty in
    let g = orbit_graph input in
    let rec dfs depth node =
      String.Map.find_multi g node |> List.iter ~f:begin fun n ->
        match Set.mem visited n with
        | false -> dfs (depth + 1) n; orbits := !orbits + (depth + 1)
        | true  -> ()
      end
    in
    dfs 0 "COM";
    !orbits

  let%test "ex" =
    let input = "
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
" in
    count_orbits input = 42

  let solution () = Input.read "day_6.txt" |> count_orbits |> Int.to_string
end

module Puzzle_2 = struct
  (* Reversing the direction of the edges *)
  let orbit_transfer_graph input =
    let g = Map.empty in
    let add_edge g edge = match edge with
      | [] -> g
      | [data; key] -> Map.add_exn g ~key ~data
      | _ -> raise (Failure "Invalid entry")
    in
    parse_orbit_map input |> List.fold ~f:add_edge ~init:g

  let find_ancestors g node =
    let rec gather node ancestors =
      match Map.find g node with
      | None -> List.rev ancestors
      | Some n -> gather n (n :: ancestors)
    in
    gather node []

  (* Finds the nearest ancestor of each target *)
  let shortest_path_to_san input =
    let g = orbit_transfer_graph input in
    let you = find_ancestors g "YOU" in
    let san = find_ancestors g "SAN" in
    let dist = ref 0 in
    match List.findi san ~f:(fun _ x ->
        match List.findi you ~f:(fun _ y -> String.equal x y) with
        | Some (yi, _) -> dist := !dist + yi; true
        | None -> false)
    with
    | Some (si, _) -> dist := !dist + si; !dist
    | None -> !dist

  let%test "ex" =
    let input = "
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN
" in
    shortest_path_to_san input = 4

  let solution () = Input.read "day_6.txt" |> shortest_path_to_san |> Int.to_string
end
