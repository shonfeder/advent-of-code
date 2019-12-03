open! Core

module Program : sig
  type t = private int array

  val of_string : string -> t
  val of_file : string -> t
  val of_array : int array -> t

  val with_noun : int -> t -> t
  val with_verb : int -> t -> t

  val length : t -> int
end = struct
  type t = int array

  let of_string str =
    str
    |> String.strip
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
    |> Array.of_list

  let of_file f =
    In_channel.read_all f |> of_string

  let of_array t = t

  let with_noun i program =
    let p = Array.copy program in
    Array.(set p 1 i);
    p

  let with_verb i program =
    let p = Array.copy program in
    Array.(set p 2 i);
    p

  let length = Array.length
end

module Comp () = struct
  type memory = int array ref
  type address = int
  type instruction = int array

  type state = Continue | Halt

  type executor  = int array -> state
  type operation = {n_vals: int; executor: executor; name: string}

  type op_code_map = operation Int.Map.t

  let memory : memory = ref [||]

  exception Intcode_error of string
  exception Invalid_address of address

  let raise_intcode_error location name params err =
    let params = Array.to_list params |> List.to_string ~f:Int.to_string in
    let msg =
      Printf.sprintf "location: %i; instruction: %s; params: %s; err: %s"
        location name params err
    in
    raise (Intcode_error msg)

  let read_addr : address -> int = fun addr ->
    if addr < Array.length !memory then
      !memory.(addr)
    else
      raise (Invalid_address addr)

  let set_addr : address -> int -> unit = fun addr v ->
    if addr < Array.length !memory then
      Array.set !memory addr v
    else
      raise (Invalid_address addr)

  module Instruction = struct
    (* Current location *)
    let pointer = ref 0

    let clear_pointer () = pointer := 0

    let bin_op : string -> (int -> int -> int) -> operation = fun name op ->
      let executor params =
        let a   = read_addr params.(0)
        and b   = read_addr params.(1)
        and loc = params.(2)
        in
        set_addr loc (op a b);
        Continue
      in
      {n_vals = 3; executor; name}

    let halt = {n_vals = 0; executor = Fn.const Halt; name = "halt"}

    let operations : op_code_map = Int.Map.of_alist_exn
        [ 1, bin_op "plus" ( + );
          2, bin_op "times" ( * );
          99, halt ]

    exception Invalid_instruction of int

    let read : unit -> operation = fun () ->
      let op_int = !memory.(!pointer) in
      match Int.Map.find operations op_int with
      | None -> raise (Invalid_instruction op_int)
      | Some operation -> operation

    let execute : operation -> state = fun {n_vals; executor; name} ->
      let mem = !memory in
      let params =
        if n_vals > 0 then
          Array.sub mem ~pos:(!pointer + 1) ~len:n_vals
        else
          [||]
      in
      match executor params with
      | Continue -> pointer := (!pointer + n_vals + 1); Continue
      | Halt     -> Halt
      | exception (Invalid_argument err) ->
        raise_intcode_error !pointer name params err
      | exception (Invalid_address err) ->
        let err_msg = Printf.sprintf "Invalid address: %i" err in
        raise_intcode_error !pointer name params err_msg
  end

  let dump : unit -> Program.t = fun () ->
    !memory |> Program.of_array

  let load : Program.t -> unit = fun program ->
    memory := Array.copy (program :> int Array.t);
    Instruction.clear_pointer ()

  let run : ?res_loc:int -> Program.t -> int = fun ?(res_loc=0) program ->
    load program;
    let rec loop = function
      | Halt -> ()
      | Continue ->
        () |> Instruction.read |> Instruction.execute |> loop
    in
    loop Continue;
    !memory.(res_loc)
end
