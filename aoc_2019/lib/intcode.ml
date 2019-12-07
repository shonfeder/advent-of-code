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

module Mode = struct
  exception Invalid_mode

  type t =
    | Position
    | Immediate
  [@@deriving enum, show { with_path = false }, eq]

  let of_int i = match of_enum i with
    | Some m -> m
    | None -> raise Invalid_mode
end

module State = struct
  type t = Continue | Halt
  [@@deriving eq, show]
end

module Effect = struct
  type t =
    | Stop
    | Resume
    | Advance
    | Input of (Mode.t * int) (* destination address *)
    | Output of int
  [@@deriving eq, show { with_path = false }]
end

type v_result =
  { outputs: int list
  ; result: int
  ; effects: Effect.t list
  }
[@@deriving show]

module Comp () = struct
  type memory = int array ref
  type address = int
  type raw_instruction = int array

  let memory : memory = ref [||]

  exception Intcode_error of string
  exception Invalid_address of address

  let raise_intcode_error location name params err =
    let params = Array.to_list params |> List.to_string ~f:(fun (m, i) -> Printf.sprintf "%s:%i" (Mode.show m) i) in
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

  module Param : sig
    type t = Mode.t * int
    [@@deriving show, eq]

    val value : t -> address
    val write : t -> int -> unit
    val to_bool : t -> bool
  end = struct
    type t = Mode.t * int
    [@@deriving show, eq]

    (** [param_val mode p] is the value of param [p] derived based on the [mode] *)
    let value : t -> int =
      fun (mode, p) -> match mode with
        | Position -> read_addr p
        | Immediate -> p

    exception Invalid_write_in_immediate_mode of int
    let write : t -> int -> unit =
      fun t v -> match t with
        | (Position, addr)  -> set_addr addr v
        | (Immediate, addr) -> raise (Invalid_write_in_immediate_mode addr)

    let to_bool : t -> bool = fun t -> value t <> 0
  end

  module Instruction = struct

    type executor = Param.t array -> Effect.t

    let array_to_string ~f arr = Array.to_list arr |> List.to_string ~f

    type t =
      { params: Param.t Array.t [@printer fun fmt x -> fprintf fmt "%s" (array_to_string ~f:Param.show x)]
      ; executor: executor [@opaque]
      ; name: string}
    [@@deriving show {with_path = false}]

    module Pointer = struct
      (* Current location *)
      let pointer = ref 0

      let get () = !pointer
      let jump n = pointer := n
      let incr n = jump (get () + n)
      let clear () = jump 0
    end

    let make ~params ~executor ~name =
      {params; executor; name}

    module Operation = struct
      type t =
        { name: string
        ; n_params: int
        ; executor: executor [@opaque]
        }
      [@@deriving show {with_path = false}]

      type dict = t Int.Map.t

      (* TODO Refactor out the instruction reading/parsing. This is very spaggeti *)
      let bin_op : string -> (int -> int -> int) -> t = fun name op ->
        let executor params =
          let a = Param.value params.(0)
          and b = Param.value params.(1)
          in
          (* Printf.printf "Write loc: %s\n" (Param.show params.(2)); *)
          Param.write params.(2) (op a b);
          (* TODO Replace with a [Write] effect *)
          Effect.Advance
        in
        {n_params=3; executor; name}

      let input : t =
        let executor = fun params -> Effect.Input params.(0) in
        { n_params = 1
        ; name = "input"
        ; executor
        }

      let output : t =
        let executor params = Effect.Output (Param.value params.(0)) in
        { n_params = 1
        ; name = "output"
        ; executor
        }

      let jump_if_true : t =
        let executor params =
          match Param.to_bool params.(0) with
          | false -> Effect.Advance
          | true ->
            Pointer.jump (Param.value params.(1));
            Effect.Resume
        in
        { n_params = 2
        ; name = "jump-if-true"
        ; executor
        }

      let jump_if_false : t =
        let executor params =
          match (Param.to_bool params.(0)) with
          | true -> Effect.Advance
          | false ->
            Pointer.jump (Param.value params.(1));
            Effect.Resume
        in
        { n_params = 2
        ; name = "jump-if-false"
        ; executor
        }

      let lt a b = if a < b then 1 else 0
      let eq a b = if a = b then 1 else 0

      let halt : t =
        { n_params = 0
        ; executor = Fn.const Effect.Stop
        ; name = "halt"
        }

      let dict : dict = Int.Map.of_alist_exn
          [ 1, bin_op "add" ( + )
          ; 2, bin_op "multiply" ( * )
          ; 3, input
          ; 4, output
          ; 5, jump_if_true
          ; 6, jump_if_false
          ; 7, bin_op "less than" lt
          ; 8, bin_op "equal" eq
          ; 99, halt
          ]

      exception Invalid_operation of int

      let of_code : int -> t = fun code ->
        match Int.Map.find dict code with
        | None -> raise (Invalid_operation code)
        | Some operation -> operation
    end

    (* Any missing modes default to Position *)
    let pad_modes n modes =
      modes @ List.init n ~f:(fun _ -> Mode.Position) |> Array.of_list

    let rec parse_modes : int -> Mode.t list = function
      | 0 -> []
      | n -> Mode.of_int (n mod 10) :: parse_modes (n / 10)

    let op_modes_of_int : int -> int * Mode.t list =
      fun mode_op ->
      let op_code = mode_op mod 100 in
      let modes = parse_modes (mode_op / 100) in
      (op_code, modes)

    let read_params n modes =
      let vals =
        if n > 0 then
          Array.sub !memory ~pos:(Pointer.get() + 1) ~len:n
        else
          [||]
      in
      Array.zip_exn modes vals

    (** [read ()] is the instruction at the location set by [pointer] *)
    let read : unit -> t = fun () ->
      (* Printf.printf "reading...\n"; *)
      let op_code, modes = op_modes_of_int (read_addr (Pointer.get ())) in
      let operation = Operation.of_code op_code in
      (* Printf.printf "read operation: %s\n" (Operation.show @@ operation); *)
      let modes = pad_modes (operation.n_params - List.length modes) modes in
      let params = read_params operation.n_params modes in
      let instr = {params; executor = operation.executor; name = operation.name} in
      (* Printf.printf "read instruction: %s\n" (show @@ instr); *)
      instr

    (** [execute inst] is the effect resulting from executing the instruction [inst] *)
    let execute : t -> Effect.t = fun {params; executor; name} ->
      try executor params with
      | (Invalid_argument err) -> raise_intcode_error (Pointer.get ()) name params err
      | (Invalid_address err) ->
        let err_msg = Printf.sprintf "Invalid address: %i" err in
        raise_intcode_error (Pointer.get ()) name params err_msg
  end

  let dump : unit -> Program.t = fun () ->
    !memory |> Program.of_array

  let load : Program.t -> unit = fun program ->
    memory := Array.copy (program :> int Array.t);
    Instruction.Pointer.clear ()

  (* Move side effects out of instructions module? *)
  let advance (instr : Instruction.t) = Instruction.Pointer.incr (Array.length instr.params + 1); State.Continue

  let get_input addr_param =
    let value () =
      Printf.printf "> ";
      Out_channel.(flush stdout);
      let input = In_channel.(input_line stdin) in
      Option.value_exn input |> Int.of_string
    in
    Param.write addr_param (value ())

  let write_out i = Printf.printf "[intcomp]: %i\n" i

  let tick : unit -> State.t = fun () ->
    let instr = Instruction.read () in
    match (Instruction.execute instr : Effect.t) with
    | Stop       -> Halt
    | Resume     -> Continue
    | Advance    -> advance instr
    | Input addr -> get_input addr; advance instr
    | Output i   -> write_out i; advance instr

  let run : ?res_loc:int -> Program.t -> int = fun ?(res_loc=0) program ->
    let rec loop : State.t -> unit = function
      | Halt     -> ()
      | Continue -> tick () |> loop
    in
    load program;
    print_endline "Program loaded...";
    loop Continue;
    !memory.(res_loc)

  module Virtual = struct
    type v_state =
      { state: State.t
      ; inputs: int list
      ; output: int option
      ; effect: Effect.t
      }

    let v_state ?output ~state ~inputs effect = {state; inputs; output; effect}

    exception Expected_input

    let tick : int list -> v_state = fun inputs ->
      let instr = Instruction.read () in
      let eff = Instruction.execute instr in
      match (eff : Effect.t) with
      | Stop       -> v_state ~state:Halt ~inputs eff
      | Resume     -> v_state ~state:Continue ~inputs eff
      | Advance    -> v_state ~state:(advance instr) ~inputs eff
      | Output i   -> v_state ~state:(advance instr) ~inputs ~output:i eff
      | Input addr ->
        match inputs with
        | [] -> raise Expected_input
        | (i :: inputs) -> v_state ~state:(Param.write addr i; advance instr) ~inputs eff

    let run : ?res_loc:int -> ?inputs:int list -> Program.t -> v_result =
      fun ?(res_loc=0) ?(inputs=[]) program ->
      let rec loop : v_state -> int list -> Effect.t list -> (int list * Effect.t list) =
        fun vs outputs effects -> match vs.state with
          | Halt     -> List.rev outputs, List.rev effects
          | Continue ->
            let vs' = tick vs.inputs in
            let outputs = match vs'.output with | Some o -> (o :: outputs) | None -> outputs in
            let effects = vs'.effect :: effects in
            loop vs' outputs effects
      in
      load program;
      print_endline "Program loaded (virtually)...";
      let (outputs, effects) = loop (v_state ~state:Continue ~inputs Resume) [] [] in
      {outputs; result = !memory.(res_loc); effects}
  end
end

let%test "modal params" =
  let module Comp = Comp () in
  Program.of_string "1002,4,3,4" |> Comp.load;
  let instr = Comp.Instruction.read () in
  String.equal instr.name "multiply"
  &&
  Array.equal Comp.Param.equal instr.params [|Position, 4; Immediate, 3; Position, 4|]

(* TODO Implement trace of program instructions and effects and states *)
let%test "basic io" =
  let module Comp = Comp () in
  let open Comp in
  let {outputs; effects; _} = Program.of_string "3,0,4,0,99" |> Virtual.run ~inputs:[5]
  in
  let expected_effects = Effect.[Input (Position, 0); Output 5; Stop]
  in
  List.equal Effect.equal expected_effects effects
  &&
  List.equal Int.equal outputs [5]

let%test "equal to" =
  let module Comp = Comp () in
  let open Comp in
  let program = Program.of_string "3,9,8,9,10,9,4,9,99,-1,8" in
  let {outputs; _} = Virtual.run program ~inputs:[8] in
  List.equal Int.equal outputs [1] &&
  let {outputs; _} = Virtual.run program ~inputs:[1] in
  List.equal Int.equal outputs [0]

let%test "less than " =
  let module Comp = Comp () in
  let open Comp in
  let program = Program.of_string "3,9,7,9,10,9,4,9,99,-1,8" in
  let {outputs; _} = Virtual.run program ~inputs:[9] in
  List.equal Int.equal outputs [0] &&
  let {outputs; _} = Virtual.run program ~inputs:[1] in
  List.equal Int.equal outputs [1]

let%test "equal to in immediate mode" =
  let module Comp = Comp () in
  let open Comp in
  let program = Program.of_string "3,3,1108,-1,8,3,4,3,99" in
  let {outputs; _} = Virtual.run program ~inputs:[8] in
  List.equal Int.equal outputs [1] &&
  let {outputs; _} = Virtual.run program ~inputs:[7] in
  List.equal Int.equal outputs [0]

let%test "less than in immediate mode" =
  let module Comp = Comp () in
  let open Comp in
  let program = Program.of_string "3,3,1107,-1,8,3,4,3,99" in
  let {outputs; _} = Virtual.run program ~inputs:[8] in
  List.equal Int.equal outputs [0] &&
  let {outputs; _} = Virtual.run program ~inputs:[7] in
  List.equal Int.equal outputs [1]

let%test "jump using position mode" =
  let module Comp = Comp () in
  let open Comp in
  let program = Program.of_string "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" in
  let {outputs; _} = Virtual.run program ~inputs:[8] in
  List.equal Int.equal outputs [1]
  &&
  let {outputs; _} = Virtual.run program ~inputs:[0] in
  List.equal Int.equal outputs [0]

let%test "jump using immediate mode" =
  let module Comp = Comp () in
  let open Comp in
  let program = Program.of_string "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" in
  let {outputs; _} = Virtual.run program ~inputs:[8] in
  List.equal Int.equal outputs [1]
  &&
  let {outputs; _} = Virtual.run program ~inputs:[0] in
  List.equal Int.equal outputs [0]

let%test "relation of input to 8" =
  let module Comp = Comp () in
  let open Comp in
  let program = Program.of_string
      "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" in
  let {outputs; _} = Virtual.run program ~inputs:[0] in
  List.equal Int.equal outputs [999]
  &&
  let {outputs; _} = Virtual.run program ~inputs:[8] in
  List.equal Int.equal outputs [1000]
  &&
  let {outputs; _} = Virtual.run program ~inputs:[10] in
  List.equal Int.equal outputs [1001]
