open! Core

let read fname =
  Fpath.(v "input" / fname)
  |> Fpath.to_string
  |> In_channel.read_all
