(** Code taken from "Unix system programming in OCaml"
  * by Xavier Leroy and Didier Rémy
  *)
open Unix
open Sys

let rec restart_on_EINTR f x =
  try f x with Unix_error (EINTR, _, _) -> restart_on_EINTR f x

let try_finalize f x finally y =
  let res = try f x with exn -> finally y; raise exn in
  finally y;
  res

let install_tcp_server_socket addr =
  let s = socket PF_INET SOCK_STREAM 0 in
  try
    bind s addr;
    listen s 10;
    s
  with z -> close s; raise z

let double_fork_treatment server service (client_descr, _ as client) =
  let treat () = match fork () with
    | 0 ->
        if fork () <> 0 then exit 0;
        close server; service client; exit 0
    | k ->
        ignore (restart_on_EINTR (waitpid []) k)
  in
  try_finalize treat () close client_descr

let tcp_server treat_connection addr =
  ignore (signal sigpipe Signal_ignore);
  let server_sock = install_tcp_server_socket addr in
  while true do
      let client = restart_on_EINTR accept server_sock in
      treat_connection server_sock client
  done
