(** Code adapted from "Unix system programming in OCaml"
  * by Xavier Leroy and Didier Rémy
  *)
open Sys
open Unix
open Misc
open Str

let status_message num =
  match num with
  | 200 -> " OK"
  | 302 -> " Moved Permanently"
  | 404 -> " Not Found"
  | 405 -> " Method Not Allowed"
  | 500 -> " Internal Server Error"
  | _ -> ""

let status ?(hdrs = []) num =
  "HTTP/1.1 " ^ string_of_int num ^ status_message num ^
  (if List.length hdrs > 0 then "\r\n" else "") ^
  String.concat "\r\n" hdrs ^
  "\r\n\r\n"

let response num hdrs body =
  let preamble = status num ~hdrs in
  preamble ^ body

let buf = Bytes.create 1024

let parse_request req =
  let parts = Str.split (Str.regexp "\r\n\r\n") req in
  let lines = Str.split (Str.regexp "\r\n") (List.hd parts) in
  let result = Str.split (Str.regexp "[ \t]+") (List.hd lines) in
  if List.length result > 1 then
    let body = if List.length parts > 1 then List.nth parts 1 else "" in
    Some (List.nth result 0, List.nth result 1, body)
  else
    None

module StringMap = Map.Make(String)
(* module VerbMap = Map.Make(String) *)
let routes = StringMap.empty
  |> StringMap.add_seq (List.to_seq [
    "/redirect", StringMap.singleton "GET" (status 301 ~hdrs:["Location: http://127.0.0.1:5000/simple_get"]);
    "/method_options", StringMap.singleton "OPTIONS" (status 200 ~hdrs:["Allow: GET, HEAD, OPTIONS"]);
    "/method_options2", StringMap.singleton "OPTIONS" (status 200 ~hdrs:["Allow: GET, HEAD, OPTIONS, PUT, POST"]);
    "/head_request", StringMap.empty
    |> StringMap.add "GET" (status 405 ~hdrs:["Allow: HEAD, OPTIONS"])
    ])

let find_route path verb =
  let with_path k _ = k = path in
  let with_verb k _ = k = verb in
  if StringMap.exists with_path routes then
    let route = StringMap.find path routes in
    if StringMap.exists with_verb route then
      StringMap.find verb route
    else
      status 405
  else
    status 404

let handler (str: string): string =
  match parse_request str with
  | None ->
    status 500
  | Some(verb, path, body) ->
    match path with
    | "/redirect" -> begin match verb with
      | "GET" -> status 301 ~hdrs:["Location: http://127.0.0.1:5000/simple_get"]
      | _ -> status 405
      end
    | "/method_options" -> begin match verb with
      | "OPTIONS" -> status 200 ~hdrs:["Allow: GET, HEAD, OPTIONS"]
      | _ -> status 405
      end
    | "/method_options2" -> begin match verb with
      | "OPTIONS" -> status 200 ~hdrs:["Allow: GET, HEAD, OPTIONS, PUT, POST"]
      | _ -> status 405
      end
    | "/head_request" -> begin match verb with
      | "GET" -> status 405 ~hdrs:["Allow: HEAD, OPTIONS"]
      | "HEAD" -> status 200
      | _ -> status 405
      end
    | "/simple_get" -> begin match verb with
      | "GET" -> status 200
      | "HEAD" -> status 200
      | _ -> status 405
      end
    | "/simple_get_with_body" -> begin match verb with
      | "GET" -> response 200 [] "Hello world"
      | _ -> status 405
      end
    | "/echo_body" -> begin match verb with
      | "POST" -> response 200 [] body
      | _ -> status 405
      end
    | _ -> status 404

let server () =
  if Array.length Sys.argv < 2 then begin
    prerr_endline "Usage: server <port> <command> [arg1 ... argn]";
    exit 2
  end;
  let port = int_of_string Sys.argv.(1) in
  let host = (gethostbyname("localhost")).h_addr_list.(0) in
  let addr = ADDR_INET (host, port) in
  let treat sock (client_sock, client_addr as client) =
    begin match client_addr with
    | ADDR_INET(caller, _) ->
      prerr_endline ("* Connection from " ^ string_of_inet_addr caller);
    | ADDR_UNIX _ ->
      prerr_endline "* Connection from the Unix domain (???)";
    end;
    let service (s, _) =
      let num_read = read s buf 0 1024 in
      let str = String.sub (Bytes.to_string buf) 0 num_read in
      let response = handler str in
      prerr_string ("> " ^ response);
      write_substring s response 0 (String.length response)
    in
    Misc.double_fork_treatment sock service client in
  begin match addr with
  | ADDR_INET (inetaddr, _) ->
    prerr_endline ("Listening on " ^ string_of_inet_addr inetaddr)
  | _ ->
    prerr_endline ("Listing on unix socket")
  end;
  Misc.tcp_server treat addr;;

handle_unix_error server ()
