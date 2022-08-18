open System.Net
open System.Net.Sockets
open System.Text
open System

type HttpVerb = GET | POST | HEAD | OPTIONS
let parseMeth s =
    match s with
    | "GET" -> GET
    | "POST" -> POST
    | "HEAD" -> HEAD
    | "OPTIONS" -> OPTIONS
    | _ -> failwith ("Unknown HTTP verb: " + s)

type Request(verb : HttpVerb, route : string, ?headers : string list, ?body : string) =
    member this.Verb = verb
    member this.Route = route
    static member create(req : string) =
        let parts = req.Split("\r\n\r\n")
        if parts.Length > 0 then
            let preamble = parts.[0].Split(" ")
            Request(parseMeth preamble.[0], preamble.[1])
        else
            failwith $"Cannot parse request: {req}"
    override this.ToString() = $"{this.Verb} {this.Route}"
    override this.GetHashCode() =
        this.Route.GetHashCode() + this.Verb.GetHashCode()
    override this.Equals(obj) =
        match obj with
        | :? Request as r -> this.Verb = r.Verb && this.Route = r.Route
        | _ -> false

(** Use an option like a bool. If `m` is `Some(_)` then pick the first arg, `x`
  * otherwise pick the second arg, `y`.
  *)
let choose m x y =
    match m with
    | Some(_) -> x
    | None -> y

type Response(num : int, ?headers : string list, ?body : string) =
    let status = num
    let status_message num =
        match num with
        | 200 -> "OK"
        | 301 -> "Moved Permanently"
        | 500 -> "Internal Server Error"
        | _ -> ""
    override this.ToString() =
        let headerString = (String.concat "\r\n" (defaultArg headers [])
            + choose headers "\r\n" "")
        let bodyString = defaultArg body ""
        $"HTTP/1.1 {num} {status_message num}\r\n{headerString}\r\n{bodyString}"

let log (msg : String) =
    Console.Error.WriteLine ("* " + msg)

let encode (resp : Response): byte[] =
    Encoding.ASCII.GetBytes (resp.ToString())

let decode (data : byte[]): Request =
    let req = Encoding.ASCII.GetString data
    Request.create(req)

let status_message num =
    match num with
    | 200 -> "OK"
    | 500 -> "Internal Server Error"
    | _ -> ""

let routes = [
    "/echo_body", [
        POST, Response(200, body="some body")
    ];
    "/head_request", [
        HEAD, Response(200);
        OPTIONS, Response(200);
    ];
    "/method_options", [
        OPTIONS, Response(200, ["Allow: GET, HEAD, OPTIONS"])
    ];
    "/method_options2", [
        OPTIONS, Response(200, ["Allow: GET, HEAD, OPTIONS, PUT, POST"])
    ];
    "/redirect", [
        GET, Response(301, ["Location: http://127.0.0.1:5000/simple_get"])
    ];
    "/simple_get", [
        GET, Response(200);
        HEAD, Response(200);
    ];
    "/simple_get_with_body", [
        GET, Response(200, body="Hello world")
    ];
]

let handler (req : Request) : Response =
    let matchingRoute (k, _) = k = req.Route
    let matchingVerb (k, _) = k = req.Verb
    match List.tryFind matchingRoute routes with
    | None -> Response(404)
    | Some(_, verbs) ->
        begin match List.tryFind matchingVerb verbs with
        | None ->
            let allow = String.concat ", " (List.map (fun (v,_) -> v.ToString()) verbs)
            Response(405, [$"Allow: {allow}"])
        | Some(_, resp) -> resp
        end

[<EntryPoint>]
let main args =
    let localhost = IPAddress.Loopback
    let listener = TcpListener(localhost, 5000)
    listener.Start(1)
    log("Server started on localhost...")
    let buf: byte[] = Array.create 1024 0uy
    while true do
        let sock = listener.AcceptSocket();      
        sock.Receive(buf) |> ignore
        log ($"Received: {decode buf}")
        let response =
            decode buf
            |> handler
            |> encode
        sock.Send(response) |> ignore
        sock.Close 5
    0
