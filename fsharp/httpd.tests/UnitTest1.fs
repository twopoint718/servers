module httpd.tests

open NUnit.Framework
open Program

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``test method parsing`` () =
    Assert.AreEqual(GET, parseMeth "GET")

[<Test>]
let ``Create request from incoming message`` () =
    let expected = Request(GET, "/foo")
    let actual = Request.create("GET /foo HTTP/1.1\r\n\r\n")
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Add headers to response`` () =
    let expected = "HTTP/1.1 301 Moved Permanently\r\nLocation: http://127.0.0.1:5000/simple_get\r\n\r\n"
    let actual = Response(301, headers=["Location: http://127.0.0.1:5000/simple_get"])
    Assert.AreEqual(expected, actual.ToString())

[<Test>]
let ``Empty body`` () =
    let expected = "HTTP/1.1 200 OK\r\n\r\n"
    Assert.AreEqual(expected, Response(200).ToString())

[<Test>]
let ``Empty body with headers present`` () =
    let expected = "HTTP/1.1 200 OK\r\nDate: today\r\n\r\n"
    Assert.AreEqual(expected, Response(200, headers=["Date: today"]).ToString())

[<Test>]
let ``Empty headers but body present`` () =
    let expected = "HTTP/1.1 200 OK\r\n\r\nbody"
    Assert.AreEqual(expected, Response(200, body="body").ToString())
