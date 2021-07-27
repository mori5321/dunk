let base = ./base.dhall

let Request = ./Types/Request.dhall

let req
    : Request.Type
    = Request.default // base // { path = "/users/returnForms", method = "GET" }

in  req
