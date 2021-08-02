let Prelude = https://prelude.dhall-lang.org/package.dhall

let JSON = Prelude.JSON

let base = ./base.dhall

let Request = ./Types/Request.dhall

let req
    : Request.Type
    =     Request.default
      //  base
      //  { path = "/auth/login"
          , method = "POST"
          , reqBody = Some
              ( JSON.object
                  ( toMap
                      { logIn =
                          JSON.object
                            ( toMap
                                { email = JSON.string "mossss@hoge.com"
                                , password = JSON.string "password"
                                }
                            )
                      }
                  )
              )
          }

in  req
