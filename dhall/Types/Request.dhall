let Prelude = https://prelude.dhall-lang.org/package.dhall

in  { Type =
        { baseUrl : Text
        , path : Text
        , method : Text
        , headers : Prelude.Map.Type Text Text
        , reqBody : Optional Prelude.JSON.Type
        }
    , default =
      { headers = Prelude.Map.empty, reqBody = None Prelude.JSON.Type }
    }
