let Token =
      "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VySWQiOiIwMUY5OTJaS1lTRUhBQ1RNMVRCQ1NTNjY0MiIsImlhdCI6MTYyNzAyMTA4OCwiZXhwIjoxNjI3MDI0Njg4fQ._7i2ToYQATRO5dMpZkn4HMiHvJ1R99GH93deMFQEWHI"

in  { baseUrl = "http://localhost:3030"
    , headers = toMap
        { Content-Type = "application/json"
        , Authorization = "Bearer " ++ Token
        , x-organization-id = "01F992ZM2TBXP8WZ2GKS2HCG4Z"
        }
    }
