module Backend.CreateUser exposing (query)

import Json.Encode as Encode
import Json.Decode as Decode
import Types exposing (Msg(..))
import Backend


{-| Attempt create a user with the Graph Cool backend using
the Auth0 idToken.

-}
query : String -> Backend.Query ()
query idToken =
    { name = "CreateUser"
    , responseMsg = CreateUserResponse
    , responseDecoder = Decode.succeed ()
    , variables =
        [ ( "idToken", Encode.string idToken )
        ]
    , graphQl = """
mutation CreateUser(
    $idToken: String!
) {
    createUser(authProvider: { auth0: { idToken: $idToken }}) {
        id
    }
}
"""
    }
