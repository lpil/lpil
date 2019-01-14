port module Sample
    exposing
        ( SampleId
        , play
        )


type alias SampleId =
    String


port play : SampleId -> Cmd msg
