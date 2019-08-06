module SerializersTests

open Expecto

open MongoDB.FSharp.Serializers

let stubbed = getClassMap (fun t -> false)

[<Tests>]
let tests =
    testList "serialization" [
        testCase "Serialization options get set correctly" <| fun _ ->
            let classMap = stubbed typeof<List<string>>
            Expect.isSome classMap "expected a classmap"
  ]
