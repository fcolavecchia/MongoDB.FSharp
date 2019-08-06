module AcceptanceTests

// open Xunit
// open Swensen.Unquote
// open Swensen.Unquote.Assertions
open Expecto
open MongoDB.Bson
open MongoDB.Driver
open MongoDB.Driver.Linq

open MongoDB.FSharp
open System.Linq
open System.Threading.Tasks
open Microsoft.FSharp.Linq

let newObjectId () = BsonObjectId(ObjectId.GenerateNewId())

type ObjectWithList() =
    member val Id : BsonObjectId = newObjectId() with get, set
    member val List : string list = [] with get, set

type RecordType = {
    Id : BsonObjectId
    Name : string
}

type Child = {
    ChildName: string
    Age: int
}

type Person = {
    Id: BsonObjectId
    PersonName: string
    Age: int
    Childs: Child seq
}

type DimmerSwitch =
    | Off
    | Dim of int
    | DimMarquee of int * string
    | On

type ObjectWithOptions() =
    member val Id : BsonObjectId = newObjectId() with get, set
    member val Age : int option = None with get, set

type ObjectWithDimmer() =
    member val Id : BsonObjectId = newObjectId() with get, set
    member val Switch : DimmerSwitch = Off with get, set

type ObjectWithDimmers() =
    member val Id : BsonObjectId = newObjectId() with get, set
    member val Kitchen : DimmerSwitch = Off with get, set
    member val Bedroom1 : DimmerSwitch = Off with get, set
    member val Bedroom2 : DimmerSwitch = Off with get, set

let findOne<'T> (db : IMongoDatabase) name id =
    async {
        let collection = db.GetCollection<'T> name
        let! fromDb = findOneById collection id
        return fromDb
    }

let findOnePerson<'T> db id =
  findOne<'T> db "persons" id

let findOneObject<'T> db id =
  findOne<'T> db "objects" id

[<Tests>]
let tests =
    let db = MongoClient("mongodb://localhost/test").GetDatabase("test")
    Serializers.Register()

    let insertOnePerson = insertOne (db.GetCollection "persons")
    let insertOneObject (o : 'T)  = insertOne (db.GetCollection "objects") o

    let teardown() =
        async {
            do! db.DropCollectionAsync "objects" |> Async.AwaitTask
            do! db.DropCollectionAsync "persons" |> Async.AwaitTask
        } |> Async.RunSynchronously

    testList "When serializing lists" [

        /// Seems to be fixed in version 1.5 of the C# driver

        testCase "It can serialize an object with a list" <| fun _ ->
            async {
                let obj = ObjectWithList()
                obj.List <- [ "hello"; "world" ]
                do! insertOneObject obj

                let! fromDb = findOneObject<BsonDocument> db obj.Id
                let array = fromDb.["List"].AsBsonArray
                Expect.equal 2 array.Count "Array count should have been 2"
            } |> Async.RunSynchronously


        testCase "It can deserialze lists" <| fun _ ->
            async {
                let list = BsonArray([ "hello"; "world" ])
                let id = newObjectId()
                let document = BsonDocument([ BsonElement("_id", id); BsonElement("List", list) ])
                do! insertOneObject document

                let! fromDb = findOneObject<ObjectWithList> db id

                let array = fromDb.List
                Expect.equal 2 array.Length "Array length should have been 2"
            } |> Async.RunSynchronously


        testCase "It can serialize records" <| fun _ ->
            async {
                let obj = { Id = newObjectId(); Name = "test"  }
                do! insertOneObject obj

                let! fromDb = findOneObject<BsonDocument> db obj.Id
                let test = fromDb.["Name"].AsString
                Expect.equal "test" test "Should have round-tripped the string \"test\""
            } |> Async.RunSynchronously


        testCase "It can deserialize records" <| fun _ ->
            async {
                let id = newObjectId()
                let document = BsonDocument([BsonElement("_id", id); BsonElement("Name", BsonString("value"))])
                do! insertOneObject document

                let! fromDb = findOneObject<RecordType> db id
                Expect.isNotNull (box fromDb) "Should not have returned null"
                Expect.equal "value" fromDb.Name "Should have round-tripped the string \"value\""
            } |> Async.RunSynchronously


        testCase "It can serialize and deserialize nested records" <| fun _ ->
            async {
                let collection = db.GetCollection<Person> "persons"
                let obj = { Id = newObjectId(); PersonName = "test"; Age = 33; Childs = [{ChildName = "Adrian"; Age = 3}] }
                do! insertOnePerson obj

                let! person = findOnePerson db obj.Id

                Expect.isNotNull (box person) "Should not have returned null"
                Expect.equal "test" person.PersonName "Name did not round-trip"
                Expect.equal 33 person.Age "Age did not round-trip"
                Expect.equal 1 (person.Childs |> Seq.length) "Children list did not round-trip"

                let child = person.Childs |> Seq.head

                Expect.equal "Adrian" child.ChildName "Child name did not round-trip"
                Expect.equal 3 child.Age "Child age did not round-trip"
            } |> Async.RunSynchronously



        testCase "It can serialize option types" <| fun _ ->
          async {
                let obj = ObjectWithOptions()
                obj.Age <- Some 42

                do! insertOneObject obj

                let! fromDb = findOneObject<BsonDocument> db obj.Id

                Expect.isNotNull (box fromDb) "Should not have returned null"
                let age = fromDb.GetElement("Age")
                Expect.isNotNull (box age) "Age should not have been null"
                Expect.equal "Some" (age.Value.AsBsonDocument.GetElement("_t").Value.AsString) "BSON should have a _t field for Age that registers as Some"
                let value = age.Value.AsBsonDocument.GetElement("_v").Value
                Expect.isTrue value.IsBsonArray "Value should have been a BSON array"
                let array = value.AsBsonArray
                Expect.equal 1 array.Count "Array should have one item"
                Expect.equal 42 array.[0].AsInt32 "Array's single item should be what we put in"
          } |> Async.RunSynchronously


        testCase "It can serialize DimmerSwitch types" <| fun _ ->
            async {
                let obj = ObjectWithDimmer()
                obj.Switch <- DimMarquee(42, "loser")

                do! insertOneObject obj

                let! fromDb = findOne<BsonDocument> db "objects" obj.Id

                Expect.isNotNull (box fromDb) "Should not have returned null"
                let switch = fromDb.GetElement("Switch")
                Expect.isNotNull (box switch) "Switch should not be null"
                Expect.equal "DimMarquee" (switch.Value.AsBsonDocument.GetElement("_t").Value.AsString) "DU types should have a _t field representing what they are"
                let value = switch.Value.AsBsonDocument.GetElement("_v").Value
                Expect.isTrue value.IsBsonArray "Value should be a BSON array"
                let array = value.AsBsonArray
                Expect.equal 2 array.Count "Array count should be 2"
                Expect.equal 42 array.[0].AsInt32 "Array's first item should be the int 42"
                Expect.equal "loser" array.[1].AsString "Array's second item should be the string \"loser\""
            } |> Async.RunSynchronously


        testCase "It can deserialize option types" <| fun _ ->
          async {
              let id = newObjectId()
              let arrayPart = BsonArray([ BsonInt32(42) ])
              let structure = BsonDocument([BsonElement("_t", BsonString("Some")); BsonElement("_v", arrayPart)])
              let document = BsonDocument([BsonElement("_id", id); BsonElement("Age", structure)])

              do! insertOneObject document

              let! fromDb = findOneObject<ObjectWithOptions> db id
              match fromDb.Age with
              | Some 42 -> ()
              | _ -> failwith "expected Some 42 but got something else"
          } |> Async.RunSynchronously


        testCase "We can integrate serialize & deserialize on DimmerSwitches" <| fun _ ->
          async {
              let obj = ObjectWithDimmers()
              obj.Kitchen <- Off
              obj.Bedroom1 <- Dim 42
              obj.Bedroom2 <- DimMarquee(12, "when I was little...")

              do! insertOneObject obj


              let! fromDb = findOneObject<ObjectWithDimmers> db obj.Id
              match fromDb.Kitchen with
              | Off -> ()
              | _ -> failwith "Kitchen light wasn't off"

              match fromDb.Bedroom1 with
              | Dim 42 -> ()
              | _ -> failwith "Bedroom1 light wasn't dim enough"

              match fromDb.Bedroom2 with
              | DimMarquee(12, "when I was little...") -> ()
              | _ -> failwith "Bedroom2 doesn't have the party we thought"
          } |> Async.RunSynchronously
]
