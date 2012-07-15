﻿namespace MongoDB.FSharp

open System
open System.Reflection
open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Options
open MongoDB.Bson.Serialization.Serializers

open SerializationOptions

module Seq =
    let tryHead s =
        if Seq.isEmpty s then
            None
        else
            Some (s |> Seq.head)

module Serializers =

    type MongoDB.Bson.IO.BsonWriter with
        member inline this.WriteEmptyArray() =
            this.WriteStartArray()
            this.WriteEndArray()

    type ListSerializer<'T>() =
        inherit MongoDB.Bson.Serialization.Serializers.BsonBaseSerializer(ListSerializationOptions())

        override this.Serialize(writer : BsonWriter, nominalType : Type, value : Object, options : IBsonSerializationOptions) =
            if value = null then
                // There aren't supposed to be null values in F#
                writer.WriteEmptyArray()
            else
                let actualType = value.GetType()
                this.VerifyTypes(nominalType, actualType, typeof<list<'T>>)

                let lst = value :?> list<'T>
                writer.WriteStartArray()
                
                lst |> List.iter (fun item -> 
                    BsonSerializer.Serialize(writer, typeof<'T>)
                )

                writer.WriteEndArray()

        override this.Deserialize(reader : BsonReader, nominalType : Type, actualType : Type, options : IBsonSerializationOptions) =
            let serializationOptions = this.EnsureSerializationOptions<ListSerializationOptions>(options)
            let itemOptions = serializationOptions.ItemSerializationOptions
            let readArray() =
                seq {
                    reader.ReadStartArray()
                    let convention = BsonSerializer.LookupDiscriminatorConvention(typeof<'T>)
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        let actualElementType = convention.GetActualType(reader, typeof<'T>)
                        let serializer = BsonSerializer.LookupSerializer(actualElementType)
                        let element = serializer.Deserialize(reader, typeof<'T>, itemOptions)
                        yield element :?> 'T
                    reader.ReadEndArray()
                }

            let readArrayFromObject () =
                reader.ReadStartDocument()
                reader.ReadString("_t") |> ignore
                reader.ReadName("_v")
                let value = this.Deserialize(reader, actualType, actualType, options)
                reader.ReadEndDocument()
                value
            
            let bsonType = reader.GetCurrentBsonType()
            match bsonType with
            | BsonType.Null ->
                reader.ReadNull()
                null
            | BsonType.Array -> readArray() |> List.ofSeq :> Object
            | BsonType.Document -> readArrayFromObject ()
            | _ -> 
                let msg = sprintf "Can't deserialize a %s from BsonType %s" actualType.FullName (bsonType.ToString())
                raise(InvalidOperationException(msg))

        interface IBsonArraySerializer with
            member this.GetItemSerializationInfo() : BsonSerializationInfo =
                let elementName = null
                let nominalType = typeof<'T>
                let serializer = BsonSerializer.LookupSerializer nominalType
                BsonSerializationInfo(elementName, serializer, nominalType, null)


    let getClassMap isClassMapRegistered (actualType : Type) =
        let rec getMember (_type : Type) name other =
            let memberInfos = _type.GetMember name
            if not (memberInfos |> Seq.isEmpty) then
                Some(Seq.head memberInfos)
            elif other <> null then
                getMember _type other null
            else
                None

        if not (isClassMapRegistered actualType) then
            let genericType = typedefof<BsonClassMap<_>>.MakeGenericType(actualType)
            let classMap = Activator.CreateInstance(genericType) :?> BsonClassMap

            classMap.AutoMap()
            actualType.GetProperties() |> Seq.where (fun prop -> 
                classMap.AllMemberMaps |> Seq.exists (fun mm -> mm.MemberInfo = (prop :> MemberInfo)) |> not
            )
            |> Seq.where (fun prop -> prop.GetGetMethod() <> null)
            |> Seq.iter (fun prop -> classMap.MapMember(prop :> MemberInfo) |> ignore )

            match getMember actualType "Id" "_id" with
            | Some memberInfo -> classMap.MapIdMember memberInfo |> ignore
            | None -> ()

            classMap.Freeze() |> Some
        else 
            None

    let ensureClassMapRegistered actualType =
        let fn = BsonClassMap.IsClassMapRegistered 
        match getClassMap fn actualType with
        | Some map -> 
            map |> BsonClassMap.RegisterClassMap
            Some map
        | None -> 
            None

    type RecordSerializer(classMap : BsonClassMap) =
        inherit MongoDB.Bson.Serialization.Serializers.BsonBaseSerializer()
        let classMapSerializer =
            let typ = typeof<BsonClassMap>.Assembly.GetType("MongoDB.Bson.Serialization.BsonClassMapSerializer")
            let ctor = typ.GetConstructor([ typeof<BsonClassMap> ] |> Seq.toArray)
            ctor.Invoke([ classMap ] |> Seq.cast<Object> |> Seq.toArray) :?> IBsonSerializer

        let idProvider =
            classMapSerializer :?> IBsonIdProvider

        override this.Serialize(writer : BsonWriter, nominalType : Type, value : Object, options : IBsonSerializationOptions) =
            classMapSerializer.Serialize(writer, nominalType, value, options)

        override this.Deserialize(reader : BsonReader, nominalType : Type, options : IBsonSerializationOptions) =
            classMapSerializer.Deserialize(reader, nominalType, options)

        interface IBsonIdProvider with
            member this.GetDocumentId(document : Object, id : Object byref, nominalType : Type byref, idGenerator : IIdGenerator byref) =
                idProvider.GetDocumentId(document, ref id, ref nominalType, ref idGenerator)

            member this.SetDocumentId(document : Object, id : Object) =
                idProvider.SetDocumentId(document, id)


    type FsharpSerializationProvider() =
        let fsharpType (typ : Type) =
            typ.GetCustomAttributes(typeof<CompilationMappingAttribute>, true) 
                |> Seq.cast<CompilationMappingAttribute>
                |> Seq.map(fun t -> t.SourceConstructFlags)
                |> Seq.tryHead

        interface IBsonSerializationProvider with
            member this.GetSerializer(typ : Type) =
                match fsharpType typ with
                | Some SourceConstructFlags.RecordType ->
                    match ensureClassMapRegistered typ with
                    | Some classMap ->
                        //RecordSerializer(classMap) :> IBsonSerializer
                        null
                    // return null means to try the next provider to see if it has a better answer
                    | None -> null

                // other F# types, when we're ready (list, seq, discriminated union)
                | Some SourceConstructFlags.SumType ->
                    // Maybe it's a list?
                    if typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<List<_>> then
                         typedefof<ListSerializer<_>>.MakeGenericType(typ.GetGenericArguments())
                         |> Activator.CreateInstance :?> IBsonSerializer
                    else
                        null
                | _ -> null

    let mutable isRegistered = false

    /// Registers all F# serializers
    let Register() =
        if not isRegistered then
            BsonSerializer.RegisterSerializationProvider(FsharpSerializationProvider())
            BsonSerializer.RegisterGenericSerializerDefinition(typeof<list<_>>, typeof<ListSerializer<_>>)
            isRegistered <- true