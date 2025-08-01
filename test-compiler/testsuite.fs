module Testsuite

open Thoth.Json.Net
open System.Xml.Serialization

[<CLIMutable>]
[<XmlRoot("testcase")>] 
type TestResult =
    { [<XmlAttribute("classname")>]
      classname : string

      [<XmlIgnore()>]
      element : string
      
      [<XmlAttribute("name")>]
      name : string
      
      [<XmlAttribute("time")>]
      time : double }

let test_result_json = "{
      \"classname\": \"feuertest807\",
      \"element\": \"testcase\",
      \"name\": \"sprite_count == 1\",
      \"time\": 0.98765
    }"
                                     


//     // Decoder specific to the user type
//     let decoder : Decoder<User> =
//         Decode.object (fun get ->
//             {
//                 Id = get.Required.Field "id" Decode.guid
//                 Name = get.Required.Field "name" Decode.string
//                 Age = get.Required.Field "age" Decode.int
//             }
//         )
// Decode.fromString
//     // Access the `data` property directly
//     // allow us direct access to the 'User' object
//     (Decode.field "data" User.decoder)
//     json


[<CLIMutable>]
[<XmlRoot("testsuite")>] 
type TestSuite =
    { [<XmlIgnore()>]
      element : string
      [<XmlAttribute("name")>]
      name : string
      [<XmlElement("testcase")>]
      results : TestResult[]}

[<CLIMutable>]
[<XmlRoot("testsuites")>] 
type TestSuites =
    { [<XmlElement("testsuite")>]
      suites : TestSuite[] }
