open System
open System.IO

open Testsuite
open Thoth.Json.Net
open System.Xml.Serialization

let array_to_map (arr: array<String>): Option<Map<String, String>> =
    match arr.Length with 
    | _ when arr.Length <> 0 && arr.Length % 2 = 0  -> let count_of_params = arr.Length / 2 in
                                                       let valipala = arr |> Array.splitInto count_of_params in
                                                       Some (valipala |> Seq.fold (fun acc e -> acc.Add (e[0], e[1])) Map[])
    | _ -> None    

let compile_backend testsuite output_filename =
    let testsuites = {new TestSuites with suites = [| testsuite |]}
    let serializerNamespaces = XmlSerializerNamespaces()
    serializerNamespaces.Add("", "")

    let writer = new StringWriter()

    let serializer = XmlSerializer(typeof<TestSuites>)
    serializer.Serialize(writer, testsuites, serializerNamespaces)
    File.WriteAllLines(output_filename, [| writer.ToString() |])
    

      
    
let compile input_f output_f =
    printfn "Compiling test file %s to %s" input_f output_f
    let input_json = File.ReadAllText(input_f) in
    let testdata_result = Decode.Auto.fromString<TestSuite> input_json in
    match testdata_result with
        | Ok testdata -> compile_backend testdata output_f
        | Error err -> printfn "Json is invalid %A" err;
    

match array_to_map (Environment.GetCommandLineArgs() |> Array.skip 1) with
    | Some options ->
        match options.TryFind "--input", options.TryFind "--output" with
            | Some input_filename, Some output_filename -> compile input_filename output_filename
            | _ -> printfn "You need to specify both --input and --output"
    | None ->
        printfn "usage: compiler.exe --input input.json --output output.xml"
