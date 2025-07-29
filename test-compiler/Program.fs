open System
open System.IO

open Testsuite
open Thoth.Json.Net

let json = File.ReadAllText("../engine-test-output.json")

let result = json |> Decode.Auto.fromString<TestSuite>

let array_to_map (arr: array<String>): Option<Map<String, String>> =
    match arr.Length with 
    | _ when arr.Length <> 0 && arr.Length % 2 = 0  -> let count_of_params = arr.Length / 2 in
                                                       let valipala = arr |> Array.splitInto count_of_params in
                                                       Some (valipala |> Seq.fold (fun acc e -> acc.Add (e[0], e[1])) Map[])
    | _ -> None
      
    

(* match result with
  | Ok obj -> printfn "Result: %s" <| obj.ToString ()
  | Error err -> printfn "Decoding failed %s" <| err.ToString ()*)

let testaan = (array_to_map <| (Environment.GetCommandLineArgs() |> Array.skip 1))

printfn "argv %A " <| testaan

