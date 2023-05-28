// For more information see https://aka.ms/fsharp-console-apps
module Commands = begin
  let(|Keyword|_|) (x : string[]) =
    match x with
    | [||] -> None
    | x  -> Some (x.[0].ToLowerInvariant(),x |> Seq.skip 1 |> List.ofSeq)
end

module Args = begin
  let(|FileName|_|) = 
    function
    | x :: y -> Some (System.IO.FileInfo(x),y)
    | _ -> None
  let(|FileExists|_|) = 
    function
    | x :: y when System.IO.File.Exists(x) -> Some (System.IO.FileInfo(x),y)
    | _ -> None
end

[<EntryPoint>]
let main = 
  function // need to improve format of command line parsing
  | Commands.Keyword ("color",x) ->
    Word_Tests.color_words (x |> String.concat " ") true
    0
  | Commands.Keyword ("colorfile", Args.FileExists (x,[])) ->
    Word_Tests.color_file x.FullName
    0
  | Commands.Keyword ("deconstruct",[x]) ->
    Word_Tests.deconstruct x
    0
  | Commands.Keyword ("checkdict", Args.FileExists (x,[])) ->
    Word_Tests.check_dictionary x.FullName true
    0

  | Commands.Keyword ("gloss", Args.FileExists (x,[])) ->
    glossing.glossconsole x 
    0

  | Commands.Keyword ("lookup", x :: []) ->
    glossing.lookup x 
    0

  | x -> 
    eprintfn "Saw: %A" x
    eprintfn "commands:"
    eprintfn "\texe color <list of words>"
    eprintfn "\t\tColorizes lojban words according to type"
    eprintfn "\texe colorfile <filename>"
    eprintfn "\t\tColorizes lojban words within a text file according to type"
    eprintfn "\texe deconstruct <word>"
    eprintfn "\t\trasfi deconstruction via prefix matching"
    eprintfn "\texe checkdict <json file from la sutysisku>"
    eprintfn "\t\tLoads json file in a format as defined by la sutysisku and tests the type checking of the dictionary entries.  Outputs words that are inconsistant with stated type."
    eprintfn "\texe gloss <filename>"
    eprintfn "\t\tLoads files and pretty prints it with undertext glossing."
    eprintfn "\texe lookup <word>"
    eprintfn "\t\tfinds definition of word"
    1


