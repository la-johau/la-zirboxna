// For more information see https://aka.ms/fsharp-console-apps
open FSharp.Data

type Dictionary = XmlProvider<"./data/jbovlaste-en.xml">

module conversion = 
  begin
    open jbovlaste

    let convert_valsi (filename : string) = 
      seq{
          let data = Dictionary.Load(filename)

          for direction in data.Directions do
            let result = 
              seq{
                for v in direction.Valsis do
                  {
                    word = v.Word.String |> Option.defaultValue "" ;
                    definition = v.Definition ;
                    id = v.Definitionid ;
                    glosses = v.Glosswords |> Array.map (fun i -> {word=i.Word.String |> Option.defaultValue ""; sense = i.Sense |> Option.defaultValue ""});
                    keywords = v.Keywords |> Array.map (fun i -> {word=i.Word; sense = i.Sense |> Option.defaultValue ""; place = i.Place});
                    notes = v.Notes |> Option.defaultValue ""
                    rafsi = v.Rafsis |> Array.map (fun i -> i.Replace("<rafsi>","").Replace("</rafsi>",""))
                    selma'o = v.Selmaho |> Option.defaultValue ""
                    kind = valsi_kind.parse v.Type
                  }
              } |> Array.ofSeq

            if result.Length > 0 then 
              yield (direction.From, result)
      }

    let convert_lookup (filename : string) = 
      seq{
          let data = Dictionary.Load(filename)

          for direction in data.Directions do
            let result = 
              seq{
                for n in direction.Nlwords do
                  {
                    word = n.Word.String |> Option.defaultValue "";
                    valsi = n.Valsi.String |> Option.defaultValue "";
                    sense = n.Sense |> Option.defaultValue "";
                    place = n.Place
                  }
              } |> Array.ofSeq

            if result.Length > 0 then 
              yield (direction.To, result)
      }

  end

let test_print (filename : string) = 
  let data = Dictionary.Load(filename)

  for direction in data.Directions do
    printfn "%s ---> %s :" direction.From direction.To
    for v in direction.Valsis[1..2009] do
      let word = v.Word.String |> Option.defaultValue ""
      let definition = v.Definition
      let defid = v.Definitionid
      let glosses = 
        v.Glosswords 
        |> Array.map 
          (fun i -> 
            {|word = {|s = i.Word.String |> Option.defaultValue ""; b = i.Word.Boolean |> Option.defaultValue false; score = i.Word.Number |> Option.defaultValue 0.|}; 
              sense = i.Sense |> Option.defaultValue ""
            |} 
          )
      let keywords = 
        v.Keywords
        |> Array.map
          (fun i -> {|word = i.Word; sense = i.Sense |> Option.defaultValue ""; place = i.Place|} )
      let notes = v.Notes |> Option.defaultValue ""
      let rafsis = v.Rafsis
      let selmaho = v.Selmaho |> Option.defaultValue ""
      let wordtype = v.Type
      let unofficial = v.Unofficial |> Option.defaultValue false
      let author = v.User |> (fun i -> {|user = i.Username; name = i.Realname|})
      printfn "word: %A ; glosses: %i ; keywords: %i ; rafsi: %A; type: %s" word glosses.Length keywords.Length rafsis wordtype
      
    for n in direction.Nlwords[0..9] do
      let v = 
        {| 
          word = n.Word.String |> Option.defaultValue "";
          valsi = n.Valsi.String |> Option.defaultValue "";
          sense = n.Sense |> Option.defaultValue "";
          place = n.Place
        |}

      printfn "%A " v


let gloss (filename) (words : #seq<string>) =
  let t = 
      conversion.convert_valsi filename 
      |> Seq.collect snd
      |> Seq.map (fun i -> System.Collections.Generic.KeyValuePair<_,_>(i.word,i))
  let table = System.Collections.Generic.Dictionary<_,_>(t,HashIdentity.Structural)
      
  seq{
    for w in words do
      (
        let s,v = table.TryGetValue(w.Trim('.'))
        if s then
          if v.glosses.Length > 0 then (w,v.glosses[0].word)
          elif v.selma'o <> "" then (w,v.selma'o)
          else (w,"????")
        else (w,"???")
      )
  }

let glossout length (words : #seq<string * string>) =
  let text = System.Text.StringBuilder()
  let glossing = System.Text.StringBuilder()
  seq{
    for (a,b) in words do
      let len = System.Math.Max(a.Length,b.Length) + 2
      if text.Length + len >= length then
        yield (text.ToString(),glossing.ToString())
        ignore (text.Clear())
        ignore (glossing.Clear())
      ignore (text.Append(a.PadRight(len,' ')))
      ignore (glossing.Append(b.PadRight(len,' ')))
    if text.Length > 0 then
      yield (text.ToString(),glossing.ToString())
  }


[<EntryPoint>]
let main = 
  function
  | [|a;b|] when a.ToLower() = "print" -> 
    test_print b
    0
  | [|a;b;c|] when a.ToLower() = "convert" && b.ToLower() = "valsi" -> 
    let mutable cnt = 0
    for (i,j) in conversion.convert_valsi c do
      eprintfn "Language : %s" i
      for w in j do
        printfn "%A" w
        cnt <- cnt + 1
    eprintfn "%i items printed" cnt
    0
  | [|a;b;c|] when a.ToLower() = "convert" && b.ToLower() = "lookup" -> 
    let mutable cnt = 0
    for (i,j) in conversion.convert_lookup c do
      eprintfn "Language : %s" i
      for w in j do
        printfn "%A" w
        cnt <- cnt + 1
    eprintfn "%i items printed" cnt
    0
  | x when x.Length > 2 && x[0].ToLower() = "gloss" ->
    gloss x[1] x[2..] |> Seq.iter (printf "%A ")
    0
  | x when x.Length = 2 && x[0].ToLower() = "glosswith" ->
    let glosser = gloss x[1]
    let input = System.Console.In.ReadToEnd()
    for l in input.Split([|"\r\n"; "\r"; "\n"|],System.StringSplitOptions.RemoveEmptyEntries &&& System.StringSplitOptions.TrimEntries) do
      l.Split(" ",System.StringSplitOptions.RemoveEmptyEntries)
      |> Seq.map (fun i -> i.Trim().ToLower())
      |> glosser
      |> glossout System.Console.BufferWidth
      |> Seq.iter (fun (a,b) -> System.Console.WriteLine(a) ; System.Console.WriteLine(b); System.Console.WriteLine())
      
     
    0
  | x -> 
    eprintfn "Unrecognized : %A" x
    eprintfn "options:"
    eprintfn "  print <xml-file>"
    eprintfn "  convert valsi <xml-file>"
    eprintfn "  convert lookup <xml-file>"
    eprintfn "  gloss <xml-file> <words>*"
    eprintfn "  glosswith <xml-file> <words from stdin>"
    1