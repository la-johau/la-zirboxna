module glossing
open Lojban

let private table = jbovlaste.lojban.Lookup.table


let private simple (w :string) = 
  let s,v = table.TryGetValue(w.Trim('.'))
  if s then Some v
  else None

let private toKind (w : string) =
  match simple w with
  | Some v -> v.kind
  | None ->
    match w with
    | Words.Cmavo _ -> jbovlaste.Cmavo
    | Words.Lujvo _ -> jbovlaste.Lujvo
    | Words.Fu'ivla _ -> jbovlaste.Fu'ivla
    | Words.Cmene _ -> jbovlaste.Cmene
    | Words.Cmevla _ -> jbovlaste.Cmevla
    | Words.Gismu _ -> jbovlaste.Gismu
    | _ -> jbovlaste.Other "Forign"

let private rafsi (w : string) = 
  let w' = w.Trim('.')
  w' 
  |> jbovlaste.rafsi.lookup 
  |> (
      function 
      | Some v when w.Length = 5 ->
        let s,v' = table.TryGetValue(w')
        if s then 
          (w, Some v')
        else (w, Some v)
      | Some v -> (w, Some v)
      | _ ->
        let s,v = table.TryGetValue(w')
        if s then
          (w, Some v)
        else
          (w, None)
  )

let decompose (x : string) =
    seq{
      match x with
      | Words.Cmavo _ ->
        match Words.Rules.cmavo_decompose x with 
        | Some x -> yield! (x |> Seq.map (fun i -> (i, simple i)))
        | None -> ()
      | Words.Lujvo _ ->
        match Words.Rules.rafsi_decompose x with
        | Some x -> yield! (x |> Seq.map rafsi)
        | None -> ()
      | Words.Cmene y ->
        yield (y, (jbovlaste.valsi.FromCmene y))
      | x -> 
        yield (x, None)
    }

let lookups (w :string) =
  let w' = w.Trim('.')
  let s,v = table.TryGetValue(w')
  if s then (w',Some v) |> Seq.singleton
  else
    decompose w

let lookup (w : string) =
  for (w,v) in lookups w do
    match v with
    | Some v -> printfn "%s:\n%A" w v
    | None -> printfn "%s : No defintion." w
  
  //let w' = w.Trim('.')
  //let s,v = table.TryGetValue(w')
  //if s then
  //  printfn "%A" (v.definition, v.Gloss)
  //else
  //  for (w,v) in decompose w do
  //    printfn "%s : %A" w (match v with Some i -> (i.definition, i.Gloss) | _ -> ("???", "???"))

  //  //if subs.Length = 0 then (w,"???")
  //  //else
  //  //  (w, String.concat "-" subs)



let gloss () =

  let simple (w :string) = 
    let s,v = table.TryGetValue(w.Trim('.'))
    if s then
      let t = v.Gloss
      if t.Length > 0 then t 
      else match v.kind with
           | jbovlaste.Cmevla -> sprintf "\"%s\"" w
           | jbovlaste.Cmene  -> sprintf "\"%s\"" w
           | v -> sprintf "%s{}" v.toFsharp
    else
      "???"
  
  let rafsi (w : string) = 
    let w' = w.Trim('.')
    w' 
    |> jbovlaste.rafsi.lookup 
    |> (
        function
        | Some v when w.Length = 5 ->
          let s,v' = table.TryGetValue(w')
          if s then 
            v'.Gloss
          else 
            v.Gloss
        | Some v -> v.Gloss 
        | _ ->
          let s,v = table.TryGetValue(w')
          if s then
            sprintf "<%s>" v.Gloss
          else
            sprintf "<%s>" w'
    )

  let decompose (x : string) =
    seq{
      match x with
      | Words.Cmavo _ ->
        match Words.Rules.cmavo_decompose x with 
        | Some x -> yield! (x |> Seq.map (fun i -> sprintf "%s[%s]" i (simple i)))
        | None -> ()
      | Words.Lujvo _ ->
        match Words.Rules.rafsi_decompose x with
        | Some x -> yield! (x |> Seq.map rafsi)
        | None -> ()
      | Words.Cmene _ ->
        "\"" + x + "\""
      | _ -> ()
    }
  
  fun (words : #seq<string>) ->
    seq{
      for w in words do
        (
          //eprintfn "Glossing : %s" w
          let w' = w.Trim('.')
          let s,v = table.TryGetValue(w')
          if s then
            (w, simple w')
          else
            let subs = decompose w |> Array.ofSeq
            if subs.Length = 0 then (w,"???")
            else
              (w, String.concat "_" subs)

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


let wordtrim (x : string) =
  try
    let x = x.ToLowerInvariant()
    let start =  x.IndexOfAny(Lojban.Words.Rules.alpha.ToCharArray())
    let stop  =  x.LastIndexOfAny(Lojban.Words.Rules.alpha.ToCharArray())
    x.Substring(start, System.Math.Max(1+stop-start,0)) |> Some
  with | _ -> None

let glossconsole (x : System.IO.FileInfo) =
  //eprintfn "Glossing : %s" x.FullName
  
  let lines = System.IO.File.ReadAllLines(x.FullName) //input.Split([|"\r\n"; "\r"; "\n"|],System.StringSplitOptions.RemoveEmptyEntries &&& System.StringSplitOptions.TrimEntries)
  //eprintfn "Lines in file : %i" lines.Length
  for l in lines do
    let w = l.Split(" ",System.StringSplitOptions.RemoveEmptyEntries)
    //eprintfn "Words on line: %i" w.Length
    w
    |> Seq.choose wordtrim
    |> gloss ()
    |> glossout System.Console.BufferWidth
    |> Seq.iter (fun (a,b) -> System.Console.WriteLine(a) ; System.Console.WriteLine(b); System.Console.WriteLine())

let wordcount (x : System.IO.FileInfo) =
  let count = 
    seq{ for l in System.IO.File.ReadAllLines(x.FullName) do yield! l.Split(" ",System.StringSplitOptions.RemoveEmptyEntries)}
    |> Seq.choose wordtrim
    |> gloss ()
    |> Seq.distinctBy fst
    |> Seq.length
  printfn "Unique Words: %i" count

let studyguide (x : System.IO.FileInfo) =
  let guide =
    seq{ for l in System.IO.File.ReadAllLines(x.FullName) do yield! l.Split(" ",System.StringSplitOptions.RemoveEmptyEntries)}
    |> Seq.choose wordtrim
    |> Seq.countBy id
    |> Seq.sortByDescending snd
    |> Seq.groupBy (fun i -> i |> fst |> toKind)
    |> Seq.filter (fun (i,_) -> match i with |jbovlaste.Other _ -> false | _ -> true)
    
  for kind, group in guide do
    printfn "%A :" kind
    for (word,count) in group |> Seq.sortBy fst do
      printfn "%s : %i" word count
      do lookup word




    


