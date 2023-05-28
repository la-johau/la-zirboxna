module glossing
open Lojban

let private table = jbovlaste.lojban.Lookup.table


let private simple (w :string) = 
  let s,v = table.TryGetValue(w.Trim('.'))
  if s then Some v
  else None


let private rafsi (w : string) = 
  let w' = w.Trim('.')
  w' 
  |> jbovlaste.rafsi.lookup 
  |> (
      function 
      Some v -> (w, Some v)
      | _ ->
        let s,v = table.TryGetValue(w')
        if s then
          (w, Some v)
        else
          (w, None)
  )

let lookup (w : string) =
  
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
      | Words.Cmene _ ->
        yield (x,None)
      | x -> 
        yield (x, None)
    }
  
  let w' = w.Trim('.')
  let s,v = table.TryGetValue(w')
  if s then
    printfn "%s" v.definition
  else
    for (w,v) in decompose w do
      printfn "%s : %s" w (match v with Some i -> i.definition | _ -> "???")

    //if subs.Length = 0 then (w,"???")
    //else
    //  (w, String.concat "-" subs)



let gloss () =

  let simple (w :string) = 
    let s,v = table.TryGetValue(w.Trim('.'))
    if s then
      let t = v.Gloss
      if t.Length > 0 then t else "!???!"
    else
      "???"
  
  let rafsi (w : string) = 
    let w' = w.Trim('.')
    w' 
    |> jbovlaste.rafsi.lookup 
    |> (
        function 
        Some v -> v.Gloss 
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
            let t = v.Gloss
            (w, if t.Length > 0 then t else "!???!")
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

let glossconsole (x : System.IO.FileInfo) =
  //eprintfn "Glossing : %s" x.FullName
  
  let lines = System.IO.File.ReadAllLines(x.FullName) //input.Split([|"\r\n"; "\r"; "\n"|],System.StringSplitOptions.RemoveEmptyEntries &&& System.StringSplitOptions.TrimEntries)
  //eprintfn "Lines in file : %i" lines.Length
  for l in lines do
    let w = l.Split(" ",System.StringSplitOptions.RemoveEmptyEntries)
    //eprintfn "Words on line: %i" w.Length
    w
    |> Seq.map (fun i -> i.Trim().ToLower())
    |> gloss ()
    |> glossout System.Console.BufferWidth
    |> Seq.iter (fun (a,b) -> System.Console.WriteLine(a) ; System.Console.WriteLine(b); System.Console.WriteLine())

