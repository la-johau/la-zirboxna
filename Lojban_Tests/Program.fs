// For more information see https://aka.ms/fsharp-console-apps

open Lojban

/// colors a word by it's lexer type.
let color_word (a : string) =
  let width = (System.Console.GetCursorPosition()|> fun struct(w,_h)->w)
  if a.Length + width >= System.Console.WindowWidth && a.Length < System.Console.WindowWidth then
    System.Console.WriteLine()
  let fg = System.Console.ForegroundColor
  try
    match a with
    | Words.Cmavo a -> 
      System.Console.ForegroundColor <- System.ConsoleColor.Cyan
      let l = match Words.Rules.cmavo_decompose a with Some l -> l | None -> failwith "boom."
      for i in l do
        if System.Console.ForegroundColor = System.ConsoleColor.Cyan then
          System.Console.ForegroundColor <- System.ConsoleColor.DarkCyan
        else
          System.Console.ForegroundColor <- System.ConsoleColor.Cyan
        System.Console.Write(i)
      System.Console.Write(" ")
    | Words.Gismu a ->
      System.Console.ForegroundColor <- System.ConsoleColor.Green
      System.Console.Write(a)
      System.Console.Write(" ")
    | Words.Lujvo a ->
      System.Console.ForegroundColor <- System.ConsoleColor.Gray
      let l = match Words.Rules.rafsi_decompose a with Some l -> l | None -> failwith "boom."
      for i in l do
        if System.Console.ForegroundColor = System.ConsoleColor.Gray then
          System.Console.ForegroundColor <- System.ConsoleColor.DarkGray
        else
          System.Console.ForegroundColor <- System.ConsoleColor.Gray
        System.Console.Write(i)
      System.Console.Write(" ")
    | Words.Fu'ivla a ->
      System.Console.ForegroundColor <- System.ConsoleColor.Magenta
      System.Console.Write(a)
      System.Console.Write(" ")
    | Words.Brivla a -> 
      System.Console.ForegroundColor <- System.ConsoleColor.DarkYellow
      System.Console.Write(a)
      System.Console.Write(" ")
    | Words.Cmene a -> 
      System.Console.ForegroundColor <- System.ConsoleColor.Blue
      System.Console.Write(a)
      System.Console.Write(" ")
    | a ->
      System.Console.ForegroundColor <- System.ConsoleColor.Red
      System.Console.Write(a)
      System.Console.Write(" ")
  finally
    System.Console.ForegroundColor <- fg

/// splits words out of string and tests lexer on each word
let color_words (x : string) (debug : bool) = 
  if debug then System.Console.WriteLine("Saw: " + x)
  let fg = System.Console.ForegroundColor
  for a in x.Split(' ',System.StringSplitOptions.RemoveEmptyEntries) do
    color_word a
    
  System.Console.ForegroundColor <- fg
  System.Console.WriteLine()

/// loads a file and tests the lexer
let color_file (filename : string) = 
  let lines = System.IO.File.ReadAllLines(filename)
  for l in lines do color_words l false

/// Tests the rafsi prefix lexer
let rec deconstruct (x : string) =
  match Words.Rules.rafsi_prefix x with
  | Some (a,b) -> 
    printfn "%s : %s" a b
    deconstruct b
  | None -> ()

/// Loads json dictionary as a unit test of lexer
let check_dictionary (filename : string) (error : bool) =
  
  let inline tryGetStr (element : string) (from : System.Text.Json.JsonElement) =
    let s, v = from.TryGetProperty(element)
    if s && v.ValueKind = System.Text.Json.JsonValueKind.String then v.GetString() |> Some
    else None

  let inline tryGetRec (element : string) (from : System.Text.Json.JsonElement) =
    let s, v = from.TryGetProperty(element)
    if s && v.ValueKind = System.Text.Json.JsonValueKind.Object then Some v
    else None

  let inline tryGetArr (element : string) (from : System.Text.Json.JsonElement) =
    let s, v = from.TryGetProperty(element)
    if s && v.ValueKind = System.Text.Json.JsonValueKind.Array then Some v
    else None

  let dictfile = System.IO.File.ReadAllText(filename)
  let dexie = System.Text.Json.JsonDocument.Parse(dictfile)
  let rows = // assuming the format from la sutysisku
    match tryGetRec "data" dexie.RootElement with
    | Some d -> 
      match tryGetArr "data" d with
      | Some a -> 
        match tryGetArr "rows" a[0] with
        | Some r -> r
        | _ -> failwith "no root.data.data[0].rows field"
      | _ -> failwith "no root.data.data field"
    | _ -> failwith "no root.data field"

      
  let fg = System.Console.ForegroundColor
  let inline color_word w t = 
    color_word w
    printfn ":%s" t
  try
    for r in rows.EnumerateArray() do
      match (tryGetStr "w" r, tryGetStr "t" r, tryGetStr "d" r) with
      | (Some w', Some t, Some d) ->
        let w = if (Words.Rules.classify_char w'[0]) = 'V' then "." + w' else w'
        match t with 
        | "bu-letteral" -> if not error then color_word w (t+" "+d)
        | "zei-lujvo" when w.Contains(' ') |> not -> if not error then color_word w (t+" "+d)
        
        | "cmavo" -> match w with Words.Cmavo _ -> () |_-> color_word w (t+" "+d)
        
        | "cmavo-compound" when w.Contains(' ') -> match w |> String.filter (fun x -> x <> ' ') with Words.Cmavo _ -> () |_-> color_word w (t+" "+d)
        | "cmavo-compound" -> match w with Words.Cmavo _ -> () |_-> color_word w (t+" "+d)
        | "experimental cmavo" -> match w with Words.Cmavo _ -> () |_-> color_word w (t+" "+d)
        | "obsolete cmavo" -> match w with Words.Cmavo _ -> () |_-> color_word w (t+" "+d)

        | "cmevla" -> match w with Words.Cmene _ -> () |_-> color_word w (t+" "+d)
        | "obsolete cmevla" -> match w with Words.Cmene _ -> () |_-> color_word w (t+" "+d)

        | "fu'ivla" -> match w with Words.Fu'ivla _ -> () |_-> color_word w (t+" "+d)
        | "obsolete fu'ivla" -> match w with Words.Fu'ivla _ -> () |_-> color_word w (t+" "+d)

        | "gismu" -> match w with Words.Gismu _ -> () |_-> color_word w (t+" "+d)
        | "experimental gismu" -> match w with Words.Gismu _ -> () |_-> color_word w (t+" "+d)
        | "lujvo" -> match w with Words.Lujvo _ -> () |_-> color_word w (t+" "+d)

        | _ when w.Contains(' ') |> not -> if not error then color_word w (t+" "+d)
        | _ -> ()
      | _ -> ()
      
  finally
    System.Console.ForegroundColor <- fg


[<EntryPoint>]
let main = 
  function // need to improve format of command line parsing
  | x when x.Length > 1 && x[0].ToLower() = "color" ->
    color_words (x[1..] |> String.concat " ") true
    0
  | x when x.Length = 2 && x[0].ToLower() = "colorfile" && System.IO.File.Exists(x[1]) ->
    color_file x[1]
    0
  | x when x.Length = 2 && x[0].ToLower() = "deconstruct" ->
    deconstruct x[1]
    0

  | x when x.Length = 2 && x[0].ToLower() = "checkdict" && System.IO.File.Exists(x[1]) ->
    check_dictionary x[1] true
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
    1


