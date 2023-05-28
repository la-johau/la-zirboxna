namespace jbovlaste

  module private Helper = begin
    let inline escape (c : char) =
      if c = '\a' then @"\a"
      elif c = '\b' then @"\b"
      elif c = '\f' then @"\f"
      elif c = '\n' then @"\n"
      elif c = '\r' then @"\r"
      elif c = '\t' then @"\t"
      elif c = '\v' then @"\v"
      elif c = '\\' then @"\\"
      elif c = '"' then @"\"""
      elif c = ''' then @"'"
      elif c = '.' then @"." 
      elif System.Char.IsAsciiLetterOrDigit(c) || System.Char.IsWhiteSpace(c)  then string c
      elif System.Char.IsAscii(c) then sprintf "\\x%02x" (c |> int)
      else sprintf "\\u%04x" (c |> int)
    let inline quote (x:string) = "\"" + (x |> String.collect escape) + "\""
  end
  open Helper

  // Word type
  type valsi = {
    word : string // literal that is the word
    definition : string // semantics in another language
    id : int  // unique number reference
    glosses : gloss array  // acceptable gloss in another language
    keywords : keyword array // glosses for arguments of selbri
    notes : string 
    rafsi : string array  //rafsi that are not default 4 and 5 rafsi
    selma'o : string  // syntaxical/semantic functional group of word
    kind : valsi_kind // syntaxical classification of word
  }
    with
      member self.Rafsi
        with get() =
          seq{
            for i in self.rafsi |> Seq.sortBy (fun i -> i.Length) do 
              i
            match self.kind with
            | Gismu -> 
              //self.word[0..3]
              self.word
            | _ -> ()
          } |> Seq.distinct
      member self.Gloss 
        with get() =
          if self.glosses.Length > 0 then (self.glosses |> Seq.sortBy (fun i -> i.word.Length) |> Seq.head).word
          else self.selma'o
      member self.Word // Fsharp name word
        with get () = 
          let t = self.word.Replace(",","")
          if t = "do" || t = "to" || t = "in" then t + "'" else t
      member self.toFsharp 
        with get () =          
          sprintf "{word=%s; definition=%s; id=%i; glosses=[|%s|]; keywords=[|%s|]; notes=%s; rafsi=[|%s|]; selma'o=%s; kind=%s}"
            (quote self.Word)
            (quote self.definition)
            self.id
            (self.glosses |> Seq.map (fun i -> i.toFsharp ) |> String.concat "; ")
            (self.keywords |> Seq.map (fun i -> i.toFsharp ) |> String.concat "; ")
            (quote self.notes)
            (self.rafsi |> Seq.map quote |> String.concat "; ")
            (quote self.selma'o)
            self.kind.toFsharp

  and gloss = {
    word : string
    sense : string
  }
    with 
      member self.toFsharp
        with get () =
          sprintf "{word=%s; sense=%s}" (quote self.word) (quote self.sense)
      
  and keyword = {
    word : string
    sense : string
    place : int
  }
    with 
      member self.toFsharp
        with get () =
          sprintf "{word=%s; sense=%s; place=%i}" (quote self.word) (quote self.sense) self.place

  and valsi_kind =
    | Cmavo
    | Gismu
    | Lujvo
    | Fu'ivla
    | Brivla
    | Cmene
    | Cmevla
    | Other of string
    with 
      static member parse (x:string) =
        let x = x.ToLower()
        if x.Contains("cmavo") then Cmavo
        elif x.Contains("gismu") then Gismu
        elif x.Contains("lujvo") then Lujvo
        elif x.Contains("fu'ivla") then Fu'ivla
        elif x.Contains("brivla") then Brivla
        elif x.Contains("cmene") then Cmene
        elif x.Contains("cmevla") then Cmevla
        else Other(x)

      member self.toFsharp
        with get () =
           match self with
           | Cmavo -> "Cmavo"
           | Gismu -> "Gismu"
           | Lujvo -> "Lujvo"
           | Fu'ivla -> "Fu'ivla"
           | Brivla -> "Brivla"
           | Cmene -> "Cmene"
           | Cmevla -> "Cmevla"
           | Other x -> sprintf "Other (\"%s\")" x

      member self.Catagory
        with get() =
          match self with 
          | Other _ -> "Others"
          | x -> x.toFsharp


  type lookup = {
    word  : string
    valsi : string
    sense : string
    place : int option
  }
    with
      member self.toFsharp
        with get () =
           sprintf "{word=%s; valsi= %s; sense= %s; place=%s}"
            (quote self.word)
            (quote self.valsi)
            (quote self.sense)
            (match self.place with None -> "None" | Some x -> sprintf "Some (%i)" x)