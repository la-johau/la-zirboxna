namespace jbovlaste

  // Word type
  type valsi = {
    word : string
    definition : string
    id : int
    glosses : gloss array
    keywords : keyword array
    notes : string
    rafsi : string array
    selma'o : string
    kind : valsi_kind
  }

  and gloss = {
    word : string
    sense : string
  }
  and keyword = {
    word : string
    sense : string
    place : int
  }
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
        elif x.Contains("Cmene") then Cmene
        elif x.Contains("Cmevla") then Cmevla
        else Other(x)

  type lookup = {
    word  : string
    valsi : string
    sense : string
    place : int option
  }