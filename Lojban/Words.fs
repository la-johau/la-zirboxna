namespace Lojban
  module Words = begin

    module Rules = begin
      // helper module for the majority of lexing.
      
      let alpha = "',.abcdefgijklmnoprstuvxyz"
      let alpha_type = "' .VCCCVCCVCCCCCVCCCCVCCvC"
      // used for pattern matching.  Sort of type-loose but it works and matchs the style sort of like CLL
      let alpha_class = Seq.zip alpha alpha_type |> Map.ofSeq
      let vowels = String.filter (fun x -> let y =Map.find x alpha_class in y = 'v' || y ='V') alpha
      let consonants = String.filter (fun x -> let y =Map.find x alpha_class in y = 'C') alpha

      let classify_char (c : char) = match Map.tryFind c alpha_class with Some x -> x | None -> '?'

      let classify_str (x : string) = 
        let x = x.Trim().ToLowerInvariant()
        x 
        |> Seq.map classify_char
        |> Array.ofSeq
        |> System.String

      // double consonant rules

      let unvoiced = "ptkfcsx"
      let voiced   = "bdgvjz"
      let voiceable = "lmnr"
      let unvoiced_s = unvoiced |> Set.ofSeq
      let voiced_s  =  voiced |> Set.ofSeq
      let voiceable_s = voiceable |> Set.ofSeq
      let voiceify = function | x when Set.contains x unvoiced_s -> 0 | x when Set.contains x voiced_s -> 1 | x when Set.contains x voiceable_s -> 2 | _ -> -1

      // pair rules

      let illegal_pairs = 
        [
          for a in consonants do (string a) + (string a)
          for a in consonants do for b in consonants do if (match (voiceify a, voiceify b) with (0,1) -> true | (1,0) -> true | _ -> false) then (string a) + (string b)
          for a in "cjsz" do for b in "cjsz" do (string a) + (string b)
          "cx";"kx";"xc";"mz"
        ] |> List.distinct
      let illegal_pairs_s = illegal_pairs |> Set.ofList

      let initial_pairs =
        [
          "bl br"
          "cf ck cl cm cn cp cr ct"
          "dj dr dz"
          "fl fr"
          "gl gr"
          "jb jd jg jm jv"
          "kl kr"
          "ml mr"
          "pl pr"
          "sf sk sl sm sn sp sr st"
          "tc tr ts"
          "vl vr"
          "xl xr"
          "zb zd zg zm zv"
        ] |> Seq.map (fun x -> x.Split(' ')) |> Seq.concat |> List.ofSeq

      let initial_pairs_s = initial_pairs |> Set.ofList

      /// Is x a permissable pair?
      let permissable_pair (x :string) = 
        if x.Length = 2 then
          not (Set.contains x illegal_pairs_s)
        else failwith "not a pair"

      /// Is x a permissable initial pair
      let permissable_init_pair (x :string) = 
        if x.Length = 2 then
          Set.contains x initial_pairs_s
        else failwith "not a pair"

      /// Is x a permissiable medial triple?
      let permissable_medial_triple (x :string) = 
        if x.Length = 3 then
          let a = x.Substring(0,2)
          let b = x.Substring(1,2)
          (permissable_pair a) && (permissable_init_pair b)
        else failwith "not a triple"

      /// String match on prefix
      let prefix_match (test : string) (item : string) = 
        if item.Length >= test.Length then
          test = item.Substring(0,test.Length)
        else false
      /// splits a string into prefix and tail
      let prefix_split (len : int) (item : string) =
        (item.Substring(0,len), item.Substring(len))

      /// Finds the first cmavo prefix of the string
      let cmavo_prefix (x : string) =
        let vowelscan (x : string) = 
          let mutable idx = 0
          let mutable go  = idx < x.Length
          let mutable error = false
          while go do
            if x[idx] = 'V' then
              idx <- idx + 1
              go  <- idx < x.Length
            elif x[idx] = ''' then 
              if (idx + 1 < x.Length) && x[idx+1] = 'V' then
                idx <- idx + 2
                go  <- idx < x.Length
              else
                go  <- false
                error <- true
            else go <- false
          if error then None else Some idx

        let y = (classify_str x)
        if   prefix_match ".v." y then prefix_split 3 x |>  Some
        elif prefix_match ".v'v." y then prefix_split 5 x |>  Some
        elif  prefix_match "Cv." y then prefix_split 3 x |>  Some
        elif  prefix_match "Cv" y then 
          match y.Substring(2).ToUpper() |> vowelscan with
          | Some i -> prefix_split (i + 2) x |> Some
          | None -> None
        elif (prefix_match ".V" y) || (prefix_match "CV" y)  then 
          match y.Substring(2).ToUpper() |> vowelscan with
          | Some i -> prefix_split (i + 2) x |> Some
          | None -> None
        
        elif y.Length = 2 && prefix_match "Cv" y then prefix_split 2 x |>  Some
        elif y.Length = 3 && prefix_match "v'v" y then prefix_split 3 x |>  Some
        elif y.Length = 1 && prefix_match "v" y then prefix_split 1 x |>  Some
        elif y.Length = 3 && prefix_match "V'V" y then prefix_split 3 x |>  Some
        elif y.Length = 1 && prefix_match "V" y then prefix_split 1 x |>  Some
        else None

      /// returns a list of cmavo or None if string contains a non-cmavo
      let cmavo_decompose (x : string) =
        let l = System.Collections.Generic.List<_>()
        let rec tst = 
          function
          | "" when x.Length > 0 -> true
          | "." -> true 
          | y -> 
            match cmavo_prefix y with
            | Some (a,b) -> (l.Add(a); tst b)
            | None -> false
        if tst x then Some l else None

      let isCmevla (x: string) =
        let c = classify_str x       
        (c.EndsWith("C") || c.EndsWith("C.") ) && (c.Contains("?") |> not) && (c.Trim('.').Contains('.') |> not) 

      let isBrivla (x: string) =
        let c = classify_str x 
        let c' = c |> String.filter (fun x -> x = 'C' || x = 'V' || x = '?' || x='.')
        if c'.Length >= 5 then
          if c'.Substring(0,5).Contains("CC") && c'.EndsWith("V") && (c'.Contains("?") |> not) then
            true
          else false
        else false

      let isFu'ivla x =
        let c = classify_str x 
        let c' = c |> String.filter (fun x -> x = 'C' || x = 'V' || x = '?') // || x='.')
        if c'.Length >= 5 then
          if c'.Substring(0,5).Contains("CC") && c'.EndsWith("V") && (c'.Contains("?") |> not) && (c.Contains("v") |> not) then
            true
          else false
        else
          if c'.Contains("CC") && c'.EndsWith("V") && (c'.Contains("?") |> not) && (c.Contains("v") |> not) then
            true
          else false

      let fu'ivla_prefix x =
        if isFu'ivla x then 
          Some (x, "")
        elif isBrivla x && x.Contains("y") then
          let len = x.IndexOf('y')
          if x.Substring(0,len) |> isFu'ivla then 
            prefix_split (len+1) x |> Some
          else
            None
        else None

      let cmevla_prefix x =
        if isCmevla x then 
          Some (x, "")
        elif isBrivla x && x.Contains("y") then
          let len = x.IndexOf('y')
          if x.Substring(0,len) |> isCmevla then 
            prefix_split (len+1) x |> Some
          else
            None
        else None

      // CCVCV ; CCVC ;: CVC (123) CVC (124) CV'V (12'5) CVV (125) CCV (345) CCV (132)
      // CVCCV ; CVCC ;: CVC (134) CVC (234) CV'V (13'5) CVV (135)  CV'V (23'5) CVV (235) CCV (123)
      // CVC CV'V- CVV- CCV
      // - y, n, r [l for fu'ivla]
      /// identifies the first rafsi_prefix or returns None
      /// rafsi prefix may have a dangling hypen
      let rafsi_prefix (x : string) = // This probably needs work.
        let rec check x inCheck =
          // checks if the string decomposes into a sequence rafsi
          // Used for lookahead.
          let rec check_path (x : string) = 
            match check x true with
            | None -> false
            | Some (_,"") -> true
            | Some (_,x)  -> check_path x

          //terminal refsi
          if x.Length <= 5 then
            match classify_str x with
            | "CVC" -> None
            | "CV'V" -> Some (x,"")
            | "CVV" -> Some(x,"")
            | "CCV" -> Some(x,"")
            | "CCVC" -> Some(x,"")
            | "CVCC" -> Some(x,"")
            | "CCVCV" -> Some(x,"")
            | "CVCCV" -> Some(x,"")
            | _ when isFu'ivla x -> Some(x,"")
            | _ -> None
          else // nonterminal rafsi
            let inline test () =
              let a = x.Substring(0,5)
              match classify_str a with  // had to sort for priority order in lookahead
              | "CCV'v" when x.Length > 6 ->
                let a = x.Substring(0,6)
                match classify_str a with | "CCV'v'" -> Some(a,x.Substring(6)) | _ -> None

              | "CCVCV"  when check_path(x.Substring(3)) -> Some(a.Substring(0,3),x.Substring(3))
              | "CVCCV"  when check_path(x.Substring(3)) -> Some(a.Substring(0,3),x.Substring(3))
              | "CCVCC"  when check_path(x.Substring(3)) -> Some(a.Substring(0,3),x.Substring(3))
              | "CVCCC"  when check_path(x.Substring(3)) -> Some(a.Substring(0,3),x.Substring(3))
          
              | "CVVCC" when (a[3] = 'r' || a[3] = 'n') && check_path (x.Substring(4)) ->  Some(x.Substring(0,4),x.Substring(4))   
              | "CV'VC" when (a[4] = 'r' || a[4] = 'n') && check_path (x.Substring(5)) ->  Some(a,x.Substring(5)) 

              | "CVCCV" when check_path(x.Substring(5)) -> Some(a,x.Substring(5))

              | "CCVCC" -> Some(x.Substring(0,4),x.Substring(4))
              | "CCVCV" -> Some(a,x.Substring(5))
              | "CCVCv" -> Some(a,x.Substring(5))
              | "CVCCv" -> Some(a,x.Substring(5))
          
              | "CVCCC" -> Some(x.Substring(0,4),x.Substring(4))
              | "CVCvC" -> Some(x.Substring(0,4),x.Substring(4))
              | "CVVCC" -> Some(x.Substring(0,3),x.Substring(3))
              | "CVVCV" -> Some(x.Substring(0,3),x.Substring(3))
              | "CV'VC" -> Some(x.Substring(0,4),x.Substring(4))
              | _ -> 
                match fu'ivla_prefix x with
                | Some (x,y) when check_path y || y="" -> Some (x,y)
                | _ -> None
            
            
            if not inCheck then
              match cmevla_prefix x with
                | Some (x,y) when check_path y -> Some (x,y)
                | _ -> test()
            else test()

        check x false
      /// Decomposes string into a List of rafsi or returns None if string contains non-rafsi
      let rafsi_decompose (x : string) =
        let l = System.Collections.Generic.List<_>()
        let rec check_path (x : string) =
          match rafsi_prefix x with
          | None -> false
          | Some (a,"") -> (l.Add(a); true)
          | Some (a,x)  -> (l.Add(a);check_path x)
        if check_path x then Some l else None 

      // https://lojban.github.io/cll/4/7/ for fu'ivla
    end

    open Rules

    // Active patterns to parse lojban word types from strings
    // Currently this does not support composite words like a cmavo attached to a gismu.
    // Rules based on the CLL chapter 4
    
    let (|Cmavo|_|) (x : string) = 
      match cmavo_decompose x with
      | Some _ -> Some x
      | None ->
        match cmavo_decompose (x.TrimEnd('.')) with
        | Some _ -> Some (x.TrimEnd('.'))
        | None -> None


    let (|Gismu|_|) (x : string) = 
      let x = x.Trim().ToLowerInvariant()
      if x.Length = 5 then
        match classify_str x with
        | "CCVCV" when x.Substring(0,2) |> permissable_init_pair -> Some x
        | "CVCCV" when x.Substring(2,2) |> permissable_pair -> Some x
        | _ -> None
      else None

    let (|Lujvo|_|) (x : string) = 
      match rafsi_decompose x with
      | Some _ -> Some x
      | None -> 
        match rafsi_decompose (x.TrimEnd('.')) with
        | Some _ -> Some (x.TrimEnd('.'))
        | None -> None

    let (|Fu'ivla|_|) (x : string) = 
      if isFu'ivla x then Some x else None

    let (|Brivla|_|) x = if isBrivla x then Some x else None

    let (|Cmene|_|) (x: string) =
      let c = classify_str x       
      if (c.EndsWith("C") || c.EndsWith("C.") ) && (c.Contains("?") |> not) then Some x else None

    let (|Cmevla|_|) x = if isCmevla x then Some x else None
      
    let (|Nalojbau|_|) =
      function
      | Cmene _ -> None
      | Brivla _ -> None
      | Cmavo _ -> None
      | x -> Some x
  end

