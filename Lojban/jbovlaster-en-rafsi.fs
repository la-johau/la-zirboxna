module jbovlaste.rafsi

open jbovlaste.lojban

let private rafsi_seq =
  seq{
    for i in Lookup.s do
      let v = i.Value
      for r in v.Rafsi do
        yield new System.Collections.Generic.KeyValuePair<_,_>(r,v)
  }
  
let table = new System.Collections.Generic.Dictionary<_,_>(rafsi_seq,HashIdentity.Structural)
let tablepick x =
  let s,v = table.TryGetValue(x)
  if s then Some v
  else None

let lookup (x : string) = 
  let e = System.Math.Min(5,x.Length)

  seq{for i in 2..(e-1) do yield x[0..i]}
  |> Seq.tryPick tablepick

  
  //result


