// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System
open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let computeNgrams i (l:string) : Map<char,int> =
    seq { for k in 0 .. l.Length do yield l.[k]}
    |> Seq.groupBy (fun x -> x)
    |> Seq.map (fun (k,v) -> (k,Seq.length v))
    |> Map.ofSeq

let addKVP m1 kvp : Map<char,int> =
    let (k,v)=kvp
    if (Map.containsKey k m1) then
        let v0=m1.[k]
        Map.add k (v+v0) (Map.remove k m1)
    else
        Map.add k v m1

let mergeMaps (m1:Map<char,int>) (m2:Map<char,int>) : Map<char,int> =
    Seq.fold addKVP m1 (Map.toSeq m2)

[<EntryPoint>]
let main argv = 
    Seq.map readLines argv
    |> Seq.concat
    |> Seq.map (computeNgrams 1)
    |> Seq.fold mergeMaps Map.empty
    |> Map.toSeq
    |> Seq.iter (fun (k,v) -> printfn "%c:%d" k v)

    printfn "%A" argv
    0 // return an integer exit code
