// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System
open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let computeNgrams i (l:string) : Map<string,int> =
    seq { for k in 0 .. l.Length-i do yield l.[k..k+i-1]}
    |> Seq.groupBy (fun x -> x)
    |> Seq.map (fun (k,v) -> (k,Seq.length v))
    |> Map.ofSeq

let addKVP m1 kvp : Map<string,int> =
    let (k,v)=kvp
    if (Map.containsKey k m1) then
        let v0=m1.[k]
        Map.add k (v+v0) (Map.remove k m1)
    else
        Map.add k v m1

let mergeMaps (m1:Map<string,int>) (m2:Map<string,int>) : Map<string,int> =
    Seq.fold addKVP m1 (Map.toSeq m2)

[<EntryPoint>]
let main argv = 
    let freq=
        argv
        |> Seq.map (fun s -> readLines s |> Seq.takeWhile (fun (l:string) -> (not (l.StartsWith("*** END")))))
        |> Seq.concat
        |> Seq.map (computeNgrams 2)
        |> Seq.fold mergeMaps Map.empty
        |> Map.toSeq
        
    let tot=Seq.sumBy (fun (k,v) -> v) freq |> float
    freq
    |> Seq.iter (fun (k,v) -> printfn "%s:%.6f" k (Math.Log((float v)/tot)))
    //printfn "%A" argv
    0 // return an integer exit code
