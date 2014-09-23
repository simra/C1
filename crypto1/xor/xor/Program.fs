// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System;
open System.IO;
open System.Globalization;
open System.Text;

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}
let xor c1 c2 =
    let h1=Int32.Parse(c1,NumberStyles.HexNumber)
    let h2=Int32.Parse(c2,NumberStyles.HexNumber)
    let result=h1^^^h2
    String.Format("{0:X2}",result)

let xorLines (l1:string) (l2:string) : string =
    let result=new StringBuilder()
    for i in seq { 0 .. 2 .. Math.Min(l1.Length,l2.Length)-2 } do
        result.Append(xor l1.[i..(i+1)] l2.[i..(i+1)]) |> ignore
    result.ToString()

let loadFreq lines =
    Seq.map (fun l -> l.Split({' ','%'}))
    |> (l.[0],Double.Parse(l.[1].Remove('%'))
    |> Map.ofSeq



[<EntryPoint>]
let main argv = 
    let lines = readLines argv.[0]
    let xored = Seq.map (fun l1 -> Seq.map (fun l2 -> xorLines l1 l2) lines) lines
    
    xored
    |> Seq.iter (Seq.iter (printfn "%s"))
    
    let freq=
        readLines "letterfreq.txt"
        |> loadFreq
        |> generateDist  
    
       
    0 // return an integer exit code
