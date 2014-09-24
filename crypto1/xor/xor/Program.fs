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

let toByteStream (s:string) : int[] =
     seq { for i in { 0 .. 2 .. s.Length-2 } do yield Int32.Parse(s.[i..i+1],NumberStyles.HexNumber) }
     |> Array.ofSeq




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

let loadFreq lines : Map<int,float> =
    Seq.map (fun (l:string) -> (int l.[0],Double.Parse(l.Substring(2))/100.0)) lines
    |> Map.ofSeq

let cost (freq:Map<int,float>) (assignment:seq<int>) : float =
    let result=
        Seq.map(fun c-> if c=0 then 1e6 else if freq.ContainsKey(c) then Math.Pow(-Math.Log(freq.[c]),2.0) else 1e6) assignment
        |> Seq.sum
   // printfn "%A\t%A" result assignment
    result


let getAssignment i c0 (line0:int[]) (line1:int[])  : int =
    if i<line0.Length && i<line1.Length then
        line0.[i]^^^line1.[i]^^^c0
    else 
        0

let getAssignments (lines:list<int[]>) i c0 : list<int> =
    match lines with
    | head :: tail -> c0 :: (List.map (getAssignment i c0 head) tail)
    | _ -> []


let optimizeAssignment (freq:Map<int,float>) (lines:list<int[]>) (i:int) : seq<int*float*list<int>> =
    let chars=
        Map.toSeq freq
        |> Seq.map (fun (k,v) -> k)
    let result = 
        Seq.map (fun c -> getAssignments lines i c |> fun a -> (i,cost freq a,a)) chars
        |> Seq.sortBy (fun (i,c,a)->c)
    result
  
let optimizeAssignments (freq:Map<int,float>) (lines:list<int[]>) : seq<seq<int*float*list<int>>> =
    seq { for i in 0 .. (Seq.last lines).Length-1 do yield optimizeAssignment freq lines i }

let printAssignment i j (vals:seq<int*float*list<int>>) =
    let a=Seq.nth j vals
    let (_,_,asst)=a
    let v = Seq.nth i asst
    printf "%c" (Convert.ToChar(v))


[<EntryPoint>]
let main argv = 
    let lines = 
        readLines argv.[0]
        |> Seq.map toByteStream
        |> List.ofSeq

    let freq=
        readLines "letterfreq.txt"
        |> loadFreq
    
    let result=optimizeAssignments freq lines
    for i in {0..10} do 
        result
        |> Seq.iter (printAssignment i 0)
        printfn "\n"
  
    for i in {0..10} do 
        result
        |> Seq.iter (printAssignment i 1)
        printfn "\n"
  
   // result    
   // |> Seq.iter (printAssignment 1)
   
    0 // return an integer exit code
