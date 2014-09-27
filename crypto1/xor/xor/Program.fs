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

(*

let toByteStreamStr (s:string) :int [] =
    seq { for i in { 0..s.Length-1} do yield (int s.[i])}
    |> Array.ofSeq

let xor (c1:int[]) (c2:int[]) = 
    seq { for i in {0..c1.Length-1} do yield c1.[i]^^^c2.[i]}
    |> Array.ofSeq

let fromByteStream (m:int[]) : string =
    seq { for i in {0..m.Length-1} do yield (sprintf "%02x" m.[i]) }
    |> (fun s -> String.Join("",s))


let c1="09e1c5f70a65ac519458e7e53f36" |> toByteStream
let m1="attack at dawn" |> toByteStreamStr
let k=xor c1 m1
let m2="attack at dusk" |> toByteStreamStr
let c2=xor k m2
let c2str=c2 |> fromByteStream
let sane=[c1;m1;k;m2;c2] |> Seq.map Seq.length
*)

let fromByteStreamStr (m:int[]) : string =
    seq { for i in {0..m.Length-1} do yield (char m.[i]) }
    |> (fun s -> String.Join("",s))


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
    Seq.map (fun (l:string) -> (int l.[0],Double.Parse(l.Substring((l.LastIndexOf(':'))+1)))) lines
    |> Map.ofSeq

let loadBigramFreq lines : Map<int*int,float> =
    Seq.map (fun (l:string) -> (((int l.[0]),(int l.[1])),Double.Parse(l.Substring((l.LastIndexOf(':'))+1)))) lines
    |> Map.ofSeq

let logBigramFreq (freq:Map<int*int,float>) c =
    if freq.ContainsKey(c) then 
        if (freq.[c]<0.0) then 
            Math.Pow(-freq.[c],2.0) 
        else 1e6
    else 1e6

let logFreq (freq:Map<int,float>) c =
    if freq.ContainsKey(c) then 
        if (freq.[c]<0.0) then 
           // Math.Pow(-Math.Log(freq.[c]),2.0) 
            Math.Pow(-freq.[c],2.0) 
        else 1e6
    else 1e6

let cost (freq:Map<int,float>) (assignment:seq<int>) : float =
    let result=
        Seq.map(fun c-> if c=0 then 1e6 else logFreq freq c) assignment
        |> Seq.sum
   // printfn "%A\t%A" result assignment
    result

let bigramCost (freq:Map<int*int,float>) (assignment:int*int*int) : float =
    let (a,b,c)=assignment

    let c1=if(a=0 || b=0) then 1e6 else if a>0 then logBigramFreq freq (a,b) else 0.0
    let c2=if(b=0 || c=0) then 1e6 else if c>0 then logBigramFreq freq (b,c) else 0.0
    c1+c2

let bigramAssignmentCost (freq:Map<int*int,float>) (assignment:seq<int*int*int>) : float =    
    Seq.map (bigramCost freq) assignment |> Seq.sum
        

let getAssignment i c0 (line0:int[]) (line1:int[])  : int =
    if i<line0.Length && i<line1.Length then
        line0.[i]^^^line1.[i]^^^c0
    else 
        0

let getAssignments (ciphertext:list<int[]>) i c0 : list<int> =
    match ciphertext with
    | head :: tail -> c0 :: (List.map (getAssignment i c0 head) tail)
    | _ -> []

let makeTuple (i:int) (j:int) (currState:list<int[]>) (assignment:list<int>)  : int*int*int =
    if i=0 then
        (-1,assignment.[j],currState.[j].[i+1])
    else if i=currState.[j].Length-1 then
        (currState.[j].[i-1],assignment.[j],-1)
    else
        (currState.[j].[i-1],assignment.[j],currState.[j].[i+1])

let makeTuples (i:int) (currState:list<int[]>) (assignment:list<int>) :seq<int*int*int> =     
    seq { for j in {0.. (List.length currState)-1} do yield (makeTuple i j currState assignment) }
        

let optimizeBigramAssignment (freq:Map<int*int,float>) (ciphertext:list<int[]>) (currState:list<int[]>) (i:int) : list<int[]> = //seq<int*float*list<int>> =
    let chars=
        Map.toSeq freq
        |> Seq.map (fun (k,v) -> (fst k))
        |> Seq.sort
        |> Seq.groupBy (fun k -> k)
        |> Seq.map (fun (k,v) -> k)
    let result = 
        Seq.map (fun c -> getAssignments ciphertext i c |> fun a -> (i,bigramAssignmentCost freq (makeTuples i currState a),a)) chars
        |> Seq.sortBy (fun (i,c,a)->c)
        |> Seq.nth 0
    let (_,_,assignment)=result
    for j in {0..(List.length currState)-1} do
        currState.[j].[i]<-assignment.[j]
    currState

let optimizeAssignment (freq:Map<int,float>) (ciphertext:list<int[]>) (i:int) : seq<int*float*list<int>> =
    let chars=
        Map.toSeq freq
        |> Seq.map (fun (k,v) -> k)
    let result = 
        Seq.map (fun c -> getAssignments ciphertext i c |> fun a -> (i,cost freq a,a)) chars
        |> Seq.sortBy (fun (i,c,a)->c)
    result
  
let optimizeAssignments (freq:Map<int,float>) (ciphertext:list<int[]>) : seq<seq<int*float*list<int>>> =
    seq { for i in 0 .. (Seq.last ciphertext).Length-1 do yield optimizeAssignment freq ciphertext i }

let printAssignment i j (vals:seq<int*float*list<int>>) =
    let a=Seq.nth j vals
    let (_,_,asst)=a
    let v = Seq.nth i asst
    printf "%c" (Convert.ToChar(v))

// todo: verify this is random
let rand = new Random()
let randPermuteArray a =
        let n = Array.length a
        let rec aux = function
            | 0 -> a
            | k ->
                let i = rand.Next(k+1)
                let tmp = a.[i]
                a.[i] <- a.[k]
                a.[k] <- tmp
                aux (k-1)
        aux (n-1)

let rec transpose = function
    | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []


let optimizeBigrams (freq:Map<int*int,float>) (ciphertext:list<int[]>) (currState:list<int[]>) : list<int[]>  =
    for i in {1..20} do 
        let indices = seq { 0 .. (currState.[currState.Length-1].Length-1)} |> Array.ofSeq |> randPermuteArray
        Seq.fold (optimizeBigramAssignment freq ciphertext) currState indices |> ignore
    currState

[<EntryPoint>]
let main argv = 
    let ciphertext = 
        readLines argv.[0]
        |> Seq.map toByteStream
        |> List.ofSeq

    let unigramFreq=
        readLines "charLogFreq.txt"
        |> loadFreq
    
    let bigramFreq=
        readLines "bigramLogFreq.txt"
        |> loadBigramFreq

    // chose a good starting point
    let currState=
        optimizeAssignments unigramFreq ciphertext
        |> Seq.map (Seq.nth 0)        
        |> Seq.map (fun (i,c,l) -> l) 
        |> List.ofSeq
        |> transpose
        |> List.map (fun l -> Array.ofList l)    

    currState|>Seq.map fromByteStreamStr|> Seq.iter (printfn "%A")

    optimizeBigrams bigramFreq ciphertext currState
    |> Seq.map fromByteStreamStr
    |> Seq.iter (printfn "%A")

    (*
    for i in {0..10} do 
        result
        |> Seq.iter (printAssignment i 0)
        printfn "\n"
  
    
    result
    |> Seq.iter (printAssignment 10 1)
    printfn "\n"
  *)
   // result    
   // |> Seq.iter (printAssignment 1)
   
    0 // return an integer exit code
