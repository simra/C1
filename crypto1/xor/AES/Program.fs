// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.


open System
open System.IO
open System.Globalization
open System.Security.Cryptography
open System.Text

type HexStream = string
type ByteStream = byte[]
type CharStream = string

let hexStreamToByteStream (hexStr:HexStream) : ByteStream =
    seq { for i in { 0 .. 2 .. hexStr.Length-2 } do yield (byte (Int32.Parse(hexStr.[i..i+1],NumberStyles.HexNumber))) }
     |> Array.ofSeq

let byteStreamToHexStream (byteStr:ByteStream) : HexStream =
    seq { for i in {0..byteStr.Length-1} do yield (sprintf "%02x" byteStr.[i]) }
    |> (fun s -> String.Join("",s))

let charStreamToByteStream (charStr:CharStream) : ByteStream =
    Encoding.ASCII.GetBytes(charStr);

let byteStreamToCharStream (byteStr:ByteStream): CharStream =   
    Encoding.ASCII.GetString(byteStr)    
    


let EncryptStringToBytes_Aes (plainText:string) (key:byte[]) (IV:byte[]) : byte[] =
    if plainText = null || plainText.Length <= 0 then
        raise (new ArgumentNullException("plainText"))
    if (key = null || key.Length <= 0) then
        raise (new ArgumentNullException("Key"))
    if (IV = null || IV.Length <= 0) then
        raise (new ArgumentNullException("IV"))
    // Create an Aes object 
    // with the specified key and IV. 
    use aesAlg = Aes.Create()
    aesAlg.Key <- key;
    aesAlg.IV <- IV;
    aesAlg.Padding<-PaddingMode.PKCS7
    aesAlg.Mode<-CipherMode.CBC
    let blockSize=aesAlg.BlockSize/8

    let encryptor = aesAlg.CreateEncryptor(aesAlg.Key, aesAlg.IV)
    use msEncrypt = new MemoryStream()
    use csEncrypt = new CryptoStream(msEncrypt, encryptor, CryptoStreamMode.Write)
    
    let byteStr=charStreamToByteStream plainText
    csEncrypt.Write(byteStr,0,byteStr.Length)

    let p0 = blockSize-(plainText.Length%blockSize)
    let padding =
        if p0=0 then blockSize
        else p0
    
    csEncrypt.FlushFinalBlock()
    msEncrypt.ToArray()
    (*use swEncrypt = new StreamWriter(csEncrypt)
    
    //Write all data to the stream.
    swEncrypt.Write(plainText)
    
   // 
    swEncrypt.Flush()
    // Return the encrypted bytes from the memory stream. 
    msEncrypt.ToArray(); *)


let DecryptStringFromBytes_Aes (cipherText:byte[]) (key:byte[]) (IV:byte[]) : string =
    // Check arguments. 
    if (cipherText = null || cipherText.Length <= 0) then
        raise (new ArgumentNullException("cipherText"))
    if (key = null || key.Length <= 0) then
        raise (new ArgumentNullException("Key"))
    if (IV = null || IV.Length <= 0) then
        raise (new ArgumentNullException("IV"))

    // Create an Aes object 
    // with the specified key and IV. 
    use aesAlg = Aes.Create()
    aesAlg.Key <- key;
    aesAlg.IV <- IV;
    aesAlg.Padding<-PaddingMode.PKCS7
    aesAlg.Mode<-CipherMode.CBC
    // Create a decrytor to perform the stream transform.
    let decryptor = aesAlg.CreateDecryptor(aesAlg.Key, aesAlg.IV)
    // Create the streams used for decryption. 
    use msDecrypt = new MemoryStream(cipherText)
    use csDecrypt = new CryptoStream(msDecrypt, decryptor, CryptoStreamMode.Read)
    use srDecrypt = new StreamReader(csDecrypt)
    // Read the decrypted bytes from the decrypting stream   
    srDecrypt.ReadToEnd();

let ctExtractIV (ct:string) =
    (hexStreamToByteStream (ct.Substring(0,32)), hexStreamToByteStream (ct.Substring(32)))
   
   
let AESEncryptBlock (key:byte[]) (input:byte[]) : byte[] =
    use rDel = new RijndaelManaged();
    let (zero: byte array) = Array.zeroCreate 16
    let Trans = rDel.CreateEncryptor(key, zero)
    let (output: byte array) = Array.zeroCreate input.Length
    Trans.TransformBlock(input, 0, input.Length, output, 0) |> ignore
    output

let AESDecryptBlock (key:byte[]) (input:byte[]) : byte[] =
    let rDel = new RijndaelManaged()
    rDel.Padding <- PaddingMode.None
    let (zero: byte array) = Array.zeroCreate 16
    let Trans = rDel.CreateDecryptor(key,zero)
    let (output: byte array) = Array.zeroCreate input.Length
    Trans.TransformBlock(input, 0, input.Length, output, 0) |> ignore
    output

let xor (c1:byte[]) (c2:byte[]) : byte[] =
    let len=Math.Min(c1.Length,c2.Length)
    seq { for i in 0..len-1 do yield c1.[i]^^^c2.[i]} 
    |> Array.ofSeq

let incrementIV (input:byte[]) : byte[] =
    let mutable currPos =input.Length-1
    input.[currPos]<-input.[currPos]+(byte 1)
    while input.[currPos]=(byte 0) do         
        currPos<-currPos-1
        if currPos>=0 then
            input.[currPos]<-input.[currPos]+(byte 1)
    input
   
let rec ctrDecrypt (IV:byte[]) (key:byte[]) (cipherText:byte[]) : byte[] =
    if cipherText.Length=0 then Array.empty
    else 
        if cipherText.Length<=16 then
            let fKIV=AESEncryptBlock key IV
            xor cipherText fKIV
        else
            let head = cipherText.[0..15]
            let tail = cipherText.[16..cipherText.Length-1] 
            Array.concat [(ctrDecrypt IV key head); (ctrDecrypt (incrementIV IV) key tail)]

[<EntryPoint>]
let main argv = 
    //let original = "Here is some data to encrypt!";

                // Create a new instance of the Aes 
                // class.  This generates a new key and initialization  
                // vector (IV). 
    //use myAes = Aes.Create()

    // Q1
   (* let key = hexStreamToByteStream "140b41b22a29beb4061bda66b6747e14"
    let cipherTextWithIV= "4ca00ff4c898d61e1edbf1800618fb2828a226d160dad07883d04e008a7897ee2e4b7465d5290d0c0e6c6822236e1daafb94ffe0c5da05d9476be028ad7c1d81"
    *)
    // Q2
    (* let key = hexStreamToByteStream "140b41b22a29beb4061bda66b6747e14"
    let cipherTextWithIV= "5b68629feb8606f9a6667670b75b38a5b4832d0f26e1ab7da33249de7d4afc48e713ac646ace36e872ad5fb8a512428a6e21364b0c374df45503473c5242a253"
    
    let (IV,cipherText) = ctExtractIV cipherTextWithIV
   
   // let encrypted = EncryptStringToBytes_Aes original myAes.Key  myAes.IV

                    // Decrypt the bytes to a string. 
  //  let roundtrip = DecryptStringFromBytes_Aes encrypted myAes.Key myAes.IV
    let result = DecryptStringFromBytes_Aes cipherText key IV
                    //Display the original data and the decrypted data.
    //printfn "Original:   %s" original
    printfn "Round Trip: %s" result *)


    // Q3
    let key = hexStreamToByteStream "36f18357be4dbd77f050515c73fcf9f2"
    let cipherTextWithIV= "69dda8455c7dd4254bf353b773304eec0ec7702330098ce7f7520d1cbbb20fc388d1b0adb5054dbd7370849dbf0b88d393f252e764f1f5f7ad97ef79d59ce29f5f51eeca32eabedd9afa9329"

    let (IV,cipherText) = ctExtractIV cipherTextWithIV
    let result = ctrDecrypt IV key cipherText
    
    // Q4     
    let key = hexStreamToByteStream "36f18357be4dbd77f050515c73fcf9f2"
    let cipherTextWithIV= "770b80259ec33beb2561358a9f2dc617e46218c0a53cbeca695ae45faa8952aa0e311bde9d4e01726d3184c34451"

    let (IV,cipherText) = ctExtractIV cipherTextWithIV
    let result = ctrDecrypt IV key cipherText
    
    
    printfn "%s" (byteStreamToCharStream result)
                    
    0 // return an integer exit code
