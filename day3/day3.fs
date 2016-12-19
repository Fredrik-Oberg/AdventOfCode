open System.Collections.Generic
open System
let squaresWithThreeSides(input:IEnumerable<string>) =
//    let isTriangle values =
//        match values with
//        | (a)
//        a + b < c

    let parseToInt(x:string[]) =
        [Int32.Parse(x.[0]); Int32.Parse(x.[1]); Int32.Parse(x.[2])]

    let res = input
            |> Seq.map (fun x -> x.Trim() )
            |> Seq.map (fun x -> x.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries))
            |> Seq.map (fun x -> parseToInt x)
//            |> Seq.map (fun x -> isTriangle())

    res

let path = __SOURCE_DIRECTORY__ + "/input.txt"
let input = System.IO.File.ReadLines path

let result = squaresWithThreeSides input
//let HowManySatisfy pred = Seq.filter pred >> Seq.length
//result |> HowManySatisfy (fun x -> x = true)

result |> Seq.iter (fun x -> printfn "%A" x )