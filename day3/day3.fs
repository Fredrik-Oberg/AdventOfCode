open System.Collections.Generic
open System
let squaresWithThreeSides(input:IEnumerable<string>) =
    let isTriangle(a:int, b:int, c:int) =
        if a + b > c && a + c > b && b + c > a  then true
        else false

    let parseToInt(x:string[]) =
        [Int32.Parse(x.[0]); Int32.Parse(x.[1]); Int32.Parse(x.[2])]

    let res = input
            |> Seq.map (fun x -> x.Trim())
            |> Seq.map (fun x ->  x.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries))
            |> Seq.map (fun x -> parseToInt x)
            |> Seq.map (fun x -> isTriangle(x.[0], x.[1],x.[2]))

    res

let path = __SOURCE_DIRECTORY__ + "/input.txt"
let input = System.IO.File.ReadLines path

let result = squaresWithThreeSides input

result |> Seq.filter (fun x -> x = true) |> Seq.length