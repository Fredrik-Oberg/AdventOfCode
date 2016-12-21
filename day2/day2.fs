open System.Collections.Generic

let bathroomSecurity(input:System.Collections.Generic.IEnumerable<string>) =
    let directions = input  |> Seq.map (fun line -> line.ToCharArray() |> Array.toList) |> Seq.toList
    let move (direction:char, currentVal:int) =
        match direction with
        | 'U' -> 
            match currentVal with
                | 3 | 13 -> currentVal - 2 
                | x when x > 9 && x < 13 || x < 9 && x > 5 -> currentVal - 4
                | _ -> currentVal
        | 'D' -> 
          match currentVal with
                | 1 | 11 -> currentVal + 2 
                | x when x > 1 &&  x < 5 || x < 9 && x > 5 -> currentVal + 4
                | _ -> currentVal
        | 'L'
            -> match currentVal with
                | x when x > 2 && x < 5 || x > 5 && x < 10 || x > 10 && x < 13 -> currentVal - 1            
                | _ -> currentVal
        | 'R'
            -> match currentVal with             
                | x when x > 1 && x < 4 || x > 4 && x < 9 || x > 9 && x < 12 -> currentVal + 1            
                | _ -> currentVal


    let rec getCode listOfDirections (resultList:List<int>) =
        match listOfDirections with
        | [] ->
            let last = resultList |> Seq.last
            printfn "last %A" last
            
            last
        | hd::tl ->

            let lastVal = resultList |> Seq.last
            let newVal = move(hd, lastVal)
            resultList.Add(newVal)
            let m = resultList |> Seq.toList
            getCode tl resultList

    let initList = new List<int>()
    initList.Add(5)

    let result = directions |> Seq.map(fun dir-> getCode dir initList)

    result

let input = System.IO.File.ReadLines @"\dev\adventofcode\day2\input.txt"
let result = bathroomSecurity input

let transformToLetter res =
    match res with
    | 10 -> "A"
    | 11 -> "B"
    | 12 -> "C"
    | 13 -> "D"
    | _ -> res.ToString()

result |> Seq.map (fun x -> transformToLetter x) 
       |> Seq.toList 
       |> Seq.reduce (fun a b -> a + b)

