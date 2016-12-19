open System.Collections.Generic
let bathroomSecurity(input:System.Collections.Generic.IEnumerable<string>) =

    let directions = input  |> Seq.map (fun line -> line.ToCharArray() |> Array.toList) |> Seq.toList

    let move (direction:char, currentVal:int) =
        printfn "currentVal %A" currentVal
        printfn "direction %A" direction
        match direction with
        | 'U' -> if currentVal < 3 then currentVal else currentVal - 3
        | 'D' -> if currentVal > 6 then currentVal else currentVal + 3
        | 'L'
            -> match currentVal with
                | 1 | 4 | 7 -> currentVal
                | _ -> currentVal - 1
        | 'R'
            -> match currentVal with
                | 3 | 6 | 9 -> currentVal
                | _ -> currentVal + 1
        | _ -> currentVal


    let rec getCode listOfDirections resultList:List<int> =
        match listOfDirections with
        | [] ->
            printfn "finalVal %A" resultList
            resultList
        | hd::tl ->

            let lastVal = resultList |> Seq.last
//            printfn "lastVal %A" lastVal
            let newVal = move(hd, lastVal)
            printfn "newVal %A" newVal
            resultList.Add(newVal)
            let m = resultList |> Seq.toList
            getCode tl resultList

    let initList = new List<int>()
    initList.Add(5)

    let result = directions |> Seq.map(fun dir-> getCode dir initList)

    result

let input = System.IO.File.ReadLines @"\dev\adventofcode\day2\input.txt"

let result = bathroomSecurity input

result |> Seq.toList