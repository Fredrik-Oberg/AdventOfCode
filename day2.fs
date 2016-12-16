
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

    let rec getCode listOfDirections currentVal =
        match listOfDirections with
        | [] ->
            printfn "finalVal %A" currentVal
            currentVal.ToString()
        | hd::tl ->
            let newVal = move(hd, currentVal)

            getCode tl newVal

    let resultList = new System.Collections.Generic.List<int>()
    let result = directions |> Seq.map(fun dir-> getCode dir 5)

    result

let input = System.IO.File.ReadLines @"\dev\adventofcode\day2input.txt"

let result = bathroomSecurity input

result |> Seq.toList |> String.concat ""