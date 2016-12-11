type Grid = {x:int; y:int; visitedTwiceLocation: int}

let noTimeForTaxicab input = 

    let commaSeparated (x:string) =
        x.Split([|','|])

    let firstChar (x:string) =
        x.Trim().Chars(0)

    let skipFirstChar (x:string) =
        x.Trim().Substring(1)

    let switchDirection (turn:char,pos:int) =
        let move = 
            match turn with  
                | 'R' -> 1
                | 'L' -> -1
        let newPos = 
            match pos + move with
            | 0 -> 4 //N -> W
            | 1 -> 1 //N
            | 2 -> 2 //E
            | 3 -> 3 //S
            | 4 -> 4 //W
            | 5 -> 1 //W -> N
        newPos

    let newGrid (grid:Grid, x:int, y:int) =
        {x = x; y = y; visitedTwiceLocation = grid.visitedTwiceLocation}

    let updateGrid (newPosition:int, grid:Grid, steps:int) =
        match newPosition with
            | 1 -> newGrid(grid, grid.x + steps, grid.y)
            | 2 -> newGrid(grid, grid.x, grid.y + steps)
            | 3 -> newGrid(grid, grid.x - steps, grid.y)
            | 4 -> newGrid(grid, grid.x, grid.y - steps)

    let calcTotalSteps (grid:Grid) =
        System.Math.Abs(grid.x) + System.Math.Abs(grid.y)
      
    let newVisistedList (grid:Grid, visitedSpots:System.Collections.Generic.List<Grid>) = 
        let hasVisited = 
            visitedSpots |> Seq.exists (fun x -> x = grid)

        if hasVisited 
            then visitedSpots.Add({x = grid.x; y = grid.y; visitedTwiceLocation = calcTotalSteps(grid)})
            else visitedSpots.Add(grid)
        visitedSpots

    let rec moveToNewPosition listOfInputs position grid visitedSpots =
        match listOfInputs with
        | [] ->
            printfn "visitedSpots %A " visitedSpots
            let firstVisitedTwoTimes = 
                visitedSpots |> Seq.find (fun x -> x.visitedTwiceLocation > 1)
            printfn "firstVisitedTwoTimes %A " firstVisitedTwoTimes

            calcTotalSteps(grid)

        | hd::tl ->
            let turn = firstChar(hd)
            let newPosition = switchDirection(turn, position)
            let steps = System.Convert.ToInt32(skipFirstChar(hd))
            let newGrid = updateGrid(newPosition, grid, steps)
            let newList = newVisistedList(grid, visitedSpots)
            
            moveToNewPosition tl newPosition newGrid newList

    let inputList = commaSeparated(input)
                    |> Array.toList

    let savedGrids = new System.Collections.Generic.List<Grid>()

    let result = moveToNewPosition inputList 1 {x = 0; y = 0; visitedTwiceLocation = 0} savedGrids

    result

let input = "R1, R1, R3, R1, R1, L2, R5, L2, R5, R1, R4, L2, R3, L3, R4, L5, R4, R4, R1, L5, L4, R5, R3, L1, R4, R3, L2, L1, R3, L4, R3, L2, R5, R190, R3, R5, L5, L1, R54, L3, L4, L1, R4, R1, R3, L1, L1, R2, L2, R2, R5, L3, R4, R76, L3, R4, R191, R5, R5, L5, L4, L5, L3, R1, R3, R2, L2, L2, L4, L5, L4, R5, R4, R4, R2, R3, R4, L3, L2, R5, R3, L2, L1, R2, L3, R2, L1, L1, R1, L3, R5, L5, L1, L2, R5, R3, L3, R3, R5, R2, R5, R5, L5, L5, R2, L3, L5, L2, L1, R2, R2, L2, R2, L3, L2, R3, L5, R4, L4, L5, R3, L4, R1, R3, R2, R4, L2, L3, R2, L5, R5, R4, L2, R4, L1, L3, L1, L3, R1, R2, R1, L5, R5, R3, L3, L3, L2, R4, R2, L5, L1, L1, L5, L4, L1, L1, R1"
let inputB = "R8, R4, R4, R8"

let result = noTimeForTaxicab input

printfn "Easter Bunny HQ is %A blocks away" result

