 // Part 1: Define the Coach, Stats, and Team types
type Coach = { 
    Name: string 
    FormerPlayer: bool 
}

type Stats = { 
    Wins: int 
    Losses: int 
}

type Team = { 
    Name: string 
    Coach: Coach 
    Stats: Stats 
}


let coaches = [
    { Name = "Asok"; FormerPlayer = true }
    { Name = "Khagendra"; FormerPlayer = true }
    { Name = "Ankit"; FormerPlayer = false }
    { Name = "Sudeep"; FormerPlayer = false }
    { Name = "Billy Donovan"; FormerPlayer = false }
]

let stats = [
    { Wins = 7; Losses = 11 }
    { Wins = 15; Losses = 3 }
    { Wins = 10; Losses = 8 }
    { Wins = 6; Losses = 11 }
    { Wins = 8; Losses = 11 }
]


let teams = [
    { Name = "Atlanta Hawks"; Coach = coaches.[0]; Stats = stats.[0] }
    { Name = "Boston Celtics"; Coach = coaches.[1]; Stats = stats.[1] }
    { Name = "Dallas Mavericks"; Coach = coaches.[2]; Stats = stats.[2] }
    { Name = "Charlotte Hornets"; Coach = coaches.[3]; Stats = stats.[3] }
    { Name = "Chicago Bulls"; Coach = coaches.[4]; Stats = stats.[4] }
]


let successfulTeams = 
    teams 
    |> List.filter (fun team -> team.Stats.Wins > team.Stats.Losses)


printfn "=== Original Teams ==="
teams |> List.iter (fun team ->
    printfn "Team: %-20s | Coach: %-20s | Wins: %-2d | Losses: %-2d" 
        team.Name team.Coach.Name team.Stats.Wins team.Stats.Losses
)


printfn "\n=== Successful Teams ==="
successfulTeams |> List.iter (fun team ->
    printfn "Team: %-20s | Coach: %-20s | Wins: %-2d | Losses: %-2d" 
        team.Name team.Coach.Name team.Stats.Wins team.Stats.Losses
)


let successPercentages = 
    successfulTeams
    |> List.map (fun team -> 
        let wins = float team.Stats.Wins
        let losses = float team.Stats.Losses
        let percentage = (wins / (wins + losses)) * 100.0
        (team.Name, percentage)
    )

printfn "\n=== Success Percentages ==="
successPercentages |> List.iter (fun (name, percentage) ->
    printfn "%-20s : %.2f%%" name percentage
)



// Part 2: Valentine's Day Budget


type Cuisine =
    | Korean
    | Turkish


type MovieType =
    | Regular
    | IMAX
    | DBOX
    | RegularWithSnacks
    | IMAXWithSnacks
    | DBOXWithSnacks


type Activity =
    | BoardGame
    | Chill
    | Movie of MovieType
    | Restaurant of Cuisine
    | LongDrive of int * float 


let calculateBudget activity =
    match activity with
    | BoardGame -> 0.0
    | Chill -> 0.0
    | Movie genre ->
        match genre with
        | Regular -> 12.0
        | IMAX -> 17.0
        | DBOX -> 20.0
        | RegularWithSnacks -> 12.0 + 5.0
        | IMAXWithSnacks -> 17.0 + 5.0
        | DBOXWithSnacks -> 20.0 + 5.0
    | Restaurant cuisine ->
        match cuisine with
        | Korean -> 70.0
        | Turkish -> 65.0
    | LongDrive (distance, costPerKm) -> float distance * costPerKm


let activities = [
    BoardGame
    Chill
    Movie RegularWithSnacks
    Movie IMAX
    Restaurant Korean
    Restaurant Turkish
    LongDrive (120, 0.25) 
]


printfn "=== Part 2: Valentine's Day Budget ===\n"


let describeActivity activity =
    match activity with
    | BoardGame -> "Playing a board game"
    | Chill -> "Relaxing at home"
    | Movie genre ->
        match genre with
        | Regular -> "Watching a Regular movie"
        | IMAX -> "Watching an IMAX movie"
        | DBOX -> "Watching a DBOX movie"
        | RegularWithSnacks -> "Watching a Regular movie with snacks"
        | IMAXWithSnacks -> "Watching an IMAX movie with snacks"
        | DBOXWithSnacks -> "Watching a DBOX movie with snacks"
    | Restaurant cuisine ->
        match cuisine with
        | Korean -> "Dining at a Korean restaurant"
        | Turkish -> "Dining at a Turkish restaurant"
    | LongDrive (distance, costPerKm) ->
        $"Taking a long drive for {distance} km at {costPerKm:F2} CAD/km"


activities
|> List.iter (fun activity -> 
    let description = describeActivity activity
    let cost = calculateBudget activity
    printfn "%-40s -> Budget: %.2f CAD" description cost
)
