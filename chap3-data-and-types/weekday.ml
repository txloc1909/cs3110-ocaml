type day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

let next_weekday d =
    match d with
    | Monday -> Tuesday
    | _ -> failwith "Unimplemented"
