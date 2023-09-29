type day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

let next_weekday d =
    match d with
    | Monday -> Tuesday
    | Tuesday -> Wednesday
    | Wednesday -> Thursday
    | Thursday -> Friday
    | Friday -> Saturday
    | Saturday -> Sunday
    | Sunday -> Monday
