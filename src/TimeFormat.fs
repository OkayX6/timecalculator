module TimeCalc.TimeFormat

open System


type TimeFormat =
  | Minute of int
  | HourAndMinute of int * int
  | Hour of int
  | Empty

  override x.ToString() =
    match x with
    | Empty -> "0h00"
    | Minute mm -> sprintf "0h%02d" mm
    | Hour hh -> sprintf "%2dh" hh
    | HourAndMinute(hh, mm) -> sprintf "%2dh%02d" hh mm

  member x.ToTimeSpan() =
    match x with
    | Empty -> TimeSpan(0, 0, 0)
    | HourAndMinute(hh, mm) -> TimeSpan(hh, mm, 0)
    | Hour(hh) -> TimeSpan(hh, 0, 0)
    | Minute(mm) -> TimeSpan(0, mm, 0)


let (|ValidInt|_|) (str: string) =
    match Int32.TryParse str with
    | true, res -> Some res
    | false, _ -> None


let (|ValidHour|_|) (str: string) =
    match Int32.TryParse str with
    | true, res when 0 <= res && res < 24 -> Some res
    | _ -> None

let (|ValidMinute|_|) (str: string) =
    match Int32.TryParse str with
    | true, res when 0 <= res && res < 60 -> Some res
    | _ -> None


let isValidHour (str: string) =
    match Int32.TryParse str with
    | true, x when x >= 0 && x < 24 -> true
    | _ -> false

let isValidMinute (str: string) =
    match Int32.TryParse str with
    | true, x when x >= 0 && x < 60 -> true
    | _ -> false

let isSeparator x = x = 'h' || x = 'H'

let isHourPattern (str: string) =
    str.[0..1]
    |> isValidHour
    && str.[2] |> isSeparator
    || str.[0..0]
       |> isValidHour
       && str.[1] |> isSeparator
       && str.[2..] |> isValidMinute


let validateTime s =
    match s |> String.length, s with
    | 0, _ -> Some Empty
    | 1, ValidInt m
    | 2, ValidMinute m -> Some <| Minute m
    | 2, _ when isSeparator s.[1] ->
        match s.[0..0] with
        | ValidHour h -> Some <| Hour h
        | _ -> None
    | 3, _ ->
        match s.[0..1], s.[0..0], s.[2..2] with
        | ValidHour hh, _, _ when s.[2] |> isSeparator -> Some <| Hour hh
        | _, ValidHour h, ValidMinute m when s.[1] |> isSeparator -> Some <| HourAndMinute(h, m)
        | _ -> None
    | 4, _ when isSeparator s.[1] ->
        match s.[0..0], s.[2..] with
        | ValidHour h, ValidMinute mm -> Some <| HourAndMinute(h, mm)
        | _ -> None
    | 4, _
    | 5, _ when isSeparator s.[2] ->
        match s.[0..1], s.[3..] with
        | ValidHour hh, ValidMinute mm -> Some <| HourAndMinute(hh, mm)
        | _ -> None
    | _, _ -> None