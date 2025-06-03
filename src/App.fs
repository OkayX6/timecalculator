module App.View

open System
open Browser.Types
open Fable.Core
open Feliz
open TimeCalc
open TimeCalc.Input

/// Uses Fable's Emit to call JavaScript directly and play sounds
[<Emit("(new Audio($0)).play();")>]
let play (fileName: string) = jsNative

let playKeyPress () = play "assets/keypress.wav"



let inline private spanClass (classNames: string) (txt: string) =
  Html.span [
    prop.className classNames
    prop.children (Html.text txt)
  ]


/// Parses a string like "0h", "0h10", "12h05", "8h", "15", "00h00" to a TimeOnly object.
/// - "8" is parsed as 8 minutes (00:08)
/// - >= 60 values are not accepted
/// - negative values are not accepted
/// Throws exception if not parseable.
let parseTimeFormat (s: string) : TimeOnly =
  if String.IsNullOrWhiteSpace(s) then
    TimeOnly(0, 0)
  else
    let lower = s.ToLowerInvariant()
    let hIndex = lower.IndexOf('h')

    if hIndex >= 0 then
      let hourPart = lower.Substring(0, hIndex)

      let minPart =
        if hIndex + 1 < lower.Length then
          lower.Substring(hIndex + 1)
        else
          ""

      match System.Int32.TryParse(hourPart) with
      | (true, hh) when hh >= 0 && hh < 60 ->
        match minPart with
        | "" -> TimeOnly(hh, 0)
        | _ ->
          match System.Int32.TryParse(minPart) with
          | (true, mm) when mm >= 0 && mm < 60 -> TimeOnly(hh, mm)
          | (true, mm) -> failwithf "Minute value out of range: %d" mm
          | _ -> failwithf "Invalid minute part: '%s'" minPart
      | (true, hh) -> failwithf "Hour value out of range: %d" hh
      | _ -> failwithf "Invalid hour part: '%s'" hourPart
    else
      // No 'h' separator, try to parse as minutes
      match System.Int32.TryParse(lower) with
      | (true, mm) when mm >= 0 && mm < 60 -> TimeOnly(0, mm)
      | (true, mm) -> failwithf "Minute value out of range: %d" mm
      | _ -> failwithf "Invalid time string: '%s'" s


type DisplayResultData =
  {| left:string
     op: Op
     right: string
   |}


type LineState =
  | OnLeftOperand of string
  | OnRightOperand of string * Op * string
  | DisplayResult of string * Op * string * isFinalResult: bool
  static member InitialState = OnLeftOperand ""
  member x.Duration =
    match x with
    | DisplayResult(left, op, right, _) ->
        let ts1 = (parseTimeFormat left).ToTimeSpan()
        let ts2 = (parseTimeFormat right).ToTimeSpan()
        match op with
        | Op.To ->
          if ts1 < ts2 then
            ts2.Subtract(ts1)
          else
            ts1.Subtract(ts2)

        | Op.Plus -> ts1.Add(ts2)
        | Op.Sub -> ts1.Subtract(ts2)

    | _ -> failwith "unsupported op"


type MultilineState =
  { LastLine: LineState
    OtherLines: List<LineState> }
  static member InitialState = { LastLine = LineState.InitialState; OtherLines = [] }
  static member FromSingle line = { LastLine = line; OtherLines = [] }






[<ReactComponent>]
let private Operand (operand: string, withCursor: bool, highlight: bool) =
  let suffix = if withCursor then "_" else ""
  let className = if highlight then "text-accent" else ""

  let operand =
    if not highlight && not (operand.Contains("h")) then
      if operand.Length = 0 || operand = "0" then
        "0h"
      else
        let time = parseTimeFormat operand

        if time.Minute = 0 then
          "0h"
        else
          sprintf "0h%02d" time.Minute
    else
      let time = parseTimeFormat operand

      if not highlight then
        if time.Minute = 0 && operand.EndsWith("h") then
          sprintf "%dh" time.Hour
        else
          sprintf "%dh%02d" time.Hour time.Minute
      else
        operand

  spanClass $"{className}" ($"{operand}{suffix}")

// let operatorSizeClass = "text-3xl sm:text-4xl md:text-5xl lg:text-6xl"
// let operandSizeClass = "text-5xl sm:text-6xl md:text-7xl lg:text-8xl"
let mainWeight = "font-extralight"
let operatorSizeClass = $"text-4xl sm:text-5xl md:text-6xl lg:text-7xl {mainWeight}"
let operandSizeClass = "text-4xl sm:text-5xl md:text-6xl lg:text-7xl"

[<ReactComponent>]
let private Operator (op: Op) =
  match op with
  | Op.To -> " -> "
  | Op.Plus -> " + "
  | Op.Sub -> " - "
  |> spanClass $"font-operator {operatorSizeClass}"


[<ReactComponent>]
let private EqualSign () =
  spanClass $"font-operator {operatorSizeClass}" " = "

[<ReactComponent>]
let private OperationResult (ts: TimeSpan, highlight: bool) =
  let className = if highlight then "text-accent" else ""
  spanClass $"{className}" (sprintf "%dh%02d" ts.Hours ts.Minutes)



[<ReactComponent>]
let private SingleLineVisualizer (model: LineState, summedResult: TimeSpan option) =
  let oldOperand = "font-default font-bold"
  let newOperand = $"font-inter {mainWeight}"
  let operandFont = newOperand

  // Content
  // We want to display a time calculation model as a string
  Html.div [
    prop.className $"{operandSizeClass} {operandFont}"
    prop.children [
      match model with
      // Here we only have the left timestamp
      | OnLeftOperand opLeft -> yield Operand(opLeft, true, true)

      // Here we have the left timestamp, the operation and the right timestamp
      | OnRightOperand (left, op, right) ->
        yield Operand(left, false, false)
        yield Operator op
        yield Operand(right, true, true)

      // Here we have the whole calculation complete and calculable, plus the result
      // displayed as a time span
      | DisplayResult (left, op, right, isFinalResult) ->
        yield Operand(left, false, false)
        yield Operator op
        yield Operand(right, false, false)
        yield EqualSign()

        let ts1 = (parseTimeFormat left).ToTimeSpan()
        let ts2 = (parseTimeFormat right).ToTimeSpan()

        yield
          match op with
          | Op.To ->
            let diff =
              if ts1 < ts2 then
                ts2.Subtract(ts1)
              else
                ts1.Subtract(ts2)

            OperationResult(diff, isFinalResult)
          | Op.Plus -> OperationResult(ts1.Add(ts2), isFinalResult)
          | Op.Sub -> OperationResult(ts1.Subtract(ts2), isFinalResult)

        match summedResult with
        | None -> ()
        | Some duration ->
          let timeRepr = sprintf "%dh%02d" duration.Hours duration.Minutes
          yield spanClass $"ml-4 font-italic" $"(= {timeRepr})"
    ]
  ]


[<ReactComponent>]
let private MultilineVisualizer (model: MultilineState) =
  Html.div [
    prop.classes ["flex flex-col"]
    prop.children [
      let lines =
        [|
          yield model.LastLine
          yield! model.OtherLines
        |]
        |> Array.rev


      // Calculation
      Html.div [
        for line in lines do SingleLineVisualizer (line, None)
      ]

      let lastDisplayResultIdx =
        match model.LastLine, model.OtherLines with
        | DisplayResult(_), DisplayResult(_)::_ -> Some (lines.Length - 1)
        | _, [_] -> None
        | _, DisplayResult(_)::_ -> Some (lines.Length - 2)
        | _, _ -> None

      match lastDisplayResultIdx with
      | None -> ()
      | Some lastDisplayResultIdx ->
        let sumTimeSpans =
          lines |> Array.mapi (fun i line ->
            if i <= lastDisplayResultIdx then
              line.Duration
            else
              TimeSpan.Zero
          )
          |> Array.reduce (fun ts1 ts2 -> ts1.Add(ts2))

        Html.div [
          prop.classes [
            "w-full text-right border-t-1 mt-6 pt-3"
            "text-5xl font-inter font-extralight"
          ]
          match model.LastLine with
          | DisplayResult _ ->
            prop.text (sprintf "total: %dh%02d" sumTimeSpans.Hours sumTimeSpans.Minutes)
          | _ ->
            prop.text ("total: ...")
        ]
    ]
  ]


[<ReactComponent>]
let Layout (models: MultilineState []) =
  // Container
  Html.div [
    prop.className "min-h-screen bg-gray-50 text-primary flex flex-col items-center justify-center font-default"
    prop.children [
      Html.div [
        prop.className "flex flex-col items-start"
        prop.children [
          // Title: Time Calculator
          let oldTitle = "font-title font-bold"
          let newTitle = "font-inter font-extrabold"
          let title = newTitle

          Html.h2 [
            prop.className $"text-3xl {title} mb-8 lowercase pt-12 border-b-1 border-primary pb-4 w-full"
            prop.children [
              Html.text "Time Calculator"
            ]
          ]

          // Toutes les lignes de calcul
          Html.div [
            prop.className "pb-20 flex flex-col gap-3"
            prop.children [
              // Content
              for model in models do
                MultilineVisualizer model
            ]
          ]
        ]
      ]
    ]
  ]



let tentativeUpdateLine (model: LineState) (c: CharMsg) =
  match model with
  | OnLeftOperand (left) ->
    match c with
    | Escape -> OnLeftOperand("")
    | Digit d -> OnLeftOperand($"{left}{d}")
    | HourSep -> OnLeftOperand($"{left}h")
    | Backspace -> OnLeftOperand(left[0 .. left.Length - 2])
    | Tab
    | Enter -> OnRightOperand(left, Op.To, "")
    | Plus -> OnRightOperand(left, Op.Plus, "")
    | Minus -> OnRightOperand(left, Op.Sub, "")
    | _ -> model

  | OnRightOperand (left, op, right) ->
    match c with
    | Escape -> OnLeftOperand("")
    | Digit d -> OnRightOperand(left, op, $"{right}{d}")
    | HourSep -> OnRightOperand(left, op, $"{right}h")
    | Backspace ->
      if right.Length > 0 then
        OnRightOperand(left, op, right[0 .. right.Length - 2])
      else
        OnLeftOperand(left)
    | Tab
    | Enter -> DisplayResult(left, op, right, isFinalResult = true)
    | _ -> model

  | DisplayResult (left, op, right, false) -> DisplayResult(left, op, right, true)
  | DisplayResult (left, op, right, _) ->
    match c with
    | Escape -> OnLeftOperand("")
    | Backspace -> OnRightOperand(left, op, right)
    | _ -> model


let checkLineResult (model) =
  try
    match model with
    | OnLeftOperand (left) -> parseTimeFormat left |> ignore

    | OnRightOperand (left, op, right) ->
      parseTimeFormat left |> ignore
      parseTimeFormat right |> ignore

    | DisplayResult (left, op, right, isFinalResult) ->
      // if not isFinalResult then failwith "Should be final result"
      parseTimeFormat left |> ignore
      parseTimeFormat right |> ignore

    playKeyPress ()
    true
  with
  | _ -> false


let updateLineModel (model: LineState) (c: CharMsg) =
  let intermediateResult = tentativeUpdateLine model c

  if checkLineResult intermediateResult then
    intermediateResult
  else
    model

let tentativeUpdate (model: MultilineState) (c: CharMsg) =
  match model.LastLine, model.OtherLines with
  | DisplayResult (left, op, right, isFinalResult), _ ->
    match c with
    | Tab
    | Enter ->
      let updatedLastLine = DisplayResult(left, op, right, false)
      { LastLine = LineState.InitialState; OtherLines = updatedLastLine :: model.OtherLines }
    | _ -> { model with LastLine = tentativeUpdateLine model.LastLine c }

  // Transition from empty line to the previous line
  | OnLeftOperand (""), DisplayResult (l, op, r, _) :: rest ->
    match c with
    | Escape
    | Backspace -> { LastLine = DisplayResult(l, op, r, true); OtherLines = rest }
    | _ -> { model with LastLine = tentativeUpdateLine model.LastLine c }

  | _ -> { model with LastLine = tentativeUpdateLine model.LastLine c }

let checkResult (model: MultilineState) = checkLineResult model.LastLine

let updateModel (model: MultilineState) (c: CharMsg) =
  let intermediateResult = tentativeUpdate model c

  if checkResult intermediateResult then
    intermediateResult
  else
    model


[<ReactComponent>]
let App () =
  let models = [||]
  // let models =
  //   [|
  //      // OnLeftOperand ""
  //      OnLeftOperand "0"
  //      OnLeftOperand "0h"
  //      OnLeftOperand "0h1"
  //      OnLeftOperand "0h10"

  //      OnRightOperand("0h10", Op.Plus, "")
  //      OnRightOperand("0h10", Op.To, "02h")
  //      OnRightOperand("0h10", Op.Sub, "02h28")

  //      DisplayResult("0h10", Op.To, "02h28", isFinalResult = true)
  //      DisplayResult("0h10", Op.Plus, "02h28", isFinalResult = false) |]

  let models = Array.map MultilineState.FromSingle models
  // let models = [|
  //   { LastLine = DisplayResult("0h10", Op.To, "02h28", isFinalResult = false)
  //     OtherLines = [
  //       DisplayResult("18h52", Op.To, "19h21", isFinalResult = false)
  //       DisplayResult("16h30", Op.To, "18h33", isFinalResult = false)
  //       DisplayResult("11h49", Op.To, "12h13", isFinalResult = false)
  //     ]
  //   }
  // |]

  let state, dispatch = React.useReducer (updateModel, MultilineState.InitialState)

  let keyDownHandler =
    React.useCallback (
      (fun (e: Browser.Types.Event) ->
        let ke: KeyboardEvent = downcast e

        match ke.key with
        | "Backspace" -> dispatch Backspace
        | "Escape" -> dispatch Escape
        | "Enter" -> dispatch Enter
        | " " -> dispatch Space
        | "Tab" ->
          e.preventDefault ()
          dispatch Tab
        | "+" -> dispatch Plus
        | "-" -> dispatch Minus
        | "h"
        | "H" -> dispatch HourSep
        | key when key.Length = 1 && '0' <= key[0] && key[0] <= '9' -> dispatch (Digit key[0])
        | _ -> ()

        ),
      [||]
    )

  React.useEffect (
    (fun () ->
      Browser.Dom.document.addEventListener ("keydown", keyDownHandler)
      // mkDisposable (fun () ->
      //   Browser.Dom.document.removeEventListener("keydown", keyDownHandler)
      // )
      ),
    [||]
  )

  let modelsToRender = [| state; yield! models |]

  Layout modelsToRender
