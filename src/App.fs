module App.View

open System
open Elmish
open Fable.Core
open Fable.React
open Fable.React.Props
open Fulma
open TimeCalc
open TimeCalc.Input
open TimeCalc.TimeFormat
open FitText


/// Uses Fable's Emit to call JavaScript directly and play sounds
[<Emit("(new Audio($0)).play();")>]
let play (fileName: string) = jsNative


type Op = Diff | Sum
type Model =
  { HasInitialState: bool
    CurrentForm: int
    CursorPos: int
    Timestamps: (string * TimeFormat) []
    Operation: Option<Op>
    ShowResult: bool }
  member x.FormCount = x.Timestamps.Length
  member x.CurrentTimestamp = x.Timestamps.[x.CurrentForm]


let replaceValueAt array i value =
    let updatedArray = Array.copy array
    updatedArray.[i] <- value
    updatedArray


type Msg =
  | DisableInitialState
  | ChangeValue of (string * TimeFormat) * cursorPosDiff:int
  | CreateForm of Op
  | RemoveCurrentForm
  | GoToPreviousForm
  | GoToNextForm
  | Reset
  | ShowResult of bool


let init _ =
  { HasInitialState = true
    CurrentForm = 0
    CursorPos = 0
    Timestamps = [| "", Empty |]
    Operation = None
    ShowResult = false }, Cmd.none


let playKeyPress () = play "assets/keypress.wav"


let convertToMsg (model: Model) msg =
  match msg with
  | (HourSep | Digit _) as c ->
      let str, _ = model.CurrentTimestamp
      let newStr = str + c.StringRepr
      match validateTime newStr with
      | Some format -> ChangeValue ((newStr, format), 1) |> Some
      | None -> None
  | Left | Right -> None
  | Escape -> Some Reset
  | Backspace when model.HasInitialState -> DisableInitialState |> Some
  | Backspace when model.ShowResult -> ShowResult false |> Some
  | Backspace when snd model.CurrentTimestamp = Empty && model.CurrentForm = 1 ->
      Some RemoveCurrentForm
  | Backspace ->
      let str, _ = model.CurrentTimestamp
      let cpos = model.CursorPos
      let newStr = str.[0..cpos-2] + str.Substring(cpos)

      if cpos > 0 then
        match validateTime newStr with
        | Some format -> ChangeValue ((newStr, format), -1) |> Some
        | None -> None
      else None
  | Tab | Enter | Space when model.CurrentForm = 0 -> CreateForm Diff |> Some
  | Plus when model.CurrentForm = 0 -> CreateForm Sum |> Some
  | Tab | Enter | Space when model.CurrentForm = 1 -> Some <| ShowResult true
  | _ -> None

let private update charMsg (model: Model) =
  let res =
    convertToMsg model charMsg
    |> Option.map (fun msg ->
      playKeyPress ()
      match msg with
      | Reset -> { fst (init ()) with HasInitialState = false }
      | DisableInitialState -> { model with HasInitialState = false }
      | ChangeValue(newTs, cursorPosDiff) ->
          { model with
              HasInitialState = false
              Timestamps = replaceValueAt model.Timestamps model.CurrentForm newTs
              CursorPos = model.CursorPos + cursorPosDiff }
      | CreateForm op ->
        { model with
            Operation = Some op; CurrentForm = 1; CursorPos = 0
            Timestamps = Array.append model.Timestamps [| "", Empty |] }
      | GoToPreviousForm -> { model with CurrentForm = model.CurrentForm - 1; CursorPos = 0 }
      | GoToNextForm -> { model with CurrentForm = model.CurrentForm + 1; CursorPos = 0 }
      | ShowResult value -> { model with ShowResult = value }
      | RemoveCurrentForm ->
          let newIdx = model.CurrentForm-1
          { model with
              CurrentForm = newIdx;
              CursorPos = fst model.Timestamps.[newIdx] |> String.length
              Timestamps = Array.sub model.Timestamps 0 (model.Timestamps.Length - 1)
              Operation = None; ShowResult = false }
    )
    |> Option.defaultValue model

  res, Cmd.none


let private spanClass classNames txt =
  Text.span [CustomClass classNames] [str txt]

let private contentLvl2Form (model: Model) dispatch =
    let fitTextParams = [ Compressor 0.95; MaxFontSize 128; Debounce 50 ]
    let cursor = if not model.ShowResult then "_" else String.Empty

    fitText fitTextParams [
      match model.FormCount with
      | 1 when model.HasInitialState ->
        spanClass "display-time" "ex: 8h30_"
      | 1 ->
        spanClass "display-time" (fst model.Timestamps.[0])
        spanClass "display-time" cursor
      | _ ->
        let param1Class = "display-time frozen"
        let operandTimeClass = if not model.ShowResult then "display-time" else "display-time frozen"

        spanClass param1Class (string <| snd model.Timestamps.[0])
        spanClass "display-time-sep frozen" (
          match model.Operation with
          | Some Diff -> " -> "
          | Some Sum -> " + "
          | _ -> "")

        spanClass operandTimeClass (
          if not model.ShowResult then
            sprintf "%s%s" (fst model.Timestamps.[1]) cursor
          else
            sprintf "%O%s" (snd model.Timestamps.[1]) cursor)

        // Display result
        if model.ShowResult then
          let ts1 = (snd model.Timestamps.[0]).ToTimeSpan()
          let ts2 = (snd model.Timestamps.[1]).ToTimeSpan()

          spanClass "display-time-sep frozen" " = "

          match model.Operation with
          | Some Diff ->
            let diff = if ts1 < ts2 then ts2.Subtract(ts1) else ts1.Subtract(ts2)
            spanClass "display-time display-result" (sprintf "%dh%02d" diff.Hours diff.Minutes) // Δ
          | Some Sum ->
            let sum = ts1.Add(ts2)
            spanClass "display-time display-result" (sprintf "%dh%02d" sum.Hours sum.Minutes)
          | _ -> ()
    ] |> List.singleton



let emptyColumn = Column.column [] []

let private contentLvl1Column (model: Model) dispatch =
    [ Container.container []
        [
          // Title
          if model.FormCount = 1 || model.ShowResult |> not then
            Heading.h2 [ Heading.CustomClass "title" ] [ str "time calculator" ]
          else
            Heading.h2 [ Heading.CustomClass "title" ] [ str "results" ]

          // Content
          Columns.columns
            [ Columns.CustomClass "has-text-centered"
              Columns.IsVCentered ]
            [
              emptyColumn
              Column.column
                [ if model.FormCount = 1 then
                    Column.Width(Screen.All, Column.IsFourFifths)
                  else
                    Column.Width(Screen.All, Column.Is9) ]
                (contentLvl2Form model dispatch)
              emptyColumn
            ] ] ]

let private view model dispatch =
    Hero.hero
        [ Hero.IsFullHeight
          Hero.IsMedium ] [ Hero.body [] (contentLvl1Column model dispatch) ]



open Elmish.Debug
open Elmish.HMR

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withSubscription Input.documentEventListener
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
