module App.View

open System
open Elmish
open Fable.Core
open Fable.React
open Fulma
open TimeCalc
open TimeCalc.Model
open FitText


/// Uses Fable's Emit to call JavaScript directly and play sounds
[<Emit("(new Audio($0)).play();")>]
let play (fileName: string) = jsNative
let playKeyPress () = play "assets/keypress.wav"


let defaultTimeFormData = "", Empty
let defaultLineCalculationState = OperandLeft(defaultTimeFormData, 0)

let init _ = //InitialState, Cmd.none
  { HasInitialState = true; CurrentForm = 0; CursorPos = 0
    Timestamps = [| "", Empty |]; Operation = None; ShowResult = false }, Cmd.none


let private update charMsg (model: Model) =
  let res =
    Model.convertToMsg model charMsg
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


let private spanClass classNames txt = Text.span [CustomClass classNames] [str txt]
let private displaySeparator txt = spanClass "display-time-sep frozen" txt
let private displayOperand subModel = spanClass "display-time" (sprintf "%O_" <| fst subModel)
let private displayResult (ts: TimeSpan) =
  spanClass "display-time display-result" (sprintf "%dh%02d" ts.Hours ts.Minutes)

let private contentLvl2Form (model: Model) _dispatch =
    let fitTextParams = [ Compressor 0.95; MaxFontSize 128; Debounce 50 ]
    let cursor = if not model.ShowResult then "_" else String.Empty

    fitText fitTextParams [
      match model.FormCount with
      | 1 when model.HasInitialState -> spanClass "display-time" "ex: 8h30_"
      | 1 -> displayOperand model.Timestamps.[0]
      | _ ->
        let param1Class = "display-time frozen"
        let operandTimeClass = if not model.ShowResult then "display-time" else "display-time frozen"

        spanClass param1Class (string <| snd model.Timestamps.[0])
        displaySeparator (
          model.Operation
          |> Option.map (function Diff -> " -> " | Sum -> " + " | Sub -> " - ")
          |> Option.defaultValue "")
        spanClass operandTimeClass (
          if not model.ShowResult
          then sprintf "%s%s" (fst model.Timestamps.[1]) cursor
          else sprintf "%O%s" (snd model.Timestamps.[1]) cursor)

        // Display result
        if model.ShowResult then
          let ts1 = (snd model.Timestamps.[0]).ToTimeSpan()
          let ts2 = (snd model.Timestamps.[1]).ToTimeSpan()

          displaySeparator " = "

          match model.Operation with
          | Some Diff ->
            let diff = if ts1 < ts2 then ts2.Subtract(ts1) else ts1.Subtract(ts2)
            displayResult diff
          | Some Sum -> ts1.Add(ts2) |> displayResult
          | Some Sub -> ts1.Subtract(ts2) |> displayResult
          | _ -> ()
    ] |> List.singleton



let emptyColumn = Column.column [] []

let private contentLvl1Column (model: Model) dispatch =
  [ Container.container [] [
      // Title
      Heading.h2 [ Heading.CustomClass "title" ] [
        if model.FormCount = 1 || not model.ShowResult
        then str "time calculator"
        else str "results"
      ]

      // Content
      Columns.columns [ Columns.CustomClass "has-text-centered"; Columns.IsVCentered ] [
        emptyColumn
        Column.column
          [ Column.Width(Screen.All, if model.FormCount = 1 then Column.IsFourFifths else Column.Is9) ]
          (contentLvl2Form model dispatch)
        emptyColumn
      ]
    ]
  ]

let private view model dispatch =
  Hero.hero [ Hero.IsFullHeight; Hero.IsMedium ] [
    Hero.body [] (contentLvl1Column model dispatch)
  ]


open Elmish.Debug
open Elmish.HMR

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withSubscription Input.documentEventListener
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
