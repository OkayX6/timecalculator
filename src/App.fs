module App.View

open System
open Browser.Dom
open Browser.Types
open Elmish
open Fable.Core
open Fable.React
open Fulma
open TimeFormat


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


type CharMsg =
  | HourSep
  | Digit of char
  | Left
  | Right
  | Plus
  | Tab
  | Space
  | Backspace
  | Enter
  member x.StringRepr =
    match x with
    | HourSep -> "h"
    | Digit c -> string c
    | _ -> ""

type Msg =
  | DisableInitialState
  | ChangeValue of (string * TimeFormat) * cursorPosDiff:int
  | CreateForm of Op
  | RemoveCurrentForm
  | GoToPreviousForm
  | GoToNextForm
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
  | Tab | Enter when model.CurrentForm = 0 -> CreateForm Diff |> Some
  | Plus when model.CurrentForm = 0 -> CreateForm Sum |> Some
  | Tab | Enter when model.CurrentForm = 1 -> Some <| ShowResult true
  | _ -> None

let private update charMsg (model: Model) =
  let res =
    convertToMsg model charMsg
    |> Option.map (fun msg ->
      playKeyPress ()
      match msg with
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
    let cursor = if not model.ShowResult then "_" else String.Empty
    [
      match model.FormCount with
      | 1 when model.HasInitialState -> spanClass "display-time" "ex: 8h30_"
      | 1 ->
        spanClass "display-time" (fst model.Timestamps.[0])
        spanClass "display-time" cursor
      | _ ->
        div [] [
          let operandTimeClass = if not model.ShowResult then "display-time" else "display-time frozen"
          Text.span [ CustomClass "display-time frozen" ] [ str (string <| snd model.Timestamps.[0]) ]
          Text.span [ CustomClass "display-time-sep" ] [
            match model.Operation with
            | Some Diff -> str " to "
            | Some Sum -> str " + "
            | _ -> () ]
          Text.span [ CustomClass operandTimeClass ]
            [ if not model.ShowResult then
                str (sprintf "%s%s" (fst model.Timestamps.[1]) cursor)
              else
                str (sprintf "%s%s" (string <| snd model.Timestamps.[1]) cursor) ]

          // Display result
          if model.ShowResult then
            let ts1 = (snd model.Timestamps.[0]).ToTimeSpan()
            let ts2 = (snd model.Timestamps.[1]).ToTimeSpan()

            spanClass "display-time-sep" " = "

            match model.Operation with
            | Some Diff ->
              let diff = if ts1 < ts2 then ts2.Subtract(ts1) else ts1.Subtract(ts2)
              spanClass "display-time display-result" (sprintf "%dh%02d" diff.Hours diff.Minutes)
            | Some Sum ->
              let sum = ts1.Add(ts2)
              spanClass "display-time display-result" (sprintf "%dh%02d" sum.Hours sum.Minutes)
            | _ -> ()
        ]

    ]


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
                    Column.Width(Screen.All, Column.Is12) ]
                (contentLvl2Form model dispatch)
              emptyColumn
            ] ] ]

let private view model dispatch =
    Hero.hero
        [ Hero.IsFullHeight
          Hero.IsMedium ] [ Hero.body [] (contentLvl1Column model dispatch) ]


let documentEventListener initial =
  let sub dispatch =
    document.addEventListener("keydown", fun e ->
      let ke: KeyboardEvent = downcast e
      printfn "%s" ke.key
      match ke.key with
      | "Backspace"                           -> dispatch Backspace
      | "Tab"                                 -> e.preventDefault(); dispatch Tab
      | "+"                                   -> dispatch Plus
      | "h" | "H"                             -> HourSep |> dispatch
      | key when key.Length = 1 && '0' <= key.[0] && key.[0] <= '9'
          -> Digit key.[0] |> dispatch
      | _ -> ()
    )

  Cmd.ofSub sub



open Elmish.Debug
open Elmish.HMR

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withSubscription documentEventListener
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
