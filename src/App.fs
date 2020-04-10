module App.View

open System
open Elmish
open Fable.React
open Fable.React.Props
open Fulma
open Browser.Types
open Browser.Dom
open TimeFormat


type Op = Diff | Sum
type Model =
    { CurrentForm: int
      Timestamps: (string * TimeFormat) []
      Operation: Option<Op>
      ShowResult: bool }
    member x.FormCount = x.Timestamps.Length


let replaceValueAt model i value =
    let updatedArray = Array.copy model.Timestamps
    updatedArray.[i] <- value
    { model with Timestamps = updatedArray }


type Msg =
    | ChangeValue of (string * TimeFormat)
    | CreateForm of Op
    | ShowResult of bool
    | DocumentBackspace
    | RemoveCurrentForm
    | GoToPreviousForm
    | GoToNextForm

let init _ =
    { CurrentForm = 0
      Timestamps = [| "", Empty |]
      Operation = None
      ShowResult = false }, Cmd.none

let private update msg model =
    match msg with
    | ChangeValue newValue -> replaceValueAt model model.CurrentForm newValue, Cmd.none
    | GoToPreviousForm -> { model with CurrentForm = model.CurrentForm - 1 }, Cmd.none
    | GoToNextForm -> { model with CurrentForm = model.CurrentForm + 1 }, Cmd.none
    | CreateForm op ->
        { model with
            CurrentForm = 1
            Timestamps = Array.append model.Timestamps [| "", Empty |]
            Operation = Some op }, Cmd.none
    | ShowResult value -> { model with ShowResult = value }, Cmd.none
    | RemoveCurrentForm ->
        { model with
            CurrentForm = model.CurrentForm - 1
            Timestamps = Array.sub model.Timestamps 0 (model.Timestamps.Length - 1)
            Operation = None
            ShowResult = false }, Cmd.none
    | DocumentBackspace -> { model with ShowResult = false }, Cmd.none


let handleKeyDownInInput model dispatch (ev: KeyboardEvent) =
  let idx, elt = model.CurrentForm, ev.target :?> HTMLInputElement
  let inputValue, _ = model.Timestamps.[idx]

  match ev.key with
  | "+" when idx = 0 -> dispatch (CreateForm Sum)
  | "Tab" | "Enter" ->
    ev.preventDefault()
    if idx = 0 then dispatch (CreateForm Diff)
    elif idx = 1 then dispatch (ShowResult true)
  | "Backspace" ->
    dispatch (ShowResult false)
    if inputValue |> String.IsNullOrWhiteSpace && model.CurrentForm > 0 then
      ev.preventDefault()
      dispatch RemoveCurrentForm
  | "ArrowLeft" when elt.selectionStart = 0 && model.CurrentForm > 0 ->
    ev.preventDefault()
    dispatch GoToPreviousForm
  | "ArrowRight" when elt.selectionEnd = inputValue.Length && model.CurrentForm = 0 && model.FormCount >= 2 ->
    dispatch GoToNextForm
  | _ -> ()


let timeInput idx model dispatch =
    let hasFocus = idx = model.CurrentForm
    let inputValue, _ = model.Timestamps.[idx]

    if hasFocus && not model.ShowResult then
      Control.p []
        [ Input.text
            [ Input.Props
                [ Style [
                    if idx = 0 && inputValue |> String.IsNullOrEmpty then
                      TextAlign TextAlignOptions.Right
                    elif idx = 0 then
                      TextAlign TextAlignOptions.Center
                    else
                      TextAlign TextAlignOptions.Left

                    Padding "1rem" ]
                  if hasFocus then AutoFocus true
                  OnKeyDown (handleKeyDownInInput model dispatch) ]
              Input.OnChange(fun ev ->
                  match validateTime ev.Value with
                  | Some format -> dispatch (ChangeValue(ev.Value, format))
                  | None -> ())
              if idx = 0 then Input.Placeholder "ex: 8h45"
              Input.Value inputValue
              Input.CustomClass "time-input"
            ] ]
    else
      Content.content
        [ Content.CustomClass "display-time"
          Content.Modifiers [
            Modifier.TextWeight TextWeight.Bold
            if idx = 0 then
              Modifier.TextAlignment(Screen.All, TextAlignment.Right)
            else
              Modifier.TextAlignment(Screen.All, TextAlignment.Left)
          ]
        ]
        [ str (string <| snd model.Timestamps.[idx]) ]


let private contentLvl2_Form (model: Model) dispatch =
    [
      if model.FormCount = 1 then
        timeInput 0 model dispatch
      else
          Columns.columns
            [ Columns.IsVCentered ]
            [ Column.column [] [ timeInput 0 model dispatch ]
              Column.column
                [ Column.Width(Screen.All, Column.IsNarrow) ]
                [ Content.content [ Content.CustomClass "display-time-sep" ] [
                    match model.Operation with
                    | Some Diff -> str " to "
                    | Some Sum -> str " + "
                    | _ -> ()
                ] ]
              Column.column [] [ timeInput 1 model dispatch ] ]

      if model.ShowResult then
        let ts1 = (snd model.Timestamps.[0]).ToTimeSpan()
        let ts2 = (snd model.Timestamps.[1]).ToTimeSpan()
        let style = [
          Content.CustomClass "display-result"
          Content.Modifiers [ Modifier.TextWeight TextWeight.Bold ]
        ]

        match model.Operation with
        | Some Diff ->
          let diff = if ts1 < ts2 then ts2.Subtract(ts1) else ts1.Subtract(ts2)
          Content.content style [ str <| sprintf "Δ = %dh%02d" diff.Hours diff.Minutes ]
        | Some Sum ->
          let sum = ts1.Add(ts2)
          Content.content style [ str <| sprintf "Σ = %dh%02d" sum.Hours sum.Minutes ]
        | _ -> ()
    ]


let private contentLvl1_Column (model: Model) dispatch =
    [ Container.container []
        [
          // Title
          if model.FormCount = 1 || model.ShowResult |> not then
            Heading.h2 [ Heading.CustomClass "title" ] [ str "time calculator" ]
          else
            Heading.h4 [ Heading.CustomClass "title title-results"] [ str "results" ]

          // Content
          Columns.columns
            [ Columns.CustomClass "has-text-centered"
              Columns.IsVCentered ]
            [
              Column.column [] []
              Column.column
                [ if model.FormCount = 1 then
                    Column.Width(Screen.All, Column.Is4)
                  else
                    Column.Width(Screen.All, Column.IsFourFifths) ]
                (contentLvl2_Form model dispatch)
              Column.column [] []
            ] ] ]

let private view model dispatch =
    Hero.hero
        [ Hero.IsFullHeight
          Hero.IsMedium ] [ Hero.body [] (contentLvl1_Column model dispatch) ]


let documentEventListener initial =
  let sub dispatch =
    document.addEventListener("keydown", fun e ->
      let ke: KeyboardEvent = downcast e
      if ke.key = "Backspace" then
        if (e.target :?> HTMLElement).nodeName.ToUpperInvariant() = "BODY" then
          e.preventDefault()
          dispatch DocumentBackspace
        else ()
      elif ke.key = "Tab" then e.preventDefault()
    )
  Cmd.ofSub sub


type Model2 = string
type Msg2 =
  | Type of char
  | Backspace

let update2 msg model =
  match msg, model with
  | Type c, s -> s + string c, Cmd.none
  | Backspace, "" -> "", Cmd.none
  | Backspace, s -> s.Substring(0, s.Length - 1), Cmd.none


let init2 _ = "hello...", Cmd.none

let view2 model dispatch =
  Hero.hero
    [ Hero.IsFullHeight
      Hero.IsMedium ]
    [ Hero.body []
        [ Container.container [ Container.Props [ Style [ FontSize "3rem" ] ] ]
            [ str (model + "_")] ] ]

let keyListener initial =
  let sub dispatch =
    document.addEventListener("keydown", fun e ->
      let ke: KeyboardEvent = downcast e
      if ke.key = "Tab" then e.preventDefault()
      elif ke.key = "Backspace" then dispatch Backspace
      else
        match ke.key.ToCharArray() with
        | [|' '|] -> dispatch (Type ' ')
        | [|x|] when 'a' <= x && x <= 'z' || 'A' <= x && x <= 'Z' || '0' <= x && x <= '9' -> dispatch (Type x)
        | _ -> ()
    )
  Cmd.ofSub sub


open Elmish.Debug
open Elmish.HMR

Program.mkProgram init update view
// Program.mkProgram init2 update2 view2
|> Program.withReactSynchronous "elmish-app"
|> Program.withSubscription documentEventListener
// |> Program.withSubscription keyListener
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
