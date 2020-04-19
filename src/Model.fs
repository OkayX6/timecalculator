namespace TimeCalc


type Op = Diff | Sum
type TimeFormData = string * TimeFormat
type CursorPos = int

type LineCalculationState =
  | OperandLeft of TimeFormData * CursorPos
  | OperandRight of TimeFormData * Op * TimeFormData * CursorPos
  | DisplayResult of TimeFormData * Op * TimeFormData

// type Model =
//   | InitialState
//   | IsEditing of LineCalculationState

type Model =
  { HasInitialState: bool
    CurrentForm: int
    CursorPos: int
    Timestamps: (string * TimeFormat) []
    Operation: Option<Op>
    ShowResult: bool }
  member x.FormCount = x.Timestamps.Length
  member x.CurrentTimestamp = x.Timestamps.[x.CurrentForm]

type Msg =
  | DisableInitialState
  | ChangeValue of (string * TimeFormat) * cursorPosDiff:int
  | CreateForm of Op
  | RemoveCurrentForm
  | GoToPreviousForm
  | GoToNextForm
  | Reset
  | ShowResult of bool


module Model =
  open TimeCalc.Input
  open TimeCalc.TimeFormat


  let replaceValueAt array i value =
      let updatedArray = Array.copy array
      updatedArray.[i] <- value
      updatedArray

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
    | Backspace when snd model.CurrentTimestamp = Empty && model.CurrentForm = 1 -> Some RemoveCurrentForm
    | Backspace ->
        let str, cpos = fst model.CurrentTimestamp, model.CursorPos
        let newStr = str.[0..cpos-2] + str.Substring(cpos)

        if cpos > 0 then
          match validateTime newStr with
          | Some format -> ChangeValue ((newStr, format), -1) |> Some
          | None -> None
        else None
    | Enter when model.HasInitialState -> None
    | Tab | Enter | Space when model.CurrentForm = 0 -> CreateForm Diff |> Some
    | Tab | Enter | Space when model.CurrentForm = 1 -> ShowResult true |> Some
    | Plus when model.CurrentForm = 0 -> CreateForm Sum |> Some
    | _ -> None
