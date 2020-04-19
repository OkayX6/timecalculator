module TimeCalc.Input

open Browser.Dom
open Browser.Types
open Elmish


type CharMsg =
  | HourSep
  | Digit of char
  | Left | Right
  | Plus | Minus
  | Tab
  | Space
  | Backspace
  | Enter
  | Escape
  member x.StringRepr =
    match x with
    | HourSep -> "h"
    | Digit c -> string c
    | _ -> ""


let documentEventListener initial =
  let sub dispatch =
    document.addEventListener("keydown", fun e ->
      let ke: KeyboardEvent = downcast e
      match ke.key with
      | "Backspace" -> dispatch Backspace
      | "Escape"    -> dispatch Escape
      | "Enter"     -> dispatch Enter
      | " "         -> dispatch Space
      | "Tab"       -> e.preventDefault(); dispatch Tab
      | "+"         -> dispatch Plus
      | "-"         -> dispatch Minus
      | "h" | "H"   -> HourSep |> dispatch
      | key when key.Length = 1 && '0' <= key.[0] && key.[0] <= '9'
          -> Digit key.[0] |> dispatch
      | _ -> ()
    )

    document.addEventListener("visibilitychange", fun e ->
      printfn "visibility change_"
    )

  Cmd.ofSub sub
