module Proto2

open Browser.Dom
open Browser.Types
open Elmish
open Fable.React
open Fable.React.Props
open Fulma


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