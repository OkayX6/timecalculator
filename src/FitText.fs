module FitText

open Fable.Core
open Fable.React


type FitTextProps =
  | Compressor of float
  | MinFontSize of int
  | MaxFontSize of int
  | DefaultFontSize of int
  | Debounce of milliseconds:int // In ms

let inline fitText (props : FitTextProps list) children : ReactElement =
  ofImport "default" "@kennethormandy/react-fittext" (JsInterop.keyValueList CaseRules.LowerFirst props) [fragment [] children]