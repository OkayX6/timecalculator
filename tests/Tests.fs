module Tests

open Fable.Mocha
open App.View
open TimeCalc
open TimeCalc.Input

let tests =
  testList
    "Time Calculator Tests"
    [ testList
        "updateLineModel Tests"
        [ testCase "Empty line with digit input"
          <| fun _ ->
               let initial = OnLeftOperand ""
               let result = updateLineModel initial (Digit '8')
               Expect.equal result (OnLeftOperand "8") "Should add digit to empty line"

          testCase "Left operand with hour separator"
          <| fun _ ->
               let initial = OnLeftOperand "1"
               let result = updateLineModel initial HourSep
               Expect.equal result (OnLeftOperand "1h") "Should add hour separator"

          testCase "Left operand with invalid input"
          <| fun _ ->
               let initial = OnLeftOperand "6"
               let result = updateLineModel initial (Digit '0') // This would make minutes > 59
               Expect.equal result initial "Should not accept invalid input"

          testCase "Transition to right operand with plus"
          <| fun _ ->
               let initial = OnLeftOperand "1h"
               let result = updateLineModel initial Plus

               Expect.equal
                 result
                 (OnRightOperand("1h", Op.Plus, ""))
                 "Should transition to right operand with plus operator"
          ]

      testList
        "updateModel Tests"
        [ testCase "Empty model with digit input"
          <| fun _ ->
               let initial = MultilineState.InitialState
               let result = updateModel initial (Digit '1')
               Expect.equal result.LastLine (OnLeftOperand "1") "Should add digit to empty model"

          testCase "Model with complete line and tab"
          <| fun _ ->
               let initial = { LastLine = DisplayResult("1h", Op.Plus, "2h", true); OtherLines = [] }
               let result = updateModel initial Tab
               Expect.equal result.LastLine LineState.InitialState "Should create new line after tab"
               Expect.equal result.OtherLines.Length 1 "Should move completed line to history"

          testCase "Model with invalid input"
          <| fun _ ->
               let initial = MultilineState.InitialState
               let result = updateModel initial HourSep 
               Expect.equal result initial "Should not accept invalid input" ]
      ]


[<EntryPoint>]
let main (args: string[]) =
  Mocha.runTests tests
