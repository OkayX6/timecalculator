module Main

open Feliz
open App
open App.View
open Browser.Dom

let root = ReactDOM.createRoot(document.getElementById "feliz-app")
root.render(App())
