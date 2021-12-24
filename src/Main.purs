module Main where

import Prelude

import Effect (Effect)
import Spork.App as App
import Spork.Interpreter (merge, never, throughAff)
import Web (app, handleErrors, run)

main ∷ Effect Unit
main = do
  let interpreter = throughAff run handleErrors
  inst <- App.makeWithSelector (interpreter `merge` never) app "#app"
  inst.run
