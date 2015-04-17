module Main

open Microsoft.VisualStudio.TestTools.UnitTesting;
open NeuralNetwork
open LinearRegression
open DataLoader

[<EntryPoint>]
let main argv = 
    basicFeedForwardTest
    basicLRTest
    0 // exit code
