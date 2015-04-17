module DataLoader

open System
open System.IO
open FSharp.Collections.ParallelSeq

type row = { sepalLength : float; sepalWidth : float; petalLength : float; petalWidth : float; species : string }

// sepal-length,sepalwidth,petal-length,petal-width,species
// 4.7,3.2,1.6,0.2,Iris-setosa
let parseLine (line : string) =
    let parts = line.Split( [| ',' |] )
    let r =
        {
        sepalLength = (Double.Parse(parts.[0]));
        sepalWidth = (Double.Parse(parts.[1]));
        petalLength = (Double.Parse(parts.[2]));
        petalWidth = (Double.Parse(parts.[3]));
        species = parts.[4];
        }
    r

let doit =
    let lines = File.ReadAllLines "..\..\Iris.data.csv" |> Seq.skip 1 // Skip the header
    let data = lines |> PSeq.map parseLine
    let sum = data |> PSeq.sumBy (fun x -> x.sepalLength)
    printfn "%f" sum

    0
