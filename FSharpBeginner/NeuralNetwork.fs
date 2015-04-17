module NeuralNetwork

open System

let softmax (values : float[]) =
    let max = Array.max values
    let scale = Array.map (fun x -> Math.Exp(x - max)) values |> Array.sum
    Array.init values.Length (fun i -> Math.Exp(values.[i] - max) / scale)

let computeValues prevV (weights : float[,]) (biases : float[]) activation =
    Array.init biases.Length (fun j -> activation((Array.mapi (fun i x -> x * weights.[i, j]) prevV|> Array.sum) + biases.[j]))

let computeValuesSoftmax prevV (weights : float[,]) (biases : float[]) =
    softmax (Array.init biases.Length (fun j -> (Array.mapi (fun i x -> x * weights.[i, j]) prevV|> Array.sum) + biases.[j]))

let basicFeedForwardTest =
    let iv = [| 0.46246; 0.56387; 0.98518 |]

    let ihw = array2D [ [0.57426; 0.56688; 0.30634; 0.62644]; [0.07241; 0.35078; 0.62603; 0.12419]; [0.52969; 0.78555; 0.44469; 0.17940] ]
    let hb = [| 0.84816; 0.00589; 0.41893; 0.31132 |]

    let how = array2D [ [0.185883618512137; 0.262706959742451]; [0.382791653919402; 0.648913248744287]; [0.149259824375277; 0.0459774290425598]; [0.771017016270672; 0.413325088291115] ]
    let ob = [| 0.226960628399141; 0.0423739743616311 |]

    let hv = computeValues iv ihw hb Math.Tanh
    let ehv = [| 0.932393086142774, 0.845384790862073, 0.874453762631829, 0.689914067784969 |]
    printfn "hv: %A" hv
    printfn "ehv: %A" ehv

    let ov = computeValuesSoftmax hv how ob
    let eov = [| 0.55603223859186, 0.44396776140814 |]
    printfn "ov: %A" ov
    printfn "eov: %A" eov
