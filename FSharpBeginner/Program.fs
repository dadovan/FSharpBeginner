let data = [ (0.0, 1.0); (1.0, 2.0); (3.0, 2.0); (4.0, 3.0)];

let square x = x * x

let hyp t0 t1 x = t0 + (t1 * x)

let cost t0 t1 x y = (hyp t0 t1 x) - y

let costSquared cost = square cost

let sumCost t0 t1 d =
    d
    |> List.map (fun (x,y) -> cost t0 t1 x y)
    |> List.sum

let allCost t0 t1 d = (1.0 / (2.0 * (float data.Length))) * (sumCost t0 t1 d)

let j0Derivative t0 t1 d = (1.0 / (float data.Length)) / ( d |> List.map (fun (x,y) -> cost t0 t1 x y) |> List.sum )
let j1Derivative t0 t1 d = (1.0 / (float data.Length)) / ( d |> List.map (fun (x,y) -> (cost t0 t1 x y) * x) |> List.sum )

let oneGD t0 t1 alpha d =
    let temp0 = t0 - alpha * (j0Derivative t0 t1 d)
    let temp1 = t1 - alpha * (j1Derivative t0 t1 d)
    [temp0; temp1]

let rec fullGD (t : float list) a d i currentAllCost =
    let newt = oneGD t.[0] t.[1] a d
    let newAllCost = allCost (newt.[0]) (newt.[1]) d
    if (i % 100) = 0 then
        printfn "%i %f %f %f" i newt.[0] newt.[1] newAllCost
    let i = i + 1
    if (newAllCost >= currentAllCost) then
        newt
    else
        fullGD newt a d i newAllCost

[<EntryPoint>]
let main argv = 
    // printfn "%A" argv

    printfn "Data: %A" data

    // Best values/cost
    let t0 = 1.2
    let t1 = 0.4
    let c = cost t0 t1 0.0 1.0
    printfn "%f %f %f" t0 t1 c
    let x1 = sumCost t0 t1 data
    printfn "%A" x1
    let x2 = allCost t0 t1 data
    printfn "%A" x2

    // Gradient descent to find approx global optimum
    let alpha = 0.001
    let t0 = 4.0
    let t1 = 1.2
    let thetas = [t0; t1]
    let i = 1
    let newt = fullGD thetas alpha data i (float System.Int32.MaxValue)

    0 // return an integer exit code
