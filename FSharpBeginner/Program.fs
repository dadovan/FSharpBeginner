open Microsoft.VisualStudio.TestTools.UnitTesting;

let square x = x * x

let hyp t0 t1 x = t0 + (t1 * x)

let cost t0 t1 x y = (hyp t0 t1 x) - y

let sumCost t0 t1 d =
    d
    |> List.map (fun (x,y) -> cost t0 t1 x y)
    |> List.sum

let allCost t0 t1 (d : (float * float) list) = (1.0 / (2.0 * (float d.Length))) * (square (sumCost t0 t1 d))

let j0Derivative t0 t1 (d : (float * float) list) = (1.0 / (float d.Length)) * ( d |> List.map (fun (x,y) -> cost t0 t1 x y) |> List.sum )
let j1Derivative t0 t1 (d : (float * float) list) = (1.0 / (float d.Length)) * ( d |> List.map (fun (x,y) -> (cost t0 t1 x y) * x) |> List.sum )

let oneDescent t0 t1 alpha d =
    let temp0 = t0 - alpha * (j0Derivative t0 t1 d)
    let temp1 = t1 - alpha * (j1Derivative t0 t1 d)
    [temp0; temp1]

let rec fullGD (t : float list) a d i currentAllCost =
    let newt = oneDescent t.[0] t.[1] a d
    let newAllCost = allCost (newt.[0]) (newt.[1]) d
    let i = i + 1
    if ((i % 1000) = 0) || (newAllCost >= currentAllCost) then
        printfn "Iteration: %i, t0: %f, t1: %f, cost: %f" i newt.[0] newt.[1] newAllCost
    if (newAllCost >= currentAllCost) then
        newt
    else
        fullGD newt a d i newAllCost

[<EntryPoint>]
let main argv = 
    let data = [ (0.0, 1.0); (1.0, 2.0); (3.0, 2.0); (4.0, 3.0)];
    printfn "Data: %A" data

    // Tests
    let t0 = 1.87
    let t1 = 0.237
    Assert.AreEqual(0.107, (cost t0 t1 1.0 2.0), 0.001)
    Assert.AreEqual(0.344, (j0Derivative t0 t1 data), 0.001)

    // Gradient descent to find approx global optimum
    let newT = fullGD [4.0; 1.2] 0.01 data 1 (float System.Int32.MaxValue)
    Assert.AreEqual(1.2, newT.[0], 0.0001);
    Assert.AreEqual(0.4, newT.[1], 0.0001);

    0 // return an integer exit code
