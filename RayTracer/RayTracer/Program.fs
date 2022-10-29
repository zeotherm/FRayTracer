
module RayTracer

// For more information see https://aka.ms/fsharp-console-apps
let square x = x * x
let isOdd x = x % 2 <> 0

let squaresOfOdds xs =
    xs
    |> Seq.filter isOdd
    |> Seq.map square
