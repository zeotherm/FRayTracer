module Matrix
open System
open System.Collections.Generic
open Tuples

let make_matrix es = array2D es

let make_ident_mat N = Array2D.init N N (fun i j -> if i = j then 1.0 else 0.0)

let mat_mat_mul (a: double[,]) (b: double[,]) =
    let inner_prod (u: double[]) (v: double[]): double = 
        let mutable sum = 0.0
        for i in 0.. u.Length - 1 do
            sum <- sum + u.[i]*v.[i]
        sum
    Array2D.init (Array2D.length1 a) (Array2D.length2 a) (fun r c -> inner_prod a[r,*] b[*,c])

let mat_tuple_mul (a: double[,]) (t: Tuple): Tuple = 
    let x' = t.x * a[0,0] + t.y * a[0,1] + t.z * a[0,2] + t.w * a[0,3]
    let y' = t.x * a[1,0] + t.y * a[1,1] + t.z * a[1,2] + t.w * a[1,3]
    let z' = t.x * a[2,0] + t.y * a[2,1] + t.z * a[2,2] + t.w * a[2,3]
    let w' = t.x * a[3,0] + t.y * a[3,1] + t.z * a[3,2] + t.w * a[3,3]

    Tuple(x', y', z', w')

let transpose a = 
    Array2D.init (Array2D.length1 a) (Array2D.length2 a) (fun i j -> a[j,i])

let memoize f =
    let dict = Dictionary<_, _>();
    fun c ->
        let exist, value = dict.TryGetValue c
        match exist with
        | true -> value
        | _ -> 
            let value = f c
            dict.Add(c, value)
            value

type sub_index_pack = int*int*int*int

let get_sub_indicies si_pack = 
    let (l1, l2, row, col) = si_pack
    
    let rs = [0 .. (l1 - 1)]
    let cs = [0 .. (l2 - 1)]

    if not (List.contains row rs) || not (List.contains col cs) then 
        raise (IndexOutOfRangeException("Submatrix indexes out of bounds"))
    else
        let all_indicies = rs |> List.collect (fun r -> cs |> List.map (fun c -> r, c))
        let remain = all_indicies 
                     |> List.filter (fun (r, _) -> r <> row) 
                     |> List.filter (fun (_, c) -> c <> col) 
                     |> Array.ofList
        remain

let memo_indicies = memoize get_sub_indicies
let submatrix row col m = 
    let remain = memo_indicies ((Array2D.length1 m),(Array2D.length2 m),row, col)
    
    let temp = Array2D.create ((Array2D.length1 m) - 1) ((Array2D.length2 m) - 1) 0.0
    let mutable i = 0
    for r in 0 .. ((Array2D.length1 m) - 2) do
        for c in 0 .. ((Array2D.length2 m) - 2) do
            let old_r = fst remain.[i]
            let old_c = snd remain.[i]
            temp.[r,c] <- m.[old_r, old_c]
            i <- i + 1
    temp

let rec det (a: double[,]): double = 
    if Array2D.length1 a = 2 then
        a[0,0]*a[1,1] - a[0,1]*a[1,0]
    else if Array2D.length1 a = 3 then
        let A = a.[0,0]
        let B = a.[0,1]
        let C = a.[0,2]
        let D = a.[1,0]
        let E = a.[1,1]
        let F = a.[1,2]
        let G = a.[2,0]
        let H = a.[2,1]
        let I = a.[2,2]
        A*(E*I-F*H)-B*(D*I-F*G)+C*(D*H-G*E)
    else
        let A11 = a.[0,0]
        let A12 = a.[0,1]
        let A13 = a.[0,2]
        let A14 = a.[0,3]
        let A21 = a.[1,0]
        let A22 = a.[1,1]
        let A23 = a.[1,2]
        let A24 = a.[1,3]
        let A31 = a.[2,0]
        let A32 = a.[2,1]
        let A33 = a.[2,2]
        let A34 = a.[2,3]
        let A41 = a.[3,0]
        let A42 = a.[3,1]
        let A43 = a.[3,2]
        let A44 = a.[3,3]        
        A11*(A22*A33*A44+A23*A34*A42+A24*A32*A43-A24*A33*A42-A23*A32*A44-A22*A34*A43)-A21*(A12*A33*A44+A13*A34*A42+A14*A32*A43-A14*A33*A42-A13*A32*A44-A12*A34*A43)+A31*(A12*A23*A44+A13*A24*A42+A14*A22*A43-A14*A23*A42-A13*A22*A44-A12*A24*A43)-A41*(A12*A23*A34+A13*A24*A32+A14*A22*A33-A14*A23*A32-A13*A22*A34-A12*A24*A33)
 
let minor (row: int) (col: int) (a: double[,]): double =
    det (submatrix row col a)

let cofactor (row: int) (col:int) (a: double[,]): double = 
    (pown -1.0 (row + col)) * det (submatrix row col a)

let is_invertable a = 
    det a <> 0

let inverse (a: double[,]): double[,] = 
    let detA = det a
    if detA = 0 then
        raise (InvalidOperationException("The matrix is not invertable"))
    else
        Array2D.init (Array2D.length1 a) (Array2D.length2 a) (fun r c -> (cofactor c r a)/detA)
