module Matrix
open System
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

let submatrix row col m = 
    let rs = [0 .. (Array2D.length1 m) - 1]
    let cs = [0 .. (Array2D.length2 m) - 1]

    if not (List.contains row rs) || not (List.contains col cs) then 
        raise (IndexOutOfRangeException("Submatrix indexes out of bounds"))
    else
        let all_indicies = rs |> List.collect (fun r -> cs |> List.map (fun c -> r, c))
        let remain = all_indicies 
                     |> List.filter (fun (r, _) -> r <> row) 
                     |> List.filter (fun (_, c) -> c <> col) 
                     |> Array.ofList
    
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
    let rec det_aux c c_max res a = 
        if c = c_max then
            res
        else
            let prefactor = pown -1.0 c
            let cf = prefactor * (det (submatrix 0 c a))
            det_aux (c+1) c_max (res + a.[0,c]*cf) a

    if Array2D.length1 a = 2 then
        a[0,0]*a[1,1] - a[0,1]*a[1,0]
    else
        det_aux 0 (Array2D.length2 a) 0 a

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
