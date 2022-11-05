module Tuples
    
type TupTest = 
    | Vector of double*double*double
    | Point of double*double*double


type Tuple(x: double, y: double, z: double, w: double) =
    member this.x = x
    member this.y = y
    member this.z = z
    member this.w = w

    static member (+) (t1: Tuple, t2: Tuple) = 
        Tuple(t1.x + t2.x, t1.y + t2.y, t1.z + t2.z, t1.w + t2.w)
    static member (-) (t1: Tuple, t2: Tuple) = 
        Tuple(t1.x - t2.x, t1.y - t2.y, t1.z - t2.z, t1.w - t2.w)
    static member (~-) (t : Tuple) =
        Tuple(-1.0 * t.x, -1.0 * t.y, -1.0*t.z, -1.0*t.w)
    static member (*) (t : Tuple, a) =
        Tuple(a * t.x, a * t.y, a * t.z, a * t.w)
    static member (*) (a, t: Tuple) =
        Tuple(a * t.x, a * t.y, a * t.z, a * t.w)
    static member (/) (t: Tuple, a) =
        Tuple(t.x / a, t.y / a, t.z / a, t.w / a)

    override this.ToString() =
        "(" + this.x.ToString() + ", " + this.y.ToString() + ", " + this.z.ToString() + ", " + this.w.ToString() + ")"
    override x.Equals(yobj) =
        match yobj with
        | :? Tuple as y -> (x.x = y.x && x.y = y.y && x.z = y.z && x.w = y.w)
        | _ -> false
    override t.GetHashCode() =
        (x, y, z, w).GetHashCode()


let is_point(t: Tuple) = t.w = 1
let is_vector(t: Tuple) = t.w = 0

let make_point x y z = Tuple(x, y, z, 1)
let make_vector x y z = Tuple(x, y, z, 0)

let magnitude (v: Tuple): double = 
    sqrt(v.x**2 + v.y**2 + v.z**2 + v.w**2)

let normalize (v: Tuple): Tuple = 
    let m = magnitude v
    v / m

let dot (u: Tuple) (v: Tuple): double = 
    u.x * v.x + u.y * v.y + u.z * v.z + u.w * v.w

let cross (u: Tuple) (v: Tuple): Tuple = 
    make_vector (u.y * v.z - u.z * v.y)
               (u.z * v.x - u.x * v.z)
               (u.x * v.y - u.y * v.x)
