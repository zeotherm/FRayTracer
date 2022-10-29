module Tuples

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


let isPoint(t: Tuple) = t.w = 1
let isVector(t: Tuple) = t.w = 0

let makePoint x y z = Tuple(x, y, z, 1)
let makeVector x y z = Tuple(x, y, z, 0)

let magnitude (v: Tuple): double = 
    sqrt(v.x**2 + v.y**2 + v.z**2 + v.w**2)

let normalize (v: Tuple): Tuple = 
    let m = magnitude v
    v / m

let dot (u: Tuple) (v: Tuple): double = 
    u.x * v.x + u.y * v.y + u.z * v.z + u.w * v.w

let cross (a: Tuple) (b: Tuple): Tuple = 
    makeVector (a.y * b.z - a.z * b.y)
               (a.z * b.x - a.x * b.z)
               (a.x * b.y - a.y * b.x)

let t = Tuple(1.0, 2.0, 3.0, 0)
