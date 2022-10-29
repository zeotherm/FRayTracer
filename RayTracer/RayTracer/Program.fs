
module RayTracer

let EPSILON = 0.00001

let approx a b = if abs(a - b) < EPSILON then true else false

type Tuple(x: double, y: double, z: double, w: int) =
    member this.x = x
    member this.y = y
    member this.z = z
    member this.w = w

    static member (+) (t1: Tuple, t2: Tuple) = 
        Tuple(t1.x + t2.x, t1.y + t2.y, t1.z + t2.z, t1.w + t2.w)
    static member (-) (t1: Tuple, t2: Tuple) = 
        Tuple(t1.x - t2.x, t1.y - t2.y, t1.z - t2.z, t1.w - t2.w)
    static member (~-) (t : Tuple) =
        Tuple(-1.0 * t.x, -1.0 * t.y, -1.0*t.z, t.w)
    static member (*) (t : Tuple, a) =
        Tuple(a * t.x, a * t.y, a * t.z, t.w)
    static member (*) (a, t: Tuple) =
        Tuple(a * t.x, a * t.y, a * t.z, t.w)
    
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

let t = Tuple(1.0, 2.0, 3.0, 0)
