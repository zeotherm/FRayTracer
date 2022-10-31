module Color

type Color(r: double, g: double, b: double) =
    member this.red = r
    member this.green = g
    member this.blue = b

    static member (+) (c1: Color, c2: Color) = 
        Color(c1.red + c2.red, c1.green + c2.green, c1.blue + c2.blue)
    static member (-) (c1: Color, c2: Color) = 
        Color(c1.red - c2.red, c1.green - c2.green, c1.blue - c2.blue)
    static member (*) (c : Color, a) =
        Color(a * c.red, a * c.green, a * c.blue)
    static member (*) (a, c: Color) =
        Color(a * c.red, a * c.green, a * c.blue)
    static member (*) (c1: Color, c2: Color) = // Hadamard Product
        Color(c1.red * c2.red, c1.green * c2.green, c1.blue * c2.blue)
    static member (/) (c: Color, a) =
        Color(c.red / a, c.green / a, c.blue / a)
    override this.ToString() =
        "RGB(" + this.red.ToString() + ", " + this.green.ToString() + ", " + this.blue.ToString() + ")"
    override x.Equals(yobj) =
        match yobj with
        | :? Color as y -> (x.red = y.red && x.green = y.green && x.blue = y.blue)
        | _ -> false
    override t.GetHashCode() =
        (r, g, b).GetHashCode()
