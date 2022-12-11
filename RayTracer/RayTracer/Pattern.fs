module Pattern
open Color
open Tuples
open Matrix

type PatternType = 
    | Stripes of Color * Color
    | Gradient of Color * Color
    | Rings of Color * Color
    | Checkers of Color * Color
    | Solid of Color
    | Default

type Pattern = PatternType * double[,]
let extract_patt_type (p: Pattern): PatternType = 
    let (pt, _) = p
    pt
let extract_patt_transform (p: Pattern): double[,] = 
    let (_, xf) = p
    xf
let set_patt_transform t (p: Pattern): Pattern = 
    (extract_patt_type p, t)

let make_pattern t = 
    let ident = make_ident_mat 4
    match t with 
    | Stripes(a, b) -> (Stripes (a, b), ident)
    | Gradient(a, b) -> (Gradient (a, b), ident)
    | Rings(a, b) -> (Rings (a, b), ident)
    | Checkers(a, b) -> (Checkers(a, b), ident)
    | Solid(a) -> (Solid(a), ident)
    | Default -> (Default, ident)

let make_stripes ca cb = make_pattern (Stripes (ca, cb))
let make_default_patt () = (Default, make_ident_mat 4)

let pattern_at patt (pt: Tuple): Color = 
    match extract_patt_type patt with 
    | Stripes(c_a, c_b) -> if int(floor pt.x) % 2 = 0 then c_a else c_b
    | Gradient(c_a, c_b) -> let dist = c_b - c_a
                            let frac = pt.x - floor(pt.x)
                            c_a + frac * dist
    | Rings(c_a, c_b) -> if int(floor(sqrt(pt.x*pt.x + pt.z*pt.z))) % 2 = 0 then c_a else c_b
    | Checkers(c_a, c_b) -> if int(floor(pt.x) + floor(pt.y) + floor(pt.z)) % 2 = 0 then c_a else c_b
    | Solid(c) -> c
    | Default -> Color(pt.x, pt.y, pt.z)
        

