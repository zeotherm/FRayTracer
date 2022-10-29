
module RayTracer
open Tuples

let EPSILON = 0.00001

let approx a b = if abs(a - b) < EPSILON then true else false

