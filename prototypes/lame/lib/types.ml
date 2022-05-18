open Base

type name = String.t

and env = value Map.M(String).t

and literal = int

and value =
| VUnit   of v_unit
| VInt    of v_int
| VEither of v_either
| VPair   of v_pair
| VFun    of v_fun

and v_unit = unit

and v_int = int

and v_either =
| VTrue   of value
| VFalse  of value

and v_pair = value * value

and v_fun = name * expr * env

and expr

let add_int (a: v_int) (b: v_int): v_int = a + b

