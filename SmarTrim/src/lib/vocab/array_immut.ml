open Array

type 'a t = 'a array

let length = length
let get = get
let make = make
let init = init
let append = append
let concat = concat
let sub = sub
let copy = copy
let to_list = to_list
let of_list = of_list
let iter = iter
let iteri = iteri
let map = map
let mapi = mapi
let unsafe_get = unsafe_get
let of_array (a : 'a array) = copy a
