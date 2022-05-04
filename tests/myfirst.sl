(use "src/std")

(fn add1 (n) (std.+ n 1))


(fn main ()
  (add1 (add1 3)))
