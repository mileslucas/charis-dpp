function bdpolyfit, x, p
  compile_opt idl2
  ymod = p[0] + p[1] * x + p[2] * x ^ 2. + p[3] * x ^ 3.
  return, ymod
end