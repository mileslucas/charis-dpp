function nbr2txt, n, ndigit
  compile_opt idl2

  ; retourne un nombre en format string complete par la gauche
  ; avec des 0 a un nombre de caracteres donne
  ;
  ; returns a format string complete the left
  ; 0 with a given number of characters

  ; n=nombre a retourner
  ; n=number to return
  ; ndigit=nombre de caratere du string
  ; ndigit=number of characters of the string

  nn = n_elements(n)

  textnumber = strmid('0000000000000', 0, ndigit)
  if nn gt 1 then textnumber = replicate(textnumber, nn)

  for i = 0, nn - 1 do begin
    tmp = textnumber[i]
    strput, tmp, strtrim(n[i], 2), ndigit - 1 - fix(alog10(n[i]))
    textnumber[i] = tmp
  endfor

  return, textnumber
end