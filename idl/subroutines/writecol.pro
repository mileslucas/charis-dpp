;+
; NAME:
; writecol
;  Version 1.1
;
; PURPOSE:
;    Prints a series of arrays to a file in ASCII format
;
; CALLING SEQUENCE:
;
;   writecol, filename, v1, v2, FMT=''
;
; INPUTS:
;   file     - Name of the ASCII file
;   v1       - Vector 1
;   v2       - Vector 2
;   [v3-v19]       - Vectors 3-14
;
; RETURNS:
;
; OUTPUTS:
;   Prints v1, v2 to screen
;
; OPTIONAL KEYWORDS:
;   FMT -  FORTRAN formatting
;   FILNUM - File number (as opposed to file)
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;   The program keys off the number of elements in v1
;
; EXAMPLES:
;   writecol, 'arrays.dat', array1, array2
;
;
; PROCEDURES CALLED:
;
; REVISION HISTORY:
;   17-June-2001 Written by JXP
;-
; ------------------------------------------------------------------------------
pro writecol, file, v1, v2, v3, v4, v5, v6, v7, v8, v9, $
  v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, $
  header = header, fmt = fmt, filnum = filnum
  compile_opt idl2

  ; writecol -- Writes a 2 column ascii file

  if (n_params() lt 2) then begin
    print, 'Syntax - ' + $
      'writecol, file, v1, v2, [v3-v19] FMT=, FILNUM= '
    return
  endif

  ;

  flgvn = n_params() - 1
  print, flgvn
  if not keyword_set(fmt) then flgfmt = 0 else begin
    flgfmt = 1
    fmt = fmt[0]
  endelse

  ;

  if not keyword_set(filnum) then begin
    filnum = 91
    close, filnum
    openw, filnum, file
    flg_fil = 91
  endif

  if keyword_set(header) then begin
    printf, filnum, header
  endif
  for i = 0l, n_elements(v1) - 1 do begin
    case flgvn of
      19: printf, filnum, format = fmt, v1[i], v2[i], v3[i], v4[i], v5[i], v6[i], v7[i], $
        v8[i], v9[i], v10[i], v11[i], v12[i], v13[i], v14[i], v15[i], $
        v16[i], v17[i], v18[i], v19[i]
      18: printf, filnum, format = fmt, v1[i], v2[i], v3[i], v4[i], v5[i], v6[i], v7[i], $
        v8[i], v9[i], v10[i], v11[i], v12[i], v13[i], v14[i], v15[i], $
        v16[i], v17[i], v18[i]
      17: printf, filnum, format = fmt, v1[i], v2[i], v3[i], v4[i], v5[i], v6[i], v7[i], $
        v8[i], v9[i], v10[i], v11[i], v12[i], v13[i], v14[i], v15[i], v16[i], v17[i]
      16: printf, filnum, format = fmt, v1[i], v2[i], v3[i], v4[i], v5[i], v6[i], v7[i], $
        v8[i], v9[i], v10[i], v11[i], v12[i], v13[i], v14[i], v15[i], v16[i]
      15: printf, filnum, format = fmt, v1[i], v2[i], v3[i], v4[i], v5[i], v6[i], v7[i], $
        v8[i], v9[i], v10[i], v11[i], v12[i], v13[i], v14[i], v15[i]
      14: printf, filnum, format = fmt, v1[i], v2[i], v3[i], v4[i], v5[i], v6[i], v7[i], $
        v8[i], v9[i], v10[i], v11[i], v12[i], v13[i], v14[i]
      13: printf, filnum, format = fmt, v1[i], v2[i], v3[i], v4[i], v5[i], v6[i], v7[i], $
        v8[i], v9[i], v10[i], v11[i], v12[i], v13[i]
      12: printf, filnum, format = fmt, v1[i], v2[i], v3[i], v4[i], v5[i], v6[i], v7[i], $
        v8[i], v9[i], v10[i], v11[i], v12[i]
      11: printf, filnum, format = fmt, v1[i], v2[i], v3[i], v4[i], v5[i], v6[i], v7[i], $
        v8[i], v9[i], v10[i], v11[i]
      10: printf, filnum, format = fmt, v1[i], v2[i], v3[i], v4[i], v5[i], v6[i], v7[i], $
        v8[i], v9[i], v10[i]
      9: printf, filnum, format = fmt, v1[i], v2[i], v3[i], v4[i], v5[i], v6[i], v7[i], $
        v8[i], v9[i]
      8: printf, filnum, format = fmt, v1[i], v2[i], v3[i], v4[i], v5[i], v6[i], v7[i], v8[i]
      7: printf, filnum, format = fmt, v1[i], v2[i], v3[i], v4[i], v5[i], v6[i], v7[i]
      6: printf, filnum, format = fmt, v1[i], v2[i], v3[i], v4[i], v5[i], v6[i]
      5: printf, filnum, format = fmt, v1[i], v2[i], v3[i], v4[i], v5[i]
      4: printf, filnum, format = fmt, v1[i], v2[i], v3[i], v4[i]
      3: printf, filnum, format = fmt, v1[i], v2[i], v3[i]
      2: printf, filnum, format = fmt, v1[i], v2[i]
      1: printf, filnum, format = fmt, v1[i]
      ; print,v1[i]
      else: stop
    endcase
  endfor
  if keyword_set(flg_fil) then close, filnum

  return
end