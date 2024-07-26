pro chisqcalcwcovarwbin, obs = obs, eobs = eobs, widthobs = widthobs, model_in = model_in, err_model_in = err_model_in, alpha_in = alpha_in, speccovar = speccovar, $
  chisqout = chisqout, ndof = ndof, alpha_out = alpha_out, good = good, phot = phot, ephot = ephot, widthphot = widthphot
  compile_opt idl2

  ; Feb 11, 2021- simple chi-squared with covariance calculations but with bin widths

  ; July 7, 2018 - simple chi-squared with covariance calculations

  ; ****hardwired for CHARIS low-res for now
  ; initiate array of CHARIS data
  ; get_charis_wvlh,dum,wavelengths,manual='lowres'
  ; lamarr=1.d-3*wavelengths
  ; lam_med=median(lamarr,/even)

  ; number of flux measurements
  n_measure = n_elements(obs)
  ndof = n_measure

  ; ****covariance
  ; ncovar=fltarr(n_measure,n_measure)
  ; cmaterro=fltarr(22,22)
  dimcovar = (size(speccovar, /dim))[0] ; assume square arrays

  ; if the bin width is not set, then assume it is 1
  if ~keyword_set(widthobs) then begin
    widthobs = obs * 0 + 1.
  endif

  ; ;*****

  chisqtot = 1d30

  sfac = (findgen(2d4) * 5d-3 + 5d-3) * alpha_in
  ; sfac=(findgen(1d5)*1d-3+1d-2)*alpha_in
  nsfac = n_elements(sfac)
  sfac_tot = -99

  for i = 0l, nsfac - 1 do begin
    cmaterr = speccovar

    diff = obs - sfac[i] * model_in

    ; ****adds model errors, sets diff = 0 for array elements not considered in fitting (i.e. array elements =/ good)
    for j = 0l, dimcovar - 1 do begin
      ; add model error
      cmaterr[j, j] += (sfac[i] ^ 2. * err_model_in[j] ^ 2.)

      ; down-select for channels used in fitting
      q = intersect(j, good)

      if q eq -1 then begin
        diff[j] = 0
      endif
    endfor

    ; skipoverme:

    ; now, fold in the variable widths
    diff *= sqrt(widthobs)

    ; gval=transpose(diff)#invert(cmaterr,/double)#diff

    gval = transpose(diff) # invert(cmaterr, /double) # diff

    gval /= float(total(widthobs))

    if gval lt chisqtot then begin
      chisqtot = gval
      sfac_tot = sfac[i]
    endif
  endfor

  alpha_out = sfac_tot
  chisqout = chisqtot

  ; now do test of the output
  ; diff=obs-sfac_tot*model_in
  ; diff=obs-sfac_tot*model_in
  ; gg=total((diff[good]/eobs[good])^2.)
  ; print,gg,chisqtot
  ; stop
end