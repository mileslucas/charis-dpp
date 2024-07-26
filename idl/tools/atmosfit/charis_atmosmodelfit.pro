pro charis_atmosmodelfit, planetspec = planetspec, planetcovar = planetcovar, planetphot = planetphot, grid = grid, pick = pick, band = band, delay = delay, saveplot = saveplot, metals = metals, calcind = calcind, justind = justind, distance = distance, extendplot = extendplot
  compile_opt idl2

  ; Feb 11, 2021 - now includes variable bin widths
  ; (TO ADD: ability to simultaneously fit photometry and spectra)

  if ~keyword_set(distance) then distance = 10.0

  ; compares a CHARIS planet spectrum to a suite of atmospheric models

  ; CURRENTLY ONLY WORKS WITH LOW-RES DATA

  ; if you include the covariance, calculate it here once, so you do not have to recalculate it
  if keyword_set(planetcovar) then begin
    readcol, planetcovar, arho, alam, adel, sigrho, siglam, rho

    ; now populate the matrix C to get errors
  endif

  if keyword_set(saveplot) then begin
    file_mkdir, 'modelspec_out'
    plotdir = './modelspec_out/'
  endif

  ; initiate array of CHARIS data
  get_charis_wvlh, dum, wavelengths, manual = 'lowres'
  lambda = 1d-3 * wavelengths

  limit1 = 1d-1
  limit2 = 2.d-1

  ; now down-select the wavelengths to identify those that are not covered over the water bands.
  ; lmbad=[1400.,1900.17]*1d-3 ;wavelengths covering the water bands
  ; lmbad=[1374.62,1867.17]*1d-3 ;wavelengths covering the water bands
  ; bad=where(abs(lambda-lmbad[0]) lt limit1 or abs(lambda-lmbad[1]) lt limit2,nbad,complement=good)

  ; *****Read in Planet Spectrum*****
  ; ***assumes F_nu (mJy) units
  if ~keyword_set(planetspec) then begin
    planetspec = dialog_pickfile(title = 'Select Your CHARIS Planet Spectrum')
  endif
  readcol, planetspec, wavelength_planet, planet_flux, eplanet_flux
  skipoverme:

  ; ****Now, decision tree to determine whether this is lowres, J, H, or K...
  minwvlhp = 1d3 * min(wavelength_planet)
  maxwvlhp = 1d3 * max(wavelength_planet)
  if minwvlhp lt 1200 and maxwvlhp gt 2300 then filtername0 = 'lowres'
  if minwvlhp lt 1200 and maxwvlhp lt 1400 then filtername0 = 'J'
  if minwvlhp lt 1500 and minwvlhp gt 1400 and maxwvlhp lt 1850 then filtername0 = 'H'
  if minwvlhp gt 1850 then filtername0 = 'K'

  lambda = wavelength_planet
  charis_bandwidth = replicate(median(wavelength_planet - shift(wavelength_planet, 1), /even), n_elements(wavelength_planet))

  bad = where(lambda eq lambda[5] or lambda eq lambda[6] $
  or lambda eq lambda[14] or lambda eq lambda[15], nbad, complement = goodinit)

  ; bad=where(lambda eq lambda[5] or lambda eq lambda[6] or lambda eq lambda[7] $
  ; or lambda eq lambda[14] or lambda eq lambda[15] or lambda eq lambda[16],nbad,complement=goodinit)

  if ~keyword_set(band) then begin
    good = goodinit
  endif else begin
    ; further down-select by wavelength
    case band of
      'j': begin
        bad2 = where(lambda gt lambda[4], badband, complement = goodband)
      end
      'h': begin
        bad2 = where(lambda lt lambda[8] or lambda gt lambda[13], badband, complement = goodband)
      end
      'k': begin
        bad2 = where(lambda lt lambda[17], badband, complement = goodband)
      end
      'hk': begin
        bad2 = where(lambda lt lambda[8], badband, complement = goodband)
      end
      'jhk': begin
        bad2 = where(lambda lt lambda[1], badband, complement = goodband)
      end
    endcase
    good = intersect(goodband, goodinit)
  endelse

  ngood = n_elements(lambda[good])
  print, 'Wavelengths Considered', lambda[good], ' out of ', lambda
  print, 'Number of Wavelengths', n_elements(lambda[good])

  ; ***Calculate Spectral Covariance in each combination of i and j channels

  ; if you include the covariance, calculate it here once, so you do not have to recalculate it
  if keyword_set(planetcovar) then begin
    readcol, planetcovar, arho, alam, adel, sigrho, siglam, rho
    nwvlh = n_elements(lambda)
    speccovar = fltarr(nwvlh, nwvlh)
    psi = fltarr(nwvlh, nwvlh)
    lamarr = lambda
    lam_med = median(lamarr, /even)

    ; now populate the matrix C to get errors

    for i = 0l, 21 do begin
      for j = 0l, 21 do begin
        lamratio = (lamarr[i] - lamarr[j]) / lam_med
        if i ne j then begin
          psi[i, j] = arho * exp(-0.5 * (rho * lamratio / sigrho) ^ 2.) + $
            alam * exp(-0.5 * (lamratio / siglam) ^ 2.) + $
            0 * adel
        endif else begin
          psi[i, j] = arho + alam + adel
        endelse

        speccovar[i, j] = (psi[i, j] * sqrt(eplanet_flux[i] ^ 2. * eplanet_flux[j] ^ 2.)) ; spectral covariance
      endfor
    endfor
  endif

  ; ****** Photometry *****
  ; in case you have complementary photometry

  ; Format:
  ; Filter name (string), Fnu, delta(Fnu)
  nphot = 0
  if keyword_set(planetphot) then begin
    readcol, planetphot, photfiltname, photwvlh, photflux, ephotflux, format = '(a,f,f,f)'
    nphot = n_elements(photflux)
    ; now, get bandwidth
    photbandwidth = charis_get_phot_bandwidth(photfiltname)
  endif

  ; ******

  ; **********ATMOSPHERE MODEL GRID*******

  if ~keyword_set(grid) then begin
    grid = 'btsettl'
  endif

  ; giant case conditional

  case grid of
    'btsettl': begin
      ; these models have are from the online substellar atmosphere model server:
      ; http://svo2.cab.inta-csic.es/theory/newov2/
      ; format lte[temp]-[logg]-[metallicity].BT-Sett.7.dat.txt
      ; e.g lte020-4.0-0.0.BT-Settl.7.dat.txt  = 2000K, log(g)=4, solar metallicity

      modeldir = charis_path(pathname = 'btsettldir')
      dirname = 'btsettl_fit'

      if ~keyword_set(pick) then begin
        modelfullnames = file_search(modeldir + 'lte*')
      endif else begin
        modelfullnames = dialog_pickfile(title = 'select models', path = modeldir, /multiple_files)
      endelse
      yourpath = strpos(modelfullnames[0], '/', /reverse_search) + 1
      ; getting the model name after pulling off the path
      modelnames = reform((strmid(modelfullnames, yourpath, strlen(modelfullnames) - 1))[0, *])

      ; now, figure out the temperatures used
      temps = long((strmid(modelnames, 3, 3)) * 100.)
      logg = float((strmid(modelnames, 7, 3)))

      if keyword_set(metals) then begin
        metals = float((strmid(modelnames, 10, 4)))
      endif
      print, temps
      print, logg
    end

    ; ***add other model grids here.  Format MUST be the same as BTSettl format
    ; '[name of your grid']: begin
    ;
    ; end
  endcase
  file_mkdir, dirname

  nmodels = n_elements(modelnames)

  if ~keyword_set(pick) then begin
    openw, 2, dirname + '/' + 'fitoutcome.txt'
  endif else begin
    openw, 2, dirname + '/' + 'fitoutcomepick.txt'
  endelse

  ; *****Read in String of Spectral Library Collection*****

  for ispec = 0l, nmodels - 1 do begin
    ; read in the spectrum
    ; readcol,modeldir+'lte032-5.0-0.0a+0.0.BT-NextGen.7.dat.txt',wvlhmodel,fmodel
    readcol, modelfullnames[ispec], wvlhmodel, fmodel
    wvlhmodel *= 1e-4

    ; cut the model to focus only on relevant wavelengths
    cut = where(wvlhmodel gt 0.9 and wvlhmodel lt 6)
    wvlhmodel = wvlhmodel[cut]
    fmodel = fmodel[cut]

    fmodel = fmodel[sort(wvlhmodel)]
    wvlhmodel = wvlhmodel[sort(wvlhmodel)]
    print, 'modelnames[ispec]', modelnames[ispec]

    outputspec_pred = charis_convert_planetspec_to_lowres_calculation2(fmodel, wvlhmodel, filtname = filtername0)

    ; works
    if nphot gt 0 then begin
      outputphot_pred = fltarr(nphot)
      for jphot = 0l, nphot - 1 do begin
        print, photfiltname[jphot]
        outputphot_pred[jphot] = charis_convert_planetphot_to_lowres_calculation(fmodel, wvlhmodel, filtname = photfiltname[jphot])
      endfor
    endif

    ; print,outputphot_pred
    ; print,'xxxx'
    ; print,outputspec_pred
    ; print,'yyyy'

    setcolors, /system_variables
    plot, wvlhmodel, smooth(fmodel, 100), xrange = [1, 5], /ylog
    oplot, lambda, outputspec_pred, psym = 4, symsize = 5, color = !green
    if nphot gt 0 then $
      oplot, photwvlh, outputphot_pred, psym = 2, symsize = 6, color = !magenta

    ; now set the distance to be distance, and scale by Jupiter's radius

    ; 1 Rj/1 pc = 2.265d-9

    sfac = 1.0
    ; sfac=5.5
    outputspec_pred *= (sfac * 2.265d-9 / distance) ^ 2.
    inputspec = fmodel * (sfac * 2.265d-9 / distance) ^ 2.

    ; convert from Flam to Fnu for model spectrum
    ; ****for charis-converted model
    clight = 2.99792458d14
    conv_fact = ((wavelength_planet[*] ^ 2) / clight) ; this is in um*s
    conv_fact *= 1e4 ; now in A*s
    conv_fact *= (1.0 / 10.0 ^ (-23.0))
    conv_fact *= 1d3 ; from Jy to mJy
    outputspec_pred *= conv_fact

    ; ****for raw model
    conv_factinput = ((wvlhmodel[*] ^ 2) / clight) ; this is in um*s
    conv_factinput *= 1e4 ; now in A*s
    conv_factinput *= (1.0 / 10.0 ^ (-23.0))
    conv_factinput *= 1d3 ; from Jy to mJy
    inputspec *= conv_factinput
    ; ****

    ; **** for photometry
    if nphot gt 0 then begin
      outputphot_pred *= (sfac * 2.265d-9 / distance) ^ 2.
      conv_factphot = (photwvlh[*] ^ 2 / clight)
      conv_factphot *= 1e4
      conv_factphot *= (1.0 / 10.0 ^ (-23.0))
      conv_factphot *= 1d3 ; from Jy to mJy
      ; print,'gggg',outputphot_pred,conv_factphot
      ; print,'stop'
      outputphot_pred *= conv_factphot
    endif

    ; ****

    goto, skipoverthisplot1
    setcolors, /system_variables
    ; window,1
    set_plot, 'x'
    plot, wvlhmodel, smooth(inputspec, 100), xrange = [1, 2.5]
    rplot, wavelength_planet, outputspec_pred, psym = 4, symsize = 5, color = !green
    plotsym, 0, /fill
    oploterror, wavelength_planet, planet_flux, eplanet_flux, psym = 8, symsize = 3, color = !blue
    stop
    skipoverthisplot1:

    goto, skipoverthisplot
    ; window,3
    ; set_plot,'x'
    ; setcolors,/system_variables,/silent

    ; plot,wavelength_planet,outputspec_pred,xrange=[1,2.5],linestyle=0,/nodata,yrange=[min(outputspec_pred),max(outputspec_pred)],ystyle=1,xstyle=1
    ; oplot,wavelength_planet,outputspec_pred,linestyle=0,color=!blue,psym=-4,thick=3
    ; al_legend,fname,/bottom,charsize=1.25,box=0
    skipoverthisplot:

    estmed = total(outputspec_pred[good] * planet_flux[good] / (eplanet_flux[good] ^ 2.)) / total(outputspec_pred[good] ^ 2. / (eplanet_flux[good] ^ 2.))

    ; outputerrspec_pred[*]=outputspec_pred*0.01

    if ~keyword_set(planetcovar) then begin
      ; chisqcalc,obs=planet_flux[good],eobs=eplanet_flux[good],model_in=outputspec_pred[good],err_model_in=outputspec_pred[good]*0,alpha_in=estmed,chisqout=chisqout,ndof=ndof,alpha_out=alpha_out

      if ~keyword_set(planetphot) then begin
        chisqcalc, obs = planet_flux[good], eobs = eplanet_flux[good], model_in = outputspec_pred[good], err_model_in = outputspec_pred[good] * 0, alpha_in = estmed, chisqout = chisqout, ndof = ndof, alpha_out = alpha_out
      endif else begin
        chisqcalc, obs = planet_flux[good], eobs = eplanet_flux[good], model_in = outputspec_pred[good], err_model_in = outputspec_pred[good] * 0, alpha_in = estmed, chisqout = chisqout, ndof = ndof, alpha_out = alpha_out, $
          phot = photflux, ephot = ephotflux, modelphot = outputphot_pred
      endelse
    endif else begin
      ; chisqcalcwcovar,obs=planet_flux,eobs=eplanet_flux,model_in=outputspec_pred,err_model_in=outputspec_pred*0,alpha_in=estmed,chisqout=chisqout,ndof=ndof,alpha_out=alpha_out,speccovar=speccovar,good=good

      if ~keyword_set(planetphot) then begin
        chisqcalcwcovar, obs = planet_flux, eobs = eplanet_flux, model_in = outputspec_pred, err_model_in = outputspec_pred * 0, alpha_in = estmed, chisqout = chisqout, ndof = ndof, alpha_out = alpha_out, speccovar = speccovar, good = good
      endif else begin
        chisqcalcwcovar, obs = planet_flux, eobs = eplanet_flux, model_in = outputspec_pred, err_model_in = outputspec_pred * 0, alpha_in = estmed, chisqout = chisqout, ndof = ndof, alpha_out = alpha_out, speccovar = speccovar, good = good, $
          phot = photflux, ephot = ephotflux, modelphot = outputphot_pred
      endelse
    endelse

    print, 'Fit Output is ...', modelnames[ispec], ' ', chisqout / float(ngood + nphot - 1), estmed, alpha_out

    ; do you do the index calculations or not?

    ; if ~keyword_set(calcind) then begin
    ; if nbaderror gt 0 then begin
    ; printf,2,fname,mont_spt[ispec],mont_gravclass[ispec],-999,format='(a,1x,a,1x,i3,1x,f8.3)'
    ; endif else begin

    ; note: alpha_out is a linear scaling to minimize chi^2.   But what is actually being scaled is the radius-squared.  so sqrt(alpha_out) = multiplicative factor for the radius in Jupiter radii.

    if ~keyword_set(metals) then begin
      printf, 2, modelnames[ispec], temps[ispec], logg[ispec], sqrt(alpha_out), chisqout, chisqout / float(ngood + nphot - 1), format = '(a,1x,f8.1,1x,f4.1,1x,f8.3,1x,f8.3,1x,f8.3)'
    endif else begin
      printf, 2, modelnames[ispec], temps[ispec], logg[ispec], metals[ispec], sqrt(alpha_out), chisqout, chisqout / float(ngood + nphot - 1), format = '(a,1x,f8.1,1x,f4.1,1x,f5.2,1x,f8.3,1x,f8.3,1x,f8.3)'
    endelse

    ; endelse

    outputspec_pred *= alpha_out
    ; outputerrspec_pred*=alpha_out
    inputspec *= alpha_out
    ; inputespec*=alpha_out

    ; photometry
    if nphot gt 0 then begin
      outputphot_pred *= alpha_out
    endif

    ; print,alpha_out
    ; print,''
    ; print,outputspec_pred
    ; print,''
    ; print,outputphot_pred
    ; stop

    ; ****Now report and plot the output

    if nphot gt 0 then begin
      mincov = min(outputspec_pred) < min(outputphot_pred)
      mincov = mincov < 0.9 * min(planet_flux - eplanet_flux)

      maxcov = max(outputspec_pred) > max(outputphot_pred)
      maxcov = maxcov > 1.1 * max(planet_flux + eplanet_flux)
    endif else begin
      mincov = min(outputspec_pred)
      maxcov = max(outputspec_pred)
    endelse

    ; window,4
    set_plot, 'x'
    window, 1
    setcolors, /system_variables, /silent
    if ~keyword_set(extendplot) then begin
      plot, wavelength_planet, outputspec_pred, xrange = [1, 2.5], linestyle = 0, /nodata, yrange = [0.9 * min(outputspec_pred), 1.1 * max(outputspec_pred)], ystyle = 1, xstyle = 1
    endif else begin
      plot, wavelength_planet, outputspec_pred, xrange = [0.9, 4], linestyle = 0, /nodata, yrange = [0. * min(outputspec_pred), 1.2 * max(outputspec_pred)], ystyle = 1, xstyle = 1
    endelse
    ; plot,wavelength_planet,outputspec_pred,xrange=[1,2.5],linestyle=0,/nodata,yrange=[min(outputspec_pred),max(outputspec_pred)],ystyle=1,xstyle=1
    ; oplot,wavelength_planet,alpha_out*outputspec_pred,linestyle=0,color=!blue,psym=-4,thick=3
    ; ****the raw spectrum
    oplot, wvlhmodel, smooth(inputspec, 1000), linestyle = 1, thick = 2, color = !yellow
    ; ****the charis-converted spectrum
    oplot, wavelength_planet, outputspec_pred, linestyle = 0, color = !blue, psym = -4, thick = 3
    ; **** the planet spectrum
    oploterror, wavelength_planet, planet_flux, eplanet_flux, color = !green, psym = -3, thick = 2

    ; ***model photometry, source photometry
    if nphot gt 0 then begin
      setcolors, /system_variables, /silent
      oplot, [photwvlh], [outputphot_pred], color = !magenta, psym = 1, symsize = 5
      plotsym, 0, /fill
      setcolors, /system_variables, /silent
      oploterror, [photwvlh], [photflux], [ephotflux], color = !cyan, psym = 4, symsize = 3
    endif

    al_legend, modelnames[ispec], /bottom, charsize = 1.25, box = 0

    if keyword_set(saveplot) then begin
      set_plot, 'ps'
      !p.font = 1
      device, filename = plotdir + modelnames[ispec] + '.eps', /encapsulated, bits = 8, /color
      setcolors, /system_variables, /silent
      if ~keyword_set(extendplot) then begin
        plot, wavelength_planet, outputspec_pred, xrange = [1, 2.5], linestyle = 0, /nodata, yrange = [0.9 * min(outputspec_pred), 1.1 * max(outputspec_pred)], ystyle = 1, xstyle = 1, $
          xthick = 5, ythick = 5, charsize = 1.25, xtitle = 'Wavelength (Microns)', ytitle = 'Flux Density (mJy)'
      endif else begin
        ; plot,wavelength_planet,outputspec_pred,xrange=[0.9,4],linestyle=0,/nodata,yrange=[0.*min(outputspec_pred),1.2*max(outputspec_pred)],ystyle=1,xstyle=1,$
        plot, wavelength_planet, outputspec_pred, xrange = [0.9, 4], linestyle = 0, /nodata, yrange = [0. * min(outputspec_pred), 1.2 * maxcov], ystyle = 1, xstyle = 1, $
          xthick = 5, ythick = 5, charsize = 1.25, xtitle = 'Wavelength (Microns)', ytitle = 'Flux Density (mJy)'
      endelse
      ; plot,wavelength_planet,outputspec_pred,xrange=[1,2.5],linestyle=0,/nodata,yrange=[min(outputspec_pred),max(outputspec_pred)],ystyle=1,xstyle=1
      ; oplot,wavelength_planet,alpha_out*outputspec_pred,linestyle=0,color=!blue,psym=-4,thick=3
      ; ****the raw spectrum
      setcolors, /system_variables, /silent

      ; ***model photometry, source photometry
      if nphot gt 0 then begin
        setcolors, /system_variables, /silent
        oplot, [photwvlh], [outputphot_pred], color = !forest, psym = 1, thick = 10, symsize = 5
        plotsym, 0, /fill
        setcolors, /system_variables, /silent
        oploterror, [photwvlh], [photflux], [ephotflux], color = !cyan, psym = 4, thick = 10, symsize = 3
      endif

      ; oplot,wvlhmodel,smooth(inputspec,1000)*alpha_out,linestyle=1,thick=20,color=!gray
      ; ;****the raw spectrum
      ; oplot,inputwvlh,inputspec,linestyle=1,thick=3
      ; ****the charis-converted spectrum
      setcolors, /system_variables, /silent
      oplot, wavelength_planet, outputspec_pred, linestyle = 0, color = !green, psym = -4, thick = 15
      ; oplot,wavelength_planet,outputspec_pred,linestyle=0,psym=-4,thick=10
      ; **** the planet spectrum
      oploterror, wavelength_planet, planet_flux, eplanet_flux, color = !blue, psym = -3, thick = 8, errthick = 3

      if ~keyword_set(extendplot) then begin
        al_legend, modelnames[ispec], charsize = 0.75, charthick = 2, box = 0, position = [1.7, min(outputspec_pred)]
      endif else begin
        al_legend, modelnames[ispec], charsize = 1.25, charthick = 2, box = 0, position = [min(wavelength_planet), 0.5 * mincov]
      endelse
      al_legend, Textoidl('\chi^2 =') + string(sigfig(chisqout / (ngood + nphot - 1), 4)), /right, box = 0, charsize = 1.2, charthick = 3
      device, /close
    endif

    if keyword_set(delay) then wait, 2
  endfor
  close, /all
end