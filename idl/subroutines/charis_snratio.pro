pro charis_snratio, cube, coord = coord, expfilt = expfilt, rmax = rmax, exc = exc, filter = filter, fixpos = fixpos, zero = zero, $
  roimask = roimask, $
  colimage = colimage
  compile_opt idl2
  ; computes a SNR map of a CHARIS wavelength-collapsed and each wavelength slice of a data cube

  ; Input:
  ; cube - the name of the data cube for which you want to compute SNR
  ; (collapsed image)

  ; Output:
  ; snrmap_col.fits - the SNR map of the wavelength-collapsed image
  ; snrmap_cube.fits - the SNR map of the data cube

  ; Free parameters/Switches
  ; colimage - overrides the name of the wavelength-collapsed image
  ; coord - perform a SNR estimate of a given position in the cube/image
  ; fixpos - 'fix' this position: e.g. do not try to get a centroid estimate of the position
  ; zero, filter - subtract the median count value in the image before computing sigma; spatially filter the image before computing sigma
  ; expfilt - increase the box size for the spatial filter

  ; rmax = set the maximum outer radius for computation
  ; exc = set the exclusion region masked in the SNR calculation for a given position

  ; SNR is computed ...
  ; -1. blindly
  ; OR
  ; -2. avoiding regions around a point source if "coord" switch is set
  ; note that not using 'alt' produces potentially skewed results with any non-zero value of exc

  ; now, take cube name to do
  cubename = cube

  if ~keyword_set(colimage) then begin
    collapsedname = (strsplit(cubename, '.', /extract))[0] + '_collapsed.fits'
  endif else begin
    collapsedname = colimage
  endelse

  ; now, pull the fits header to get information about this cube
  h0 = headfits(cubename)
  h1 = headfits(cubename, ext = 1)

  ; wavelength array
  get_charis_wvlh, h0, wvlh_charis
  lambda = 1d-3 * wvlh_charis

  ; pixel scale
  pixscale = charis_get_constant(name = 'pixscale')

  ; effective telescope pupil size
  Dtel = charis_get_constant(name = 'Dtel')

  ; FWHM in each slice; FWHM in the collapsed cube
  fwhmarray = 1. * (1.d-6 * lambda / Dtel) * (180. * 3600. / !dpi) / pixscale
  fwhmcollapsed = median(fwhmarray, /even)

  if keyword_set(coord) then begin
    xc = coord[0]
    yc = coord[1]
  endif

  if ~keyword_set(rmax) then rmax = 70.
  if ~keyword_set(exc) then exc = 1.5

  ; Data Cube; Collapsed Data Cube
  imcube = readfits(cubename, ext = 1)
  imcol = readfits(collapsedname, ext = 1)

  ; expfilt=1
  ; expfilt0=3
  ; if keyword_set(expfilt) then expfilt=expfilt0

  s = size(imcol)
  dimx = s[1]
  dimy = s[2]
  xcen = dimx / 2
  ycen = dimy / 2

  snrmap_collapsed = fltarr(dimx, dimy)
  snrmap_cube = fltarr(dimx, dimy, n_elements(fwhmarray))

  ; Now do SNR for collapsed cube

  print, 'Calculating SNR Map for Wavelength-Collapsed Image'
  ; print,fwhmcollapsed

  charis_snratio_sub, imcol, coord = coord, fwhm = fwhmcollapsed, rmax = rmax, snrval = snrvalcol, expfilt = expfilt, $
    exc = exc, filter = filter, fixpos = fixpos, zero = zero, /finite, snrmap = snrmapo, noisemap = noisemap, roimask = roimask, /silent

  snrmap_collapsed = snrmapo

  if keyword_set(coord) then begin
    print, 'SNR in Collapsed Image is ', snrvalcol
  endif

  writefits, 'snrmap_col.fits', snrmap_collapsed, h1
  ; Now do SNR for cube
  for i = 0l, n_elements(wvlh_charis) - 1 do begin
    imslice = imcube[*, *, i]
    ; if keyword_set(filter) then imslice-=filter_image(imslice,median=5*expfilt*fwhmarray[i])

    ;
    print, 'Calculating SNR Map for Slice ', strtrim(string(i + 1), 2), '/', strtrim(string(n_elements(wvlh_charis)), 2)

    charis_snratio_sub, imcube[*, *, i], coord = coord, fwhm = fwhmarray[i], rmax = rmax, snrval = snrval, expfilt = expfilt, $
      exc = exc, filter = filter, fixpos = fixpos, zero = zero, /finite, snrmap = snrmapo, noisemap = noisemap, roimask = roimask, /silent

    snrmap_cube[*, *, i] = snrmapo
    if keyword_set(coord) then begin
      print, 'SNR in Slice ', strtrim(string(i + 1), 2), '/', strtrim(string(n_elements(wvlh_charis)), 2), ' is ', snrval
    endif
  endfor

  ; now, write the output

  writefits, 'snrmap_cube.fits', snrmap_cube, h1
end