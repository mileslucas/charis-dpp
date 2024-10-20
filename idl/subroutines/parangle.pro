function parangle, ha, dec, latitude, degree = degree
  compile_opt idl2

  ;+
  ; NAME:
  ;      PARANGLE
  ;
  ; PURPOSE:
  ;      Return the parallactic angle of a source in degrees.
  ;
  ; CALLING SEQUENCE:
  ;      Result = PARANGLE(HA, DEC, LATITUDE [,/DEGREE])
  ;
  ; INPUTS:
  ;      HA - the hour angle of the source in decimal hours, unless /DEGREE
  ;           keyword is set, in which case input is assumed to be in decimal
  ;           degrees; a scalar or vector.
  ;      DEC - the declination of the source in decimal degrees; a scalar or
  ;            vector.
  ;      LATITUDE - The latitude of the telescope; a scalar.
  ;
  ; KEYWORD PARAMETERS:
  ;      /DEGREE - If set, then the HA parameter has been input in degrees
  ;                rather than hours.
  ;
  ; OUTPUTS:
  ;      Function returns a double real scalar or vector.  The parallactic
  ;      angle is returned in the range [-180,180] degrees.  If either ha or
  ;      dec is input as a scalar and the other input as a vector, the result
  ;      will be returned as a vector.  If both are input as vectors, it is
  ;      expected that the vector have the same number of elements.  If the
  ;      vectors are not the same length, then excess elements of the longer
  ;      one will be ignored
  ;
  ; COMMON BLOCKS:
  ;      None.
  ;
  ; RESTRICTIONS:
  ;      For measurements made at the poles, it is imperative that the
  ;      parallactic angle be calculated from native geocentric apparent
  ;      coordinates (HA,dec); being a singularity in horizon coordinates
  ;      (alt,az), if you transform to (HA,dec) you lose all HA information
  ;      and the parallactic angle becomes singular.  At the NCP the PA is
  ;      (HA-180) and at the SCP it is equivalent to the HA.
  ;
  ; EXAMPLE:
  ;      The parallactic angle at the north celestial pole should be
  ;      equivalent to (HA-180)...
  ;      IDL> ha = dindgen(360*2)/2.-180d0
  ;      IDL> dec = dblarr(360*2)+90d0
  ;      IDL> plot, ha, parangle(ha,dec,latitude,/DEG), PS=3
  ;
  ;      If you have a set of sexigesimal (RA,dec) coordinates, you'll
  ;      need to first convert them to decimal and calculate the Hour
  ;      Angle (HA=LST-RA):
  ;      IDL> ra = [12,23,34.5] & dec = [-4,12,45.6] & lst = 14.34
  ;      IDL> pa = parangle(lst-ten(ra),ten(dec),latitude)
  ;
  ;      If you have a set of coordinates in horizon coordinates
  ;      (alt-az) and you'd like to use this routine, just convert to
  ;      geocentric apparent (HA-dec) using the Goddard routine ALTAZ2HADEC:
  ;      IDL> altaz2hadec, el, az, latitude, ha, dec
  ;      IDL> pa = parangle(ha,dec,latitude)
  ;
  ; NOTES:
  ;      The parallactic angle at a point in the sky is the position angle of
  ;      the vertical, i.e., the angle between the direction to the North
  ;      Celestial Pole and to the zenith. In precise applications care must
  ;      be taken only to use geocentric apparent coordinates (HA,DEC). It is
  ;      measured from North through East and is always negative when the
  ;      source is in the East and positive when in the West.
  ;
  ;      The following references are very informative:
  ;      van der Kamp, 1967, "Principles of Astrometry" p. 22
  ;      Green, 1985, "Spherical Astronomy" p. 12
  ;      Thompson, Moran, Swenson, 2001, "Interferometry and Synthesis
  ;      in Radio Astronomy" p. 97
  ;
  ;      It should be noted that the Goddard IDL Astronomy library has a
  ;      procedure named POSANG that is capable of calculating the
  ;      parallactic angle and it even uses the same four-parts formula.  The
  ;      parallactic angle would be calculated by finding the angle between
  ;      the source position (ha,dec) and the pole position (0,latitude).
  ;      POSANG makes its calculations in equatorial coordinates so we need
  ;      to pass in -ha:
  ;      IDL> posang, 1, -ha, dec, 0d0, latitude, pa
  ;      Also, POSANG takes the difference between the two positions
  ;      introducing a tiny roundoff error into the calculation.
  ;
  ; MODIFICATION HISTORY:
  ;	Written by Tim Robishaw, Berkeley  08 May 2006
  ;       D.L., March 12, 2009 -  modified it such that it always
  ;                              returns a continuous angle across HA=0.
  ;       08.14.2013 - ds - Fixed so it actually works on arrays.
  ;-

  if (n_params() lt 3) then begin
    message, 'Syntax - Result = parangle(ha, dec, latitude [, /DEGREE])', /info
    return, 0
  endif

  ; ;check inputs
  if n_elements(latitude) gt 1 then message, 'Latitude must be a scalar.'
  if (n_elements(dec) gt 1) && (n_elements(dec) ne n_elements(ha)) then message, 'DEC must be a scalar or array of equal size to HA.'

  r2d = 180d0 / !dpi
  d2r = !dpi / 180d0

  ; MAKE SURE THE INPUT HOUR ANGLE IS IN DEGREES...
  had = keyword_set(degree) ? ha : 15d0 * ha

  ; USE THE FOUR-PARTS FORMULA FROM SPHERICAL TRIGONOMETRY
  ; TO DETERMINE THE PARALLACTIC ANGLE...
  pa = -r2d * atan(-sin(d2r * had), $
    cos(d2r * dec) * tan(d2r * latitude) - $
    sin(d2r * dec) * cos(d2r * had))

  if n_elements(dec) eq 1 then begin
    if dec gt latitude then pa = (pa + 360.d0) mod 360.d0
  endif else begin
    inds = where(dec gt latitude, ct)
    if ct gt 0 then pa[inds] = (pa[inds] + 360.d0) mod 360.d0
  endelse

  return, pa
end
; parangle