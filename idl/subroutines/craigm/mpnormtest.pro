;+
; NAME:
;   MPNORMTEST
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE:
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Compute the probability of a given normally distributed Z value
;
; MAJOR TOPICS:
;   Curve and Surface Fitting, Statistics
;
; CALLING SEQUENCE:
;   PROB = MPNORMTEST(Z, [/CLEVEL, /SLEVEL ])
;
; DESCRIPTION:
;
;  The function MPNORMTEST() computes the probability for the
;  magnitude of a value drawn from the normal distribution to equal or
;  exceed the given value Z.  This can be used for confidence testing
;  of a measured value obeying the normal distribution.
;
;    P_NORM(ABS(X) > Z) = PROB
;
;  In specifying the returned probability level the user has two
;  choices:
;
;    * return the confidence level when the /CLEVEL keyword is passed;
;      OR
;
;    * return the significance level (i.e., 1 - confidence level) when
;      the /SLEVEL keyword is passed (default).
;
;  Note that /SLEVEL and /CLEVEL are mutually exclusive.
;
; INPUTS:
;
;   Z - the value to best tested.  Z should be drawn from a normal
;       distribution with zero mean and unit variance.  If a given
;       quantity Y has mean MU and standard deviation STD, then Z can
;       be computed as Z = (Y-MU)/STD.
;
; RETURNS:
;
;  Returns a scalar or vector of probabilities, as described above,
;  and according to the /SLEVEL and /CLEVEL keywords.
;
; KEYWORD PARAMETERS:
;
;   SLEVEL - if set, then PROB describes the significance level
;            (default).
;
;   CLEVEL - if set, then PROB describes the confidence level.
;
; EXAMPLES:
;
;   print, mpnormtest(5d, /slevel)
;
;   Print the probability for the magnitude of a randomly distributed
;   variable with zero mean and unit variance to exceed 5, as a
;   significance level.
;
; REFERENCES:
;
;   Algorithms taken from CEPHES special function library, by Stephen
;   Moshier. (http://www.netlib.org/cephes/)
;
; MODIFICATION HISTORY:
;   Completed, 1999, CM
;   Documented, 16 Nov 2001, CM
;   Reduced obtrusiveness of common block and math error handling, 18
;     Nov 2001, CM
;   Corrected error in handling of CLEVEL keyword, 05 Sep 2003
;   Convert to IDL 5 array syntax (!), 16 Jul 2006, CM
;   Move STRICTARR compile option inside each function/procedure, 9 Oct 2006
;   Add usage message, 24 Nov 2006, CM
;
;  $Id: mpnormtest.pro,v 1.8 2006/11/25 01:44:13 craigm Exp $
;-
; Copyright (C) 1997-2001, 2003, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
; -

forward_function cephes_polevl, cephes_erfc, cephes_erf, mpnormtest

; ; Set machine constants, once for this session.  Double precision
; ; only.
pro cephes_setmachar
  compile_opt strictarr
  common cephes_machar, cephes_machar_vals
  if n_elements(cephes_machar_vals) gt 0 then return

  if (!version.release) lt 5 then dummy = check_math(1, 1)

  mch = machar(/double)
  machep = mch.eps
  maxnum = mch.xmax
  minnum = mch.xmin
  maxlog = alog(mch.xmax)
  minlog = alog(mch.xmin)
  maxgam = 171.624376956302725d

  cephes_machar_vals = {machep: machep, maxnum: maxnum, minnum: minnum, $
    maxlog: maxlog, minlog: minlog, maxgam: maxgam}

  if (!version.release) lt 5 then dummy = check_math(0, 0)
  return
end

function cephes_polevl, x, coef
  compile_opt strictarr
  ans = coef[0]
  nc = n_elements(coef)
  for i = 1l, nc - 1 do ans = ans * x + coef[i]
  return, ans
end

pro cephes_set_erf_common
  compile_opt strictarr
  common cephes_erf_data, pp, qq, rr, ss, tt, uu, uthresh

  pp = [2.46196981473530512524d-10, 5.64189564831068821977d-1, $
    7.46321056442269912687d0, 4.86371970985681366614d1, $
    1.96520832956077098242d2, 5.26445194995477358631d2, $
    9.34528527171957607540d2, 1.02755188689515710272d3, $
    5.57535335369399327526d2]

  qq = [1.00000000000000000000d0, 1.32281951154744992508d1, $
    8.67072140885989742329d1, 3.54937778887819891062d2, $
    9.75708501743205489753d2, 1.82390916687909736289d3, $
    2.24633760818710981792d3, 1.65666309194161350182d3, $
    5.57535340817727675546d2]

  rr = [5.64189583547755073984d-1, 1.27536670759978104416d0, $
    5.01905042251180477414d0, 6.16021097993053585195d0, $
    7.40974269950448939160d0, 2.97886665372100240670d0]

  ss = [1.00000000000000000000d0, 2.26052863220117276590d0, $
    9.39603524938001434673d0, 1.20489539808096656605d1, $
    1.70814450747565897222d1, 9.60896809063285878198d0, $
    3.36907645100081516050d0]

  tt = [9.60497373987051638749d0, 9.00260197203842689217d1, $
    2.23200534594684319226d3, 7.00332514112805075473d3, $
    5.55923013010394962768d4]

  uu = [1.00000000000000000000d0, 3.35617141647503099647d1, $
    5.21357949780152679795d2, 4.59432382970980127987d3, $
    2.26290000613890934246d4, 4.92673942608635921086d4]

  uthresh = 37.519379347d
  return
end

; erfc.c
;
; Complementary error function
;
;
;
; SYNOPSIS:
;
; double x, y, erfc();
;
; y = erfc( x );
;
;
;
; DESCRIPTION:
;
;
; 1 - erf(x) =
;
; inf.
; -
; 2         | |          2
; erfc(x)  =  --------     |    exp( - t  ) dt
; sqrt(pi)   | |
; -
; x
;
;
; For small x, erfc(x) = 1 - erf(x); otherwise rational
; approximations are computed.
;
;
;
; ACCURACY:
;
; Relative error:
; arithmetic   domain     # trials      peak         rms
; DEC       0, 9.2319   12000       5.1e-16     1.2e-16
; IEEE      0,26.6417   30000       5.7e-14     1.5e-14
;
;
; ERROR MESSAGES:
;
; message         condition              value returned
; erfc underflow    x > 9.231948545 (DEC)       0.0
;
;
; /
function cephes_erfc, a
  compile_opt strictarr
  common cephes_erf_data
  if n_elements(p) eq 0 then cephes_set_erf_common

  common cephes_machar, machvals
  MAXLOG = machvals.maxlog

  if a lt 0 then x = -a else x = a
  if x lt 1. then return, 1.d - cephes_erf(a)

  z = -a * a
  if z lt -MAXLOG then begin
    under:
    ; message, 'ERROR: underflow', /info
    if a lt 0 then return, 2.d else return, 0.d
  endif

  z = exp(z)
  if x lt 8. then begin
    p = cephes_polevl(x, pp)
    q = cephes_polevl(x, qq)
  endif else begin
    p = cephes_polevl(x, rr)
    q = cephes_polevl(x, ss)
  endelse

  y = (z * p) / q
  if a lt 0 then y = 2.d - y
  if y eq 0 then goto, under

  return, y
end

; erf.c
;
; Error function
;
;
;
; SYNOPSIS:
;
; double x, y, erf();
;
; y = erf( x );
;
;
;
; DESCRIPTION:
;
; The integral is
;
; x
; -
; 2         | |          2
; erf(x)  =  --------     |    exp( - t  ) dt.
; sqrt(pi)   | |
; -
; 0
;
; The magnitude of x is limited to 9.231948545 for DEC
; arithmetic; 1 or -1 is returned outside this range.
;
; For 0 <= |x| < 1, erf(x) = x * P4(x**2)/Q5(x**2); otherwise
; erf(x) = 1 - erfc(x).
;
;
;
; ACCURACY:
;
; Relative error:
; arithmetic   domain     # trials      peak         rms
; DEC       0,1         14000       4.7e-17     1.5e-17
; IEEE      0,1         30000       3.7e-16     1.0e-16
;
;
function cephes_erf, x
  compile_opt strictarr
  common cephes_erf_data
  if abs(x) gt 1. then return, 1.d - cephes_erfc(x)
  if n_elements(p) eq 0 then cephes_set_erf_common
  z = x * x
  y = x * cephes_polevl(z, tt) / cephes_polevl(z, uu)
  return, y
end

function mpnormtest, a, clevel = clevel, slevel = slevel
  compile_opt strictarr

  if n_params() eq 0 then begin
    message, 'USAGE: PROB = MPNORMTEST(Z, [/CLEVEL, /SLEVEL ])', /info
    return, !values.d_nan
  endif

  cephes_setmachar ; ; Set machine constants

  y = a * 0
  sqrth = sqrt(2.d) / 2.d
  x = a * sqrth

  ; ; Default is to return the significance level
  if n_elements(slevel) eq 0 and n_elements(clevel) eq 0 then slevel = 1
  if keyword_set(slevel) then begin
    for i = 0l, n_elements(y) - 1 do begin
      if abs(x[i]) lt sqrth then y[i] = 1.d - cephes_erf(abs(x[i])) $
      else y[i] = cephes_erfc(abs(x[i]))
    endfor
  endif else if keyword_set(clevel) then begin
    for i = 0l, n_elements(y) - 1 do begin
      if abs(x[i]) lt sqrth then y[i] = cephes_erf(abs(x[i])) $
      else y[i] = 1.d - cephes_erfc(x[i])
    endfor
  endif else begin
    message, 'ERROR: must specify one of CLEVEL, SLEVEL'
  endelse

  return, y
end