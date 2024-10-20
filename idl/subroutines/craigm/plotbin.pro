;+
; NAME:
;   PLOTBIN
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Makes a plot in a histogram style.
;
; CALLING SEQUENCE:
;   PLOTBIN, x, y, WIDTH=width, PIXCENTER=pixcenter, ...
;
; DESCRIPTION:
;
;   PLOTBIN makes an unfilled histogram plot.  The width of each
;   histogram bin can be specified individually, and the alignment of
;   the bin centers can be given explicitly.
;
;   PLOTBIN accepts several specialized keyword parameters of its own,
;   but passes any other keywords to the built-in IDL PLOT procedure.
;   Thus, any keywords accepted by PLOT can be passed to PLOTBIN.
;
;   PLOTBIN uses the PANEL/SUBPANEL system to partition the viewport.
;
; INPUTS:
;
;   X, Y - Two arrays which give the "X" and "Y" position of each bin.
;          If only the Y values are given, then the X values will be
;          the bin numbers.
;
; OPTIONAL INPUTS:
;   NONE
;
; INPUT KEYWORD PARAMETERS:
;
;   PANEL, SUBPANEL - An alternate way to more precisely specify the
;                     plot and annotation positions.  See SUBCELL.
;                     Default is full-screen.  Overridden by POSITION.
;
;   WIDTH - The width of each histogram bin.  If a scalar, then the
;           width is assumed to be the same for all histogram bins.
;           If a vector, then WIDTH should have the same number of
;           elements as X and Y, and specify the width of each
;           individual bin.
;           Default value: width is the separation between the first
;                          two X values.
;
;   PIXCENTER - Describes the alignment of "X" values with respect to
;               the histogram bin centers:
;                   PIXCENTER = 0.0  -- "X" values are left edges of bins
;                             = 0.5  -- "X" values are bin centers
;                             = 1.0  -- "X" values are right edges of bins
;               Intermediate values are also permitted.
;               Default value: 0.5 ("X" values are bin centers)
;
;   MIDPOINT - if set, then ignore the WIDTH and PIXCENTER keyword
;              values, and instead construct bin edges which lie at
;              the midpoints between data points.  This is usually the
;              most straightforward way to connect irregularly sampled
;              points "like a histogram," although at the expense of
;              not having a direct relation between X and the bin
;              centers.
;
;   EDGE - if set, then the X values will be taken to be the bin edges
;          rather than the bin midpoints.  In this case, the number of
;          X values should be one more than the number of Y values.
;
;   PLOTVERT - plot "vertically", that is, X is vertical and Y is
;              horizontal.
;
;   Other options are passed along to the PLOT command directly.
;
; OUTPUTS:
;   NONE
;
; PROCEDURE:
;
; EXAMPLE:
;
; SEE ALSO:
;
;   SUBCELL, DEFSUBCELL, SUBCELLARRAY
;
; EXTERNAL SUBROUTINES:
;
;   PLOT, SUBCELL
;
; MODIFICATION HISTORY:
;   Written, CM, 1997
;   Documented, CM, July 1999
;   Added MIDPOINT keyword, 21 Feb 2000
;   Added EDGE keyword, 21 Apr 2000
;   Corrected way that PIXCENTER works (Thanks to
;     J. Guerber), CM, 28 Mar 2002
;   Changed _EXTRA handling to use EXECUTE internally.  Unfortunately
;     makes it incompatible with VM version of IDL, 03 Aug 2003, CM
;   Remove EXECUTE function, move to CALL_PROCEDURE, 23 Nov 2003, CM
;   Add PLOTVERT keyword, 19 Apr 2004, CM
;
;  $Id: plotbin.pro,v 1.7 2004/04/19 09:09:10 craigm Exp $
;
;-
; Copyright (C) 1997-2000, 2002, 2003, 2004, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy and distribute unmodified copies for
; non-commercial purposes, and to modify and use for personal or
; internal use, is granted.  All other rights are reserved.
; -
; %insert HERE
; %include subcell.pro
pro plotbin, x0, y0, width = width, pixcenter = pixcenter, plotvert = plotvert, $
  subpanel = subpanel, panel = panel, midpoint = midpoint, edge = edge, $
  _extra = extra
  compile_opt idl2

  ; ; Account for a single "Y" value
  if n_params() eq 1 then begin
    x = dindgen(n_elements(x0))
    y = x0
  endif else begin
    x = x0
    y = y0
  endelse

  numx = n_elements(x)
  numy = n_elements(y)
  nump = numx < numy
  xtop = fltarr(2, nump)
  if numx le 0 or numy le 0 then begin
    message, 'ERROR: X and Y must contain at least one data point'
    return
  endif
  if keyword_set(midpoint) then begin
    if n_elements(width) eq 0 then width = 1
    if nump eq 1 then xtop[*] = x[0] + width[0] * [-0.5, 0.5] $
    else begin
      xtop[0, 1 : *] = 0.5 * (x[1 : nump - 1] + x[0 : nump - 2])
      xtop[1, 0 : nump - 2] = xtop[0, 1 : *]
      xtop[0, 0] = 2 * x[0] - xtop[1, 0]
      xtop[1, nump - 1] = 2 * x[nump - 1] - xtop[0, nump - 1]
    endelse
  endif else if keyword_set(edge) then begin
    if n_elements(x) ne numy + 1 then begin
      message, 'ERROR: X must contain one more element than Y'
      return
    endif
    xtop[0, *] = x[0 : nump - 1]
    xtop[1, *] = x[1 : nump]
  endif else begin
    if n_elements(x) eq 1 and n_elements(width) eq 0 then width = x[0] * 0 + 1
    if n_elements(width) eq 0 then width = (x[1] - x[0])
    if n_elements(width) eq 1 then width = width[0]
    if n_elements(width) gt 1 and n_elements(width) lt nump then begin
      message, 'ERROR: WIDTH must be the same size as X & Y (or be scalar)'
      return
    endif
    if n_elements(pixcenter) eq 0 then pixcenter = 0.5
    xtop[0, *] = x[0 : nump - 1] - width * pixcenter
    xtop[1, *] = x[0 : nump - 1] + width * (1. - pixcenter)
  endelse

  ytop = rebin(reform(y[0 : nump - 1], 1, nump), 2, nump)

  ; ; Vertical plot: swap X/Y
  if keyword_set(plotvert) then begin
    temp = temporary(xtop)
    xtop = temporary(ytop)
    ytop = temporary(temp)
  endif

  ; ; Default is full-screen
  if n_elements(panel) eq 0 and n_elements(subpanel) eq 0 then begin
    call_procedure, 'plot', xtop, ytop, _extra = extra
  endif else begin
    if n_elements(panel) eq 0 then panel = [0.0, 0.0, 1.0, 1.0]
    call_procedure, 'plot', xtop, ytop, /normal, $
      position = subcell(subpanel, panel, /marg), _extra = extra
  endelse

  return
end