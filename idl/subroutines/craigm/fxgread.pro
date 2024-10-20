;+
; NAME:
;   FXGREAD
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE:
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Perform an unformatted read on a generic resource.
;
; MAJOR TOPICS:
;   File I/O, Pipes, URLs, FITS
;
; CALLING SEQUENCE:
;   FXGREAD, UNIT, BUFFER, TRANSFER_COUNT=TC
;
; DESCRIPTION:
;
;   FXGREAD performs an unformatted read on the unit UNIT.  The UNIT
;   must have previously been opened by FXGOPEN.
;
;   Currently only unformatted reads are permitted because the precise
;   number of bytes to read must be known ahead of time.
;
;   In other respects, this procedure is similar to the READU built-in
;   IDL procedure.
;
;   You must use the specialized 'FXG' style functions to read, write
;   and seek on file units opened with FXGOPEN:
;
;     FXGOPEN  - open resource
;     FXGCLOSE - close resource
;     FXGREAD  - read from resource
;     FXGWRITE - write to resource
;     FXGSEEK  - seek on resource (i.e., perform POINT_LUN)
;
;     FXGFILTERED - determine if resource is a normal file.
;
; INPUTS:
;
;   UNIT - the unit number to operate on.  The unit must have been
;          previously opened by FXGOPEN.
;
;   BUFFER - an array of the desired type and size is passed upon
;            input to FXGREAD.  Only basic types are permitted.  Upon
;            output, the array will have been filled with data from
;            the resource.  The full extent of the transfer can be
;            determined by examining the TRANSFER_COUNT.
;
;
; KEYWORD PARAMETERS:
;
;   TRANSFER_COUNT - upon output, contains the number of elements
;                    transferred to BUFFER.
;
; MODIFICATION HISTORY:
;   Written, 1999, CM
;   Documented, 02 Oct 1999, CM
;   Changed copyright notice, 21 Sep 2000, CM
;
;  $Id: fxgread.pro,v 1.3 2007/09/01 23:03:23 craigm Exp $
;
;-
; Copyright (C) 1999-2000, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
; -

pro FXGREAD, UNIT, BUFFER, transfer_count = TC, status = STATUS
  compile_opt idl2

  on_error, 2
  TC = 0

  if n_params() ne 2 then begin
    message, 'USAGE: FXGREAD, UNIT, BUFFER [, TRANSFER_COUNT=TC ]', /info
    return
  endif
  if n_elements(UNIT) eq 0 then $
    message, 'ERROR: UNIT is not defined'
  if n_elements(BUFFER) eq 0 then $
    message, 'ERROR: BUFFER is not defined'

  @fxfilter
  if UNIT lt 0 or UNIT ge FXFILTER_MAX_LUN then $
    message, 'ERROR: UNIT is not a valid file unit'

  readu = 'READU'
  if filterflag(UNIT) and 1 then $
    if read_cmd(UNIT) ne '' then readu = read_cmd(UNIT)
  if readu eq '-' then begin
    errmsg = string(UNIT, $
      format = '("ERROR: Resource unit ",I0," does not support reading.")')
    message, errmsg
  endif

  TC = 0l
  STATUS = 0l
  catch, catcherror
  if catcherror ne 0 then begin
    STATUS = !err
    catch, /cancel
    return
  endif

  call_procedure, readu, UNIT, BUFFER, transfer_count = TC

  return
end