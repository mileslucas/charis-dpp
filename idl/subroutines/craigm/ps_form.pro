;+
; NAME:
;   PS_FORM
;
; PURPOSE:
;   This function puts up a form the user can configure a PostScript
;   device driver. The function result (if the user selects either the
;   ACCEPT or CREATE FILE buttons) can be sent directly to the DEVICE
;   procedure by means of its _Extra keyword.  User's predefined
;   configurations may be retrieved from a common block.
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   $Id: ps_form.pro,v 1.5 2004/10/03 09:40:08 craigm Exp $
;
;   Based almost entirely on, but a totally revamped version of, PS_FORM by
;   FANNING SOFTWARE CONSULTING (David Fanning Ph.D.) http://www.dfanning.com
;
; MAJOR TOPICS:
;   Device Drivers, Hardcopy Output, PostScript Output
;
; PROCEDURE:
;   This is a pop-up form widget. It is a modal or blocking widget.
;   Keywords appropriate for the PostScript DEVICE command are returned.
;
;   Use your LEFT mouse button to move the "Plot Window" around the page.
;   Use your RIGHT mouse button to draw your own "Plot Window" on the page.
;
; HELP:
;   formInfo = PS_FORM(/Help)
;
; CALLING SEQUENCE:
;    formInfo = PS_FORM(xoffset, yoffset, Cancel=cancelButton)
;
; OPTIONAL INPUTS:
;
;    XOFFSET -- Optional xoffset of the top-level base of ps_form. Default is
;    to try to center the form on the display.
;
;    YOFFSET -- Optional yoffset of the top-level base of ps_form. Default is
;    to try to center the form on the display.
;
; INPUT KEYWORD PARAMETERS:
;
;    BITS_PER_PIXEL -- The initial configuration of the bits per pixel button.
;
;    BLOCKING -- Set this keyword to make this a blocking widget under IDL 5.0.
;    (All widget programs block under IDL 4.0.)
;
;    COLOR -- The initial configuration of the color switch.
;
;    DEFAULTS -- A stucture variable of the same type and structure as the
;    RETURN VALUE of ps_form. It will set initial conditions. This makes
;    it possible to start ps_form up again with the same values it had the
;    last time it was called. For example:
;
;       mysetup = ps_form()
;       newsetup = ps_form(Defaults=mysetup)
;
;    ENCAPSULATED -- The initial configuration of the encapsulated switch.
;
;    FILENAME -- The initial filename to be used on the form.
;
;    HELP -- Prints a helpful message in the output log.
;
;    INCHES -- The initial configuration of the inches/cm switch.
;
;    INITIALIZE -- If this keyword is set, the program immediately returns the
;    "localdefaults" structure. This gives you the means to configue the
;    PostScript device without interrupting the user.
;
;    SELECT -- used only when INITIALIZE is set.  Set SELECT to a
;              string which identifies the predefined configuration to
;              be returned by ps_form when INITIALIZE is set.  This is
;              a convenient way to select a predefined config
;              non-interactively.
;
;    LANDSCAPE -- The initial configuration of the landscape/portrait switch.
;
;    LOCALDEFAULTS -- A structure like the DEFAULTS structure. If specified,
;    then it is added as a predefined configuration entry called "Local".
;    See below for a further discussion of predefined configurations.
;
;    PREDEFINED -- An alternate way to specify predefined
;                  configurations.  Pass an array of structures to
;                  populate the "predefined" dropbox in the
;                  dialog. This array, if specified, overrides the the
;                  common block technique.
;
;    XOFFSET -- The initial XOffSet of the PostScript window.
;
;    YOFFSET -- The initial YOffSet of the PostScript window.
;
;    XSIZE -- The initial XSize of the PostScript window.
;
;    YSIZE -- The initial YSize of the PostScript window.
;
;    ASPECT -- The aspect ratio of the window (Y/X).  This keyword can
;              substitute for one of XSIZE or YSIZE.
;
;    PRESERVE_ASPECT -- Set this keyword if you want to hold the
;                       aspect ratio constant.
;
;    PAPERSIZE -- If set, allows user to specify the size of the paper
;                 media to be printed on, as a scalar string.  NOTE:
;                 this specification cannot be passed to DEVICE, but
;                 can be selected for completeness's sake.  Default is
;                 'Letter'.
;
;    MARGINSIZE -- Size of the margins on all sides.  Default is 0.25 inches.
;                  When MARGINSIZE is non-zero, a graphic cannot directly
;                  abut the edge of the page.  This is normally a good thing,
;                  since there is often a  non-printable region which borders
;                  the page.
;
;   DEFAULTPAPER -- Default paper size to use, when it is unspecified
;                   in a predefined, "local", or "default"
;                   configuration.  This value also overrides any
;                   configuration from common blocks.  European users
;                   will probably set this to 'A4'.
;
;   PARENT -- if this widget is invoked by another widget program,
;             then this keyword parameter must be set to the top level
;             widget which is to serve as the group leader.  Failure
;             to do so will result in unexpected behavior.  IDL 4
;             programs do not need to pass this parameter.  Default:
;             NONE.
;
; OUTPUT KEYWORD PARAMETERS
;
;    CANCEL -- This is an OUTPUT keyword. It is used to check if the user
;    selected the "Cancel" button on the form. Check this variable rather
;    than the return value of the function, since the return value is designed
;    to be sent directly to the DEVICE procedure. The varible is set to 1 if
;    the user selected the "Cancel" button. Otherwise, it is set to 0.
;
;    CREATE -- This output keyword can be used to determine if the user
;    selected the 'Create File' button rather than the 'Accept' button.
;    The value is 1 if selected, and 0 otherwise.
;
;    PAPERSIZE -- If set to a named variable, any newly selected paper
;    size is returned in that variable.
;
;    XPAGESIZE -- Size of paper in "X" dimension, in units given by
;                 the returned config structure.
;
;    YPAGESIZE -- Size of paper in "Y" dimension, in units given by
;                 the returned config structure.
;
;    PAGEBOX -- specifies the page rectangle relative to the plot
;               window, in normalized units.  A 4-vector of the form
;               [XLL, YLL, XUR, YUR] is returned, giving the positions
;               of the lower left (LL) and upper right (UR) corners of
;               the page with respect to the plot window.  Thus, the
;               following command:
;
;                    PLOT, x, y, position=PAGEBOX
;
;               will construct a graphic whose plot region exactly
;               fills the page (with no margin around the edges).
;
;               Naturally, the page is usually larger than the
;               graphics window, so the normalized coordinates will
;               usually fall outside the range [0,1].
;
;               However, the bounding box constructed by the
;               Postscript driver includes only the graphics window.
;               Anything drawn outside of it may be clipped or
;               discarded.
;
; RETURN VALUE:
;
;     formInfo = { ps_form_INFO, $
;                  xsize:0.0, $        ; The x size of the plot
;                  xoff:0.0, $         ; The x offset of the plot
;                  ysize:0.0, $        ; The y size of the plot
;                  yoff:0.0 $          ; The y offset of the plot
;                  filename:'', $      ; The name of the output file
;                  inches:0 $          ; Inches or centimeters?
;                  color:0, $          ; Color on or off?
;                  bits_per_pixel:0, $ ; How many bits per image pixel?
;                  encapsulated:0,$    ; Encapsulated or regular PostScript?
;                  isolatin1:0,$       ; Encoded with ISOLATIN1?
;                  landscape:0 }       ; Landscape or portrait mode?
;
; USAGE:
;
;  The calling procedure for this function in a widget program will
;  look something like this:
;
;     info.ps_config = ps_form(/Initialize)
;
;     formInfo = ps_form(Cancel=canceled, Create=create, $
;                          Defaults=info.ps_config)
;
;     IF NOT canceled THEN BEGIN
;        IF create THEN BEGIN
;           thisDevice = !D.Name
;           Set_Plot, "PS"
;           Device, _Extra=formInfo
;
;           Enter Your Graphics Commands Here!
;
;           Device, /Close
;           Set_Plot, thisDevice
;           info.ps_config = formInfo
;        ENDIF ELSE
;           info.ps_config = formInfo
;     ENDIF
;
; MAJOR FUNCTIONS and PROCEDURES:
;
;   None. Designed to work originally in conjunction with XWindow, a
;   resizable graphics window.  [ NOTE: this modified version of
;   ps_form, by Craig Markwardt, is incompatible with the original
;   version of XWINDOW. ]
;
; MODIFICATION HISTORY:
;
;   Based on ps_form of : David Fanning, RSI, March 1995.
;   Major rewrite by: Craig Markwardt, October 1997.
;     - Drawing and updating of form and sample box are now modular
;     - Option of storing more than one predefined postscript configuration
;     - Selection of paper size by name
;     - Access to predfined configurations through (optional) common
;       block
;   Several additions, CM, April 1998  VERSION CM2.0
;     - better integration of paper sizes throughout program.
;       Predefined configurations now also know about paper.
;     - allow passing predefined configurations instead of using
;       common block
;     - addition of ISOLATIN selection, and streamlining of dialog
;       appearance
;   Fixed bug in INITIALIZE w.r.t. paper sizes, CM, Nov 1998
;   Added SELECT keyword, CM, 09 Dec 1998
;   Added Parent keyword to allow modal widgets in IDL 5, 19 Jan 1999
;   Added "Choose" button for filename selection, 19 Sep 1999
;   Added ability to program different button names, 19 Sep 1999
;   Added ASPECT and PRESERVE_ASPECT, based on work by Aaron Barth, 18
;     Oct 1999
;   Removed NOCOMMON documentation and logic, 19 Oct 1999, CM
;   Added aspect to ps_form_numevents (per Aaron Barth), 18 Oct 1999
;   Corrected small bug under Initialize keyword (inches), 18 Oct 1999
;   Made call to *_pscoord more consistent, 18 Oct 1999
;   Added XPAGESIZE, YPAGESIZE and PAGEBOX keywords, 19 Oct 1999
;   Small cosmetic cleanup, CM, 01 Feb 2000
;   Fix for IDL 5.5's handling of structures with arrays, CM, 11 Dec 2001
;   Replaced obsolete PICKFILE call with DIALOG_PICKFILE, Jeff Guerber,
;     24 Sep 2004
;   Transfer DEFAULTS and LOCALDEFAULTS values via STRUCT_ASSIGN,/NOZERO
;     instead of EXECUTE, Jeff Guerber, 24 Sep 2004.
;   Set CANCELBUTTON and CREATEBUTTON immediately on entry, so they're
;     defined even if user kills the window, Jeff Guerber, 24 Sep 2004.
;
; COMMON BLOCKS:
;
;   The user may store frequently used or helpful configurations in a
;   common block, and ps_form() will attempt to access them.  This
;   provides a way for the user to have persistent, named,
;   configurations.
;
;   NOTE: this format has changed since the last version.  You may
;   have to quit your IDL session for the changes to take effect
;   properly.  If you have place a predefined configuration in your
;   startup file, you should review the new format.
;
;     COMMON PS_FORM_CONFIGS, ps_form_DEFAULT_PAPERSIZE, $
;                               ps_form_STDCONFIGS
;
;        ps_form_DEFAULT_PAPERSIZE - a string designating the default
;                                    paper size, when none is given.
;                                    The predefined configurations
;                                    offerred by this program will
;                                    respect the default value.  (See
;                                    also the DEFAULTPAPER keyword.)
;
;        ps_form_STDCONFIGS - An array of ps_form_CONFIG structures,
;                             each containing information about one
;                             predefined configuration, such as its
;                             name and size of paper.  Each "config"
;                             element is a ps_form_INFO structure,
;                             which contains the actual postscript
;                             configuration.
;
;   See the IDL source code ps_form_LOAD_CONFIGS for an example of how
;   to make a list of configurations.  One possibility would be to
;   declare and populate the common block from within the user's
;   start-up script, allowing the same configurations to appear in
;   every session.
;
;   ps_form() takes its initial list of configurations from this
;   common block if it exists.  A default list is provided ala the
;   procedure ps_form_LOAD_CONFIGS.  Any modifications that take place
;   during the ps_form() widget session are not transferred back to
;   the common block upon return.  It might be useful to be able to do
;   this, through some form of 'save' procedure.
;
;   Also, if the PREDEFINED keyword is used, then the common block is
;   not consulted.
;
;  $Id: ps_form.pro,v 1.5 2004/10/03 09:40:08 craigm Exp $
;
;-
; Copyright (C) 1996-1997, David Fanning
; Copyright (C) 1997-2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
; -

; ***************************************************************
; Utility routines

forward_function filepath

; Convert from inches and centimeters to WIDGET_DRAW pixels
pro ps_form_Draw_Coords, drawpixperunit, xoff, yoff, xsize, ysize
  compile_opt idl2

  if n_elements(xoff) gt 0 then xoff = xoff * drawpixperunit + 1
  if n_elements(yoff) gt 0 then yoff = yoff * drawpixperunit + 1
  if n_elements(xsize) gt 0 then xsize = xsize * drawpixperunit
  if n_elements(ysize) gt 0 then ysize = ysize * drawpixperunit

  return
end

; Perform the opposite conversion of ps_form_DRAW_COORDS
pro ps_form_Real_Coords, drawpixperunit, xoff, yoff, xsize, ysize
  compile_opt idl2

  if n_elements(xoff) gt 0 then xoff = (xoff - 1) / drawpixperunit
  if n_elements(yoff) gt 0 then yoff = (yoff - 1) / drawpixperunit
  if n_elements(xsize) gt 0 then xsize = xsize / drawpixperunit
  if n_elements(ysize) gt 0 then ysize = ysize / drawpixperunit

  return
end

pro ps_form_Select_File, event
  compile_opt idl2

  ; Allows the user to select a filename for writing.

  widget_control, event.top, get_uvalue = info, /no_copy

  ; Start with the name in the filename widget.

  widget_control, info.idfilename, get_value = initialFilename
  initialFilename = initialFilename[0]
  filename = dialog_pickfile(/write, file = initialFilename)
  if filename ne '' then $
    widget_control, info.idfilename, set_value = filename
  widget_control, event.top, set_uvalue = info, /no_copy
end
; *******************************************************************

; Calculate a list of vertices to be plotted as a box in the
; draw widget.
function ps_form_PlotBox_Coords, xsize, ysize, xoff, yoff, drawpixperunit
  compile_opt idl2

  ; This function converts sizes and offsets to appropriate
  ; Device coordinates for drawing the PLOT BOX on the PostScript
  ; page. The return value is a [2,5] array.

  returnValue = intarr(2, 5)
  xs = xsize
  ys = ysize
  xof = xoff
  yof = yoff
  ps_form_Draw_Coords, drawpixperunit, xof, yof, xs, ys

  ; Add one because we do for the page outline
  xcoords = round([xof, xof + xs, xof + xs, xof, xof]) + 1
  ycoords = round([yof, yof, yof + ys, yof + ys, yof]) + 1

  returnValue[0, *] = xcoords
  returnValue[1, *] = ycoords

  RETURN, returnValue
end
; *******************************************************************

; Convert between the IDL-form of PS coordinates (including the
; strange definition of YOFFSET and XOFFSET) to a more
; "human-readable" form where the Xoffset and YOFFSET always refer to
; the lower-left hand corner of the output
pro ps_form_conv_pscoord, info, xpagesize, ypagesize, $
  toidl = toidl, tohuman = tohuman
  compile_opt idl2

  if info.landscape eq 1 then begin
    ixoff = info.xoff
    iyoff = info.yoff
    if keyword_set(tohuman) then begin
      info.yoff = ixoff
      info.xoff = xpagesize - iyoff
    endif else if keyword_set(toidl) then begin
      info.xoff = iyoff
      info.yoff = xpagesize - ixoff
    endif
  endif
  return
end

; Return names of paper sizes
function ps_form_papernames
  compile_opt idl2

  return, ['Letter', 'Legal', 'Tabloid', 'Ledger', 'Executive', 'Monarch', $
    'Statement', 'Folio', 'Quarto', 'C5', 'B4', 'B5', 'Dl', 'A0', 'A1', $
    'A2', 'A3', 'A4', 'A5', 'A6']
end

; Select a paper size based on number or string.  Returns x and
; y page sizes, accouting for the units of measurement and the
; orientation of the page.
pro ps_form_select_papersize, papertype, xpagesize, ypagesize, $
  inches = inches, landscape = landscape, index = index
  compile_opt idl2

  ; Letter Legal Tabloid Ledger Executive Monarch Statement Folio
  xpaper = [612., 612, 792, 792, 540, 279, 396, 612, $
    $ ; Quarto C5  B4  B5  Dl  A0   A1   A2   A3  A4  A5  A6
    610, 459, 729, 516, 312, 2380, 1684, 1190, 842, 595, 420, 297]

  ; Letter Legal Tabloid Ledger Executive Monarch Statement Folio
  ypaper = [792., 1008, 1224, 1224, 720, 540, 612, 936, $
    $ ; Quarto C5  B4   B5  Dl  A0   A1   A2   A3   A4  A5  A6
    780, 649, 1032, 729, 624, 3368, 2380, 1684, 1190, 842, 595, 421]
  names = ps_form_papernames()

  sz = size(papertype)
  tp = sz[sz[0] + 1]
  if tp gt 0 and tp lt 6 then begin
    index = fix(papertype)
  endif else if tp eq 7 then begin
    index = where(strupcase(papertype) eq strupcase(names), ict)
    if ict eq 0 then index = 0
  endif else $
    index = 0

  index = index[0]

  xpagesize = xpaper[index] / 72. ; Convert to inches
  ypagesize = ypaper[index] / 72.
  xpagesize = xpagesize[0]
  ypagesize = ypagesize[0]
  if not keyword_set(inches) then begin
    xpagesize = xpagesize * 2.54
    ypagesize = ypagesize * 2.54
  endif
  if keyword_set(landscape) then begin
    temp = xpagesize
    xpagesize = ypagesize
    ypagesize = temp
  endif

  return
end

; ps_form_LOAD_CONFIGS
;
; Loads a set of default configurations into the output variables,
;
; CONFIGNAMES - array of names for configurations.
;
; CONFIGS - array of ps_form_INFO structures, each with a separate
; configuration in it, and corresponding to the
; configuration name.
;
; Intended as an intelligent default when no other is specified.
;
pro ps_form_load_configs, defaultpaper, configs
  compile_opt idl2

  ; This is the default paper size, when none is given
  defaultpaper = 'Letter'

  ; Here is how the ps_form_INFO structure is defined.  Refer to it
  ; when creating new structures.
  template = {Ps_Form_Info, $
    xsize: 0.0, $ ; The x size of the plot
    xoff: 0.0, $ ; The x offset of the plot
    ysize: 0.0, $ ; The y size of the plot
    yoff: 0.0, $ ; The y offset of the plot
    filename: '', $ ; The name of the output file
    inches: 0, $ ; Inches or centimeters?
    color: 0, $ ; Color on or off?
    bits_per_pixel: 0, $ ; How many bits per image pixel?
    encapsulated: 0, $ ; Encapsulated or regular PostScript?
    isolatin1: 0, $ ; Encoding is not ISOLATIN1
    landscape: 0} ; Landscape or portrait mode?

  pctemplate = {Ps_Form_Config, $
    config: {Ps_Form_Info}, $
    configname: '', $ ; Name of configuration
    papersize: ''} ; Size of paper for configuration

  ; Set of default configurations (no ISOLATIN1 encoding)
  ; 1.  7x5    inch color plot region in portrait
  ; 2.  7.5x10 inch centered color plot region, covering almost whole
  ; portrait page (0.5 inch margins)
  ; 3.  10x7.5 inch centered color plot region, covering almost whole
  ; landscape page (0.5 inch margins)
  ; 4.  7x5    inch gray plot region in portrait (IDL default config)
  configs = [{Ps_Form_Config, config: $
      {Ps_Form_Info, 7.0, 0.75, 5.0, 5.0, 'idl.ps', 1, 1, 8, 0, 0, 0}, $
    configname: 'Half Portrait (color)', papersize: defaultpaper}, $
    {Ps_Form_Config, config: $
      {Ps_Form_Info, 7.5, 0.50, 10., 0.5, 'idl.ps', 1, 1, 8, 0, 0, 0}, $
      configname: 'Full Portrait (color)', papersize: defaultpaper}, $
    {Ps_Form_Config, config: $
      {Ps_Form_Info, 10., 0.50, 7.5, 10.5, 'idl.ps', 1, 1, 8, 0, 0, 1}, $
      configname: 'Full Landscape (color)', papersize: defaultpaper}, $
    {Ps_Form_Config, config: $
      {Ps_Form_Info, 18., 1.5, 26.7, 1.5, 'idl.ps', 0, 1, 8, 0, 0, 0}, $
      configname: 'A4 Portrait (color)', papersize: 'A4'}, $
    {Ps_Form_Config, config: $
      {Ps_Form_Info, 26.7, 1.5, 18., 28.2039, 'idl.ps', 0, 1, 8, 0, 0, 1}, $
      configname: 'A4 Landscape (color)', papersize: 'A4'}, $
    {Ps_Form_Config, config: $
      {Ps_Form_Info, 17.78, 1.91, 12.70, 12.70, 'idl.ps', 0, 1, 4, 0, 0, 0}, $
      configname: 'IDL Standard', papersize: defaultpaper}]

  return
end

;
; ps_form_Update_Info
;
; This procedure modifies an "info" structure, according to new
; specifications about the PS configuration.  This is the central
; clearing house for self-consistent modification of the info structure.
;
; INPUTS
; info    - info structure to be modified
; keywords- IDL keywords are contain information is folded
; into the "info" structure.
; Valid keywords are:
; XSIZE, YSIZE,
; XOFF, YOFF    - size and offset of plotting region in
; "human" coordinates.  This is the
; natural size as measured from the
; lower-left corner of the page in its
; proper orientation (not the IDL
; definition!).  These are the same
; values that are printed in the form's
; Size and Offset fields.
; INCHES        - whether dimensions are in inches or
; centimeters (1=in, 0=cm)
; COLOR         - whether output is color (1=y, 0=n)
; BITS_PER_PIXEL- number of bits per pixel (2,4,8)
; ENCAPSULATED  - whether output is EPS (1=EPS, 0=PS)
; LANDSCAPE     - whether output is portrait or
; landscape (1=land, 0=port)
; FILENAME      - output file name (with respect to
; current directory)
;
pro ps_form_Update_Info, info, set = set, _extra = newdata
  compile_opt idl2

  if n_elements(newdata) gt 0 then $
    names = tag_names(newdata)
  set = keyword_set(set)
  centerfactor = 1.0

  for j = 0, n_elements(names) - 1 do begin
    case strupcase(names[j]) of
      'XSIZE': info.devconfig.xsize = float(newdata.xsize)
      'YSIZE': info.devconfig.ysize = float(newdata.ysize)
      'XOFF': info.devconfig.xoff = float(newdata.xoff)
      'YOFF': info.devconfig.yoff = float(newdata.yoff)
      'INCHES': begin
        inches = fix(newdata.inches)
        if inches ne 0 then inches = 1
        if set ne 1 then begin
          convfactor = 1.0
          if info.devconfig.inches eq 0 and inches eq 1 then $
            convfactor = 1.0 / 2.54 $ ; centimeters to inches
          else if info.devconfig.inches eq 1 and inches eq 0 then $
            convfactor = 2.54 ; inches to centimeters

          info.devconfig.xsize = info.devconfig.xsize * convfactor
          info.devconfig.ysize = info.devconfig.ysize * convfactor
          info.devconfig.xoff = info.devconfig.xoff * convfactor
          info.devconfig.yoff = info.devconfig.yoff * convfactor
          info.xpagesize = info.xpagesize * convfactor
          info.ypagesize = info.ypagesize * convfactor
          info.marginsize = info.marginsize * convfactor
          info.drawpixperunit = info.drawpixperunit / convfactor
        endif

        info.devconfig.inches = inches
      end

      'LANDSCAPE': begin
        landscape = fix(newdata.landscape)
        if landscape ne 0 then landscape = 1

        if landscape ne info.devconfig.landscape and $
          set ne 1 then begin
          temp = info.xpagesize
          info.xpagesize = info.ypagesize
          info.ypagesize = temp

          ; Since the margins are bound to be way out of wack,
          ; we could recenter here.

          xsize = info.devconfig.xsize
          ysize = info.devconfig.ysize

          centerfactor = 2.0

          ; We will have to redraw the reserve pixmap
          info.pixredraw = 1
        endif

        info.devconfig.landscape = landscape
      end

      'COLOR': begin
        info.devconfig.color = fix(newdata.color)
        if info.devconfig.color ne 0 then info.devconfig.color = 1
      end
      'ENCAPSULATED': begin
        info.devconfig.encapsulated = fix(newdata.encapsulated)
        if info.devconfig.encapsulated ne 0 then $
          info.devconfig.encapsulated = 1
      end
      'ISOLATIN1': begin
        info.devconfig.isolatin1 = fix(newdata.isolatin1)
        if info.devconfig.isolatin1 ne 0 then $
          info.devconfig.isolatin1 = 1
      end
      'BITS_PER_PIXEL': begin
        bpp = fix(newdata.bits_per_pixel)
        if bpp lt 1 then bpp = 2
        if bpp gt 2 and bpp lt 4 then bpp = 4
        if bpp gt 4 and bpp lt 8 then bpp = 8
        if bpp gt 8 then bpp = 8
        info.devconfig.bits_per_pixel = bpp
      end
      'FILENAME': begin
        if string(newdata.filename) ne info.devconfig.filename then $
          info.filechanged = 1
        info.devconfig.filename = string(newdata.filename)
      end
    endcase
  endfor

  ; Now check the sizes and offsets, to be sure they are sane for the
  ; particular landscape/portrait and inch/cm settings that have been
  ; chosen.
  pgwid = info.xpagesize
  pglen = info.ypagesize
  pgmar = info.marginsize

  if set ne 1 then begin
    info.devconfig.xsize = (pgmar) > info.devconfig.xsize < (pgwid - 2. * pgmar)
    info.devconfig.ysize = (pgmar) > info.devconfig.ysize < (pglen - 2. * pgmar)
    info.devconfig.xoff = (pgmar) > info.devconfig.xoff < (pgwid - info.devconfig.xsize - pgmar)
    info.devconfig.yoff = (pgmar) > info.devconfig.yoff < (pglen - info.devconfig.ysize - pgmar)
    if info.devconfig.xsize + info.devconfig.xoff gt (pgwid - pgmar) then $
      info.devconfig.xoff = (pgwid - info.devconfig.xsize) / centerfactor
    if info.devconfig.ysize + info.devconfig.yoff gt (pglen - pgmar) then $
      info.devconfig.yoff = (pglen - info.devconfig.ysize) / centerfactor
  endif

  ; Preserve aspect ratio if necessary
  if (info.preserve_aspect eq 1) then begin
    sizeratio = info.aspect / (info.ypagesize / info.xpagesize)
    if (sizeratio ge 1) then $
      info.devconfig.xsize = info.devconfig.ysize / info.aspect $
    else $
      info.devconfig.ysize = info.devconfig.xsize * info.aspect
  endif

  return
end

;
; PRO ps_form_DRAW_BOX
;
; Draw the "sample" box in the draw widget.  If necessary, also
; redraws the backing reserve pixmap.
;
pro ps_form_draw_box, xsize, ysize, xoff, yoff, info
  compile_opt idl2

  ; First order of business is to make a new reserve pixmap, if
  ; necessary.

  if info.pixredraw eq 1 then begin
    ; Operate on the pixmap first
    wset, info.idpixwid
    erase
    ; Make background ...
    tv, replicate(info.bkgcolor, info.xpixwinsize, info.ypixwinsize)
    ; ... and page outline
    coords = ps_form_PlotBox_Coords(info.xpagesize, info.ypagesize, $
      0., 0., info.drawpixperunit)
    plots, coords[0, *], coords[1, *], /device, color = info.pagecolor

    info.pixredraw = 0
  endif

  ; Now, we transfer the reserve pixmap to the screen

  wset, info.idwid
  device, copy = [0, 0, info.xpixwinsize, info.ypixwinsize, 0, 0, $
    info.idpixwid]

  ; Finally we overlay the plot region
  coords = ps_form_PlotBox_Coords(xsize, ysize, xoff, yoff, info.drawpixperunit)
  plots, coords[0, *], coords[1, *], color = info.boxcolor, /device

  return
end

;
; ps_form_DRAW_FORM
;
; Update the widget elements of the ps_form form, using the INFO structure.
;
; If the NOBOX keyword is set, then the draw widget is not updated.
;
pro ps_form_draw_form, info, nobox = nobox
  compile_opt idl2

  ; Draw the DRAW widget if needed
  if not keyword_set(nobox) then $
    ps_form_draw_box, info.devconfig.xsize, info.devconfig.ysize, $
    info.devconfig.xoff, info.devconfig.yoff, info

  ; Update the numeric text fields
  xsizestr = strtrim(string(info.devconfig.xsize, format = '(F6.2)'), 2)
  ysizestr = strtrim(string(info.devconfig.ysize, format = '(F6.2)'), 2)
  xoffstr = strtrim(string(info.devconfig.xoff, format = '(F6.2)'), 2)
  yoffstr = strtrim(string(info.devconfig.yoff, format = '(F6.2)'), 2)

  widget_control, info.idxsize, set_value = xsizestr
  widget_control, info.idysize, set_value = ysizestr
  widget_control, info.idxoff, set_value = xoffstr
  widget_control, info.idyoff, set_value = yoffstr

  widget_control, info.idaspect, set_button = (info.preserve_aspect eq 1)

  ; Set EPS (encapsulated ps) buttons
  widget_control, info.idencap, set_button = (info.devconfig.encapsulated eq 1)

  ; Set color buttons.
  widget_control, info.idcolor, set_button = (info.devconfig.color eq 1)

  ; Set inch/cm buttons.
  widget_control, info.idinch, set_button = (info.devconfig.inches eq 1)
  widget_control, info.idcm, set_button = (info.devconfig.inches eq 0)

  ; Set bits_per_pixel buttons.
  widget_control, info.idbit2, set_button = (info.devconfig.bits_per_pixel eq 2)
  widget_control, info.idbit4, set_button = (info.devconfig.bits_per_pixel eq 4)
  widget_control, info.idbit8, set_button = (info.devconfig.bits_per_pixel eq 8)
  widget_control, info.idbitbase, sensitive = (info.devconfig.color eq 1)

  ; Set encoding button
  widget_control, info.idisolatin1, set_button = (info.devconfig.isolatin1 eq 1)

  ; Set default filename.
  widget_control, info.idfilename, get_value = wfilename
  if string(wfilename[0]) ne info.devconfig.filename then begin
    widget_control, info.idfilename, set_value = info.devconfig.filename
    ; Put caret at end of pathname text so that filename itself is visible
    widget_control, info.idfilename, $
      set_text_select = [strlen(info.devconfig.filename), 0]
  endif

  ; Set protrait/landscape button.
  widget_control, info.IDLand, set_button = (info.devconfig.landscape eq 1)
  widget_control, info.idport, set_button = (info.devconfig.landscape eq 0)

  ; Set Paper
  pn = ps_form_papernames()
  xp = strtrim(string(info.xpagesize, format = '(F10.2)'), 2)
  yp = strtrim(string(info.ypagesize, format = '(F10.2)'), 2)
  un = 'in'
  if not info.devconfig.inches then un = 'cm'

  paperlab = string(pn[info.paperindex], xp, un, yp, un, $
    format = '("   Paper: ",A0," (",A0,A0," x ",A0,A0,")   ")')
  widget_control, info.idpaperlabel, set_value = paperlab

  return
end

pro ps_form_Null_Events, event
  compile_opt idl2
end
; *******************************************************************

function ps_form_What_Button_Type, event
  compile_opt idl2

  ; Checks event.type to find out what kind of button
  ; was clicked in a draw widget. This is NOT an event handler.

  type = ['DOWN', 'UP', 'MOTION', 'SCROLL']
  Return, type[event.type]
end
; *******************************************************************

function ps_form_What_Button_Pressed, event
  compile_opt idl2

  ; Checks event.press to find out what kind of button
  ; was pressed in a draw widget.  This is NOT an event handler.

  button = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT']
  Return, button[event.press]
end
; *******************************************************************

function ps_form_What_Button_Released, event
  compile_opt idl2

  ; Checks event.release to find out what kind of button
  ; was released in a draw widget.  This is NOT an event handler.

  button = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT']
  Return, button[event.release]
end
; *******************************************************************

;
; ps_form_NUMEVENTS
;
; Events sent to the numeric text field widgets are sent here.  We
; harvest the data values from the text field and update the screen.
;
pro ps_form_NumEvents, event
  compile_opt idl2

  ; If an event comes here, read the offsets and sizes from the
  ; form and draw the appropriately sized box in the draw widget.

  widget_control, event.top, get_uvalue = info, /no_copy

  ; Get current values for offset and sizes

  widget_control, info.idxsize, get_value = xsize
  widget_control, info.idysize, get_value = ysize
  widget_control, info.idxoff, get_value = xoff
  widget_control, info.idyoff, get_value = yoff

  xsize = xsize[0]
  ysize = ysize[0]
  xoff = xoff[0]
  yoff = yoff[0]

  if info.preserve_aspect eq 1 then begin
    if event.id eq info.idysize then xsize = ysize / info.aspect $
    else ysize = xsize * info.aspect
  endif

  ; Fold this information into the "info" structure
  ps_form_Update_Info, info, xsize = xsize, ysize = ysize, xoff = xoff, yoff = yoff

  ; Update form and redraw sample box
  ps_form_draw_form, info

  ; Put the info structure back into the top-level base

  widget_control, event.top, set_uvalue = info, /no_copy
end
; *******************************************************************

pro ps_form_Move_Box, event
  compile_opt idl2

  ; This is the event handler that allows the user to "move"
  ; the plot box around in the page window. It will set the
  ; event handler back to "ps_form_Box_Events" when it senses an
  ; "UP" draw button event and it will also turn ps_form_Draw_Motion_Events
  ; OFF.

  ; Get the info structure out of the top-level base.

  widget_control, event.top, get_uvalue = info, /no_copy

  whatButtonType = ps_form_What_Button_Type(event)

  dpu = info.drawpixperunit

  ixmin = 0.
  iymin = 0.
  ixsize = info.devconfig.xsize
  iysize = info.devconfig.ysize
  ps_form_Draw_Coords, dpu, ixmin, iymin, ixsize, iysize
  ; Now ixmin,iymin have the minimum values of x and y, in pixels
  ; ixsize and iysize are the size of the box, in pixels

  ixmax = info.xpagesize
  iymax = info.ypagesize
  ps_form_Draw_Coords, dpu, ixmax, iymax
  ; ixmax and iymax are the max values of x and y, in pixels

  ; info.ideltx/y contains the offset of the lower left corner of the box,
  ; with respect to the mouse's position
  ixoff = event.x + info.ideltx
  iyoff = event.y + info.idelty

  ; Keep box inside the page
  if ixoff lt ixmin then ixoff = ixmin
  if iyoff lt iymin then iyoff = iymin
  if (ixoff + ixsize) gt ixmax then ixoff = ixmax - ixsize
  if (iyoff + iysize) gt iymax then iyoff = iymax - iysize

  if whatButtonType eq 'UP' then begin
    ; When the button is "up" the moving event is over.  We reset the
    ; event function and update the information about the box's position

    widget_control, info.iddraw, draw_motion_events = 0, $ ; Motion events off
      event_pro = 'ps_form_Box_Events' ; Change to normal processing

    ps_form_Real_Coords, dpu, ixoff, iyoff, ixsize, iysize

    ; Update the info structure
    ps_form_Update_Info, info, xoff = ixoff, yoff = iyoff
    ; Draw it
    ps_form_draw_form, info

    ; Put the info structure back in the top-level base and RETURN
    widget_control, event.top, set_uvalue = info, /no_copy
    Return
  endif

  ; You come to this section of the code for all events except
  ; an UP button event. Most of the action in this event handler
  ; occurs here.

  ps_form_Real_Coords, dpu, ixoff, iyoff, ixsize, iysize

  ; Simply draw the new box
  ps_form_draw_box, ixsize, iysize, ixoff, iyoff, info

  ; Put the info structure back into the top-level base.
  widget_control, event.top, set_uvalue = info, /no_copy
end
; *******************************************************************

pro ps_form_Grow_Box, event
  compile_opt idl2

  ; This event handler is summoned when a RIGHT button is clicked
  ; in the draw widget. It allows the user to draw the outline of a
  ; box with the mouse. It will continue drawing the new box shape
  ; until an UP event is detected. Then it will set the event handler
  ; back to ps_form_Box_Events and turn ps_form_Draw_Motion_Events to OFF.

  ; Get the info structure out of the top-level base.

  widget_control, event.top, get_uvalue = info, /no_copy

  whatButtonType = ps_form_What_Button_Type(event)

  dpu = info.drawpixperunit

  ixmin = 0.
  iymin = 0.
  ixsize = info.devconfig.xsize
  iysize = info.devconfig.ysize
  ps_form_Draw_Coords, dpu, ixmin, iymin, ixsize, iysize
  ; Now ixmin,iymin have the minimum values of x and y, in pixels
  ; ixsize and iysize are the size of the box, in pixels

  ixmax = info.xpagesize
  iymax = info.ypagesize
  ps_form_Draw_Coords, dpu, ixmax, iymax
  ; ixmax and iymax are the max values of x and y, in pixels

  ; Keep box inside the page
  if event.x lt ixmin then event.x = ixmin
  if event.x gt ixmax then event.x = ixmax
  if event.y lt iymin then event.y = iymin
  if event.y gt iymax then event.y = iymax

  ; Decide on which corner is the lower left (it's arbitrary)
  ixoff = min([info.imousex, event.x])
  iyoff = min([info.imousey, event.y])
  ixsize = max([info.imousex, event.x]) - ixoff
  iysize = max([info.imousey, event.y]) - iyoff

  ; ; Enforce the aspect ratio
  if info.preserve_aspect eq 1 then begin
    sizeratio = info.aspect / (info.ypagesize / info.xpagesize)
    if (sizeratio ge 1) then ixsize = iysize / info.aspect $
    else iysize = ixsize * info.aspect
    if info.imousex gt event.x then ixoff = info.imousex - ixsize
    if info.imousey gt event.y then iyoff = info.imousey - iysize
  endif

  if whatButtonType eq 'UP' then begin
    ; When the button is "up" the moving event is over.  We reset the
    ; event function and update the information about the box's position

    widget_control, info.iddraw, draw_motion_events = 0, $ ; Motion events off
      event_pro = 'ps_form_Box_Events' ; Change to normal processing

    ps_form_Real_Coords, dpu, ixoff, iyoff, ixsize, iysize

    ; Update the info structure
    ps_form_Update_Info, info, xoff = ixoff, yoff = iyoff, $
      xsize = ixsize, ysize = iysize
    ; Draw it
    ps_form_draw_form, info

    ; Put the info structure back in the top-level base and RETURN
    widget_control, event.top, set_uvalue = info, /no_copy
    Return
  endif

  ; This is the portion of the code that handles all events except for
  ; UP button events. The bulk of the work is done here. Basically,
  ; you need to erase the old box and draw a new box at the new
  ; location. Just keep doing this until you get an UP event.

  ps_form_Real_Coords, dpu, ixoff, iyoff, ixsize, iysize

  ; Simply draw the new box
  ps_form_draw_box, ixsize, iysize, ixoff, iyoff, info

  ; Put the info structure back in the top-level base.
  widget_control, event.top, set_uvalue = info, /no_copy
end
; *******************************************************************

;
; Buttondown events sent to this procedure at first.  This is sets up
; the initial move/drag elements and hands off the events to the more
; specialized procedures ps_form_grow_box and ps_form_move_box above.
;
pro ps_form_Box_Events, event
  compile_opt idl2

  whatButtonType = ps_form_What_Button_Type(event)
  if whatButtonType ne 'DOWN' then Return

  ; Get info structure out of TLB

  widget_control, event.top, get_uvalue = info, /no_copy

  whatButtonPressed = ps_form_What_Button_Pressed(event)

  dpu = info.drawpixperunit
  ixmin = 0.
  iymin = 0.
  ixsize = info.devconfig.xsize
  iysize = info.devconfig.ysize
  ps_form_Draw_Coords, dpu, ixmin, iymin, ixsize, iysize
  ixmax = info.xpagesize
  iymax = info.ypagesize
  ps_form_Draw_Coords, dpu, ixmax, iymax
  ixoff = info.devconfig.xoff
  iyoff = info.devconfig.yoff
  ps_form_Draw_Coords, dpu, ixoff, iyoff

  if event.x lt ixmin or event.x gt ixmax $
  or event.y lt iymin or event.y gt iymax then begin
    widget_control, event.top, set_uvalue = info, /no_copy
    return
  endif

  case whatButtonPressed of
    'RIGHT': begin
      ; Resize the plot box interactively. Change the event handler
      ; to ps_form_Grow_Box. All subsequent events will be handled by
      ; ps_form_Grow_Box until an UP event is detected. Then you will
      ; return to this event handler. Also, turn motion events ON.

      widget_control, event.id, event_pro = 'ps_form_Grow_Box', $
        draw_motion_events = 1

      ps_form_draw_box, 1. / dpu, 1. / dpu, ixoff, iyoff, info

      info.imousex = event.x
      info.imousey = event.y
    end

    'LEFT': begin
      ; Resize the plot box interactively. Change the event handler
      ; to ps_form_Move_Box. All subsequent events will be handled by
      ; ps_form_Move_Box until an UP event is detected. Then you will
      ; return to this  event handler. Also, turn motion events ON.

      ; Only move the box if the cursor is inside the box.
      ; If it is NOT, then RETURN.

      if event.x lt ixoff or event.x gt (ixoff + ixsize) or $
        event.y lt iyoff or event.y gt (iyoff + iysize) then begin
        widget_control, event.top, set_uvalue = info, /no_copy
        Return
      endif

      ; Relocate the event handler and turn motion events ON.

      widget_control, event.id, event_pro = 'ps_form_Move_Box', $
        draw_motion_events = 1

      ; ideltx and idelty contain the offset of the lower left
      ; corner of the plot region with respect to the mouse.
      info.ideltx = ixoff - event.x
      info.idelty = iyoff - event.y
    end

    else: ; Middle button ignored in this program
  endcase

  ; Put the info structure back into the top-level base

  widget_control, event.top, set_uvalue = info, /no_copy
end
; *******************************************************************

;
; Handle events to the drop-list widgets, which contain predefined
; configurations.
;
pro ps_form_predef_events, event
  compile_opt idl2

  name = tag_names(event, /structure_name)
  if strupcase(name) ne 'WIDGET_DROPLIST' then return

  ; Get the info structure out of the top-level base
  widget_control, event.top, get_uvalue = info, /no_copy

  widget_control, event.id, get_uvalue = thislist

  ; Pre-read the values from the text fields
  widget_control, info.idfilename, get_value = filename
  ps_form_Update_Info, info, filename = filename

  case thislist of
    'PAPER': info.paperindex = event.index ; Paper change
    'PREDEF': begin
      old_filename = info.devconfig.filename ; Keep old filename
      info.devconfig = info.predefined(event.index) ; New config
      info.paperindex = info.papersizes(event.index) ; New paper too
      if info.filechanged then $
        info.devconfig.filename = old_filename $
      else begin
        cd, current = thisdir
        l = strlen(thisdir)
        if strmid(info.devconfig.filename, 0, l) ne thisdir then $
          info.devconfig.filename = old_filename $
        else $
          info.devconfig.filename = filepath(info.devconfig.filename, $
            root_dir = thisdir)
      endelse
    end
  endcase

  ; Be sure to select a pristine set of paper
  ps_form_select_papersize, info.paperindex, xpagesize, ypagesize, $
    landscape = info.devconfig.landscape, inches = info.devconfig.inches
  info.xpagesize = xpagesize
  info.ypagesize = ypagesize

  widget_control, info.idpaperlist, set_droplist_select = info.paperindex

  ; Reset the drawpixperunit value
  convfactor = 1.0
  if info.devconfig.inches eq 0 then convfactor = convfactor * 2.54
  info.marginsize = 0.25 * convfactor

  ; The conversion between length and pixels cannot always be set precisely,
  ; depending on the size of the paper
  dpp = 10.0 / convfactor ; Desire 10 pixels per inch
  if dpp * info.xpagesize gt info.xpixwinsize or $
    dpp * info.ypagesize gt info.ypixwinsize then $
    dpp = min([float(info.xpixwinsize - 2) / info.xpagesize, $
      float(info.ypixwinsize - 2) / info.ypagesize])
  info.drawpixperunit = dpp
  info.pixredraw = 1

  ; Update the info structure and draw it
  ps_form_Update_Info, info, xoff = info.devconfig.xoff
  ps_form_draw_form, info

  widget_control, event.top, set_uvalue = info, /no_copy
  return
end

;
; Handle events sent to any of the button elements of the form.
;
pro ps_form_Event, event
  compile_opt idl2

  ; This is the main event handler for ps_form. It handles
  ; the exclusive buttons on the form. Other events on the form
  ; will have their own event handlers.

  ; Get the name of the event structure

  name = tag_names(event, /structure_name)

  ; Get the User Value of the Button
  widget_control, event.id, get_uvalue = thisButton

  ; If name is NOT "WIDGET_BUTTON" or this is not a button
  ; selection event, RETURN.
  nonexclusive = (thisButton eq 'ISOLATIN1' or $
    thisButton eq 'COLOR' or $
    thisButton eq 'ENCAPSULATED' or $
    thisButton eq 'ASPECT')

  if name ne 'WIDGET_BUTTON' or $
    (not nonexclusive and event.select ne 1) then Return

  ; Get the info structure out of the top-level base
  widget_control, event.top, get_uvalue = info, /no_copy

  redraw_form = 0
  redraw_box = 0

  ; Pre-read the values from the text fields
  widget_control, info.idxsize, get_value = xsize
  widget_control, info.idysize, get_value = ysize
  widget_control, info.idxoff, get_value = xoff
  widget_control, info.idyoff, get_value = yoff
  widget_control, info.idfilename, get_value = filename
  ps_form_Update_Info, info, filename = filename

  ; Respond appropriately to whatever button was selected
  case thisButton of
    'INCHES': begin
      ps_form_Update_Info, info, xsize = xsize, ysize = ysize, $
        xoff = xoff, yoff = yoff
      ps_form_Update_Info, info, inches = 1
      redraw_form = 1
    end

    'CENTIMETERS': begin
      ps_form_Update_Info, info, xsize = xsize, ysize = ysize, $
        xoff = xoff, yoff = yoff
      ps_form_Update_Info, info, inches = 0
      redraw_form = 1
    end

    'COLOR': begin
      ps_form_Update_Info, info, color = (1 - info.devconfig.color)
      redraw_form = 1
    end

    'BITS2': begin
      ps_form_Update_Info, info, bits_per_pixel = 2
      redraw_form = 1
    end

    'BITS4': begin
      ps_form_Update_Info, info, bits_per_pixel = 4
      redraw_form = 1
    end

    'BITS8': begin
      ps_form_Update_Info, info, bits_per_pixel = 8
      redraw_form = 1
    end

    'ISOLATIN1': begin
      ps_form_Update_Info, info, isolatin1 = (1 - info.devconfig.isolatin1)
    end

    'ASPECT': begin
      if info.preserve_aspect eq 0 then $
        info.aspect = info.devconfig.ysize / info.devconfig.xsize
      info.preserve_aspect = (1 - info.preserve_aspect)
    end

    'LANDSCAPE': begin
      ps_form_Update_Info, info, xsize = xsize, ysize = ysize, $
        xoff = xoff, yoff = yoff
      ps_form_Update_Info, info, landscape = 1
      redraw_form = 1
      redraw_box = 1
    end

    'PORTRAIT': begin
      ps_form_Update_Info, info, landscape = 0
      ps_form_Update_Info, info, xsize = xsize, ysize = ysize, $
        xoff = xoff, yoff = yoff
      redraw_form = 1
      redraw_box = 1
    end

    'ENCAPSULATED': begin
      ps_form_Update_Info, info, encapsulated = (1 - info.devconfig.encapsulated)
    end

    'ACCEPT': begin
      ; The user wants to accept the information in the form.
      ; The procedure is to gather all the information from the
      ; form and then fill out a formInfo structure variable
      ; with the information. The formInfo structure is stored
      ; in a pointer. The reason for this is that we want the
      ; information to exist even after the form is destroyed.

      ; Gather the information from the form

      widget_control, info.idfilename, get_value = filename
      ps_form_Update_Info, info, xsize = xsize, ysize = ysize, $
        xoff = xoff, yoff = yoff
      ps_form_Update_Info, info, filename = filename
      widget_control, event.id, get_value = buttonname

      formInfo = { $
        cancel: 0, $ ; CANCEL flag
        create: 0, $ ; CREATE flag
        buttonname: buttonname, $
        xpagesize: info.xpagesize, $
        ypagesize: info.ypagesize, $
        paperindex: info.paperindex, $
        result: info.devconfig $ ; Results are ready-made
        }

      goto, finish_destroy
    end

    'CREATE': begin
      widget_control, info.idfilename, get_value = filename
      ps_form_Update_Info, info, xsize = xsize, ysize = ysize, $
        xoff = xoff, yoff = yoff
      ps_form_Update_Info, info, filename = filename

      formInfo = { $
        cancel: 0, $ ; CANCEL flag
        create: 1, $ ; CREATE flag
        buttonname: 'Create File', $
        xpagesize: info.xpagesize, $
        ypagesize: info.ypagesize, $
        paperindex: info.paperindex, $
        result: info.devconfig $ ; Results are ready-made
        }

      goto, finish_destroy
    end

    'CANCEL': begin
      ; The user wants to cancel out of this form. We need a way to
      ; do that gracefully. Our method here is to set a "cancel"
      ; field in the formInfo structure.
      formInfo = {cancel: 1, create: 0}

      goto, finish_destroy
    end
  endcase

  if redraw_form eq 1 then $
    ps_form_draw_form, info, nobox = (1 - redraw_box)

  ; Put the info structure back into the top-level base if the
  ; base is still in existence.

  if widget_info(event.top, /valid) then $
    widget_control, event.top, set_uvalue = info, /no_copy
  return

  ; We only reach this stage if we are ending the ps_form widget
  ; These commands store the results, restore colors, and destroy
  ; the form widget.
  finish_destroy:

  ; Put the formInfo structure into the location pointer
  ; to by the pointer
  Handle_Value, info.ptrresult, formInfo, /set, /no_copy

  ; Delete the pixmap window
  wdelete, info.idpixwid

  ; Restore the user's color table
  tvlct, info.red, info.green, info.blue

  ; Destroy the ps_form widget program
  widget_control, event.top, /destroy

  return
end
; *******************************************************************

function ps_form, xoffset, yoffset, cancel = cancelButton, help = help, $
  xsize = xsize, ysize = ysize, xoffset = xoff, yoffset = yoff, $
  inches = inches, color = color, bits_per_pixel = bits_per_pixel, $
  encapsulated = encapsulated, landscape = landscape, filename = filename, $
  defaults = defaults, localdefaults = localDefaults, initialize = initialize, $
  select = select, parent = parent, $
  create = createButton, nocommon = nocommon, papersize = paperSize, $
  button_names = buttons, button_sel = button_sel, $
  predefined = predefined, defaultpaper = defaultpaper, $
  aspect = aspect, preserve_aspect = preserve_aspect, $
  xpagesize = xpagesize, ypagesize = ypagesize, pagebox = pagebox
  compile_opt idl2

  ; If the Help keyword is set, print some help information and return

  if keyword_set(help) then begin
    doc_library, 'ps_form'
    RETURN, 0
  endif

  ; Set cancelButton and createButton as if canceled, so will be defined
  ; (and with appropriate values) even if user kills the window instead of
  ; using the buttons.  Normal exit will reassign them later on.
  cancelButton = 1
  createButton = 0

  ; Load default setups via a common block, if they are available
  if n_elements(predefined) eq 0 then begin
    common ps_form_configs, ps_form_default_papersize, $
      ps_form_stdconfigs
    if n_elements(ps_form_stdconfigs) gt 0 then $
      predefined = ps_form_stdconfigs
  endif

  ; If the user has not set up a common block, then get some pre
  if n_elements(predefined) eq 0 then $
    ps_form_load_configs, ps_form_default_papersize, predefined

  ; Transfer to local copies so that we don't overwrite
  confignames = predefined[*].configname
  configs = predefined[*].config
  configs = configs[*] ; ; IDL 5.5 will make a 1xN array -- collapse it now
  papernames = predefined[*].papersize
  if n_elements(defaultpaper) eq 0 $
    and n_elements(ps_form_default_papersize) gt 0 then $
    defaultpaper = ps_form_default_papersize
  if n_elements(defaultpaper) eq 0 then $
    defaultpaper = 'Letter'

  papersizes = intarr(n_elements(papernames))

  ; If localdefaults exist, then enter them into a new first entry of
  ; the configuration list
  if n_elements(localDefaults) ne 0 then begin
    configs = [configs[0], configs]
    confignames = ['Local', confignames]
    papernames = [defaultpaper, papernames]
    papersizes = [0, papersizes]

    tmpc = configs[0]
    struct_assign, localDefaults, tmpc, /nozero
    configs[0] = tmpc
  endif

  ; Generate a new entry at the beginning, which will be the initial,
  ; default configuration.
  configs = [configs[0], configs]
  confignames = ['Default', confignames]
  papernames = [defaultpaper, papernames]
  papersizes = [0, papersizes]

  filechanged = 0
  defaultset = 0
  if n_elements(defaults) ne 0 then begin
    defaultset = 1
    tmpc = configs[0]
    struct_assign, defaults, tmpc, /nozero
    configs[0] = tmpc
    void = where(strupcase(tag_names(defaults)) eq 'FILENAME', count)
    if (count ne 0) then filechanged = 1
  endif

  ; Next, enter in the keyword defaults
  if not defaultset or n_elements(inches) gt 0 then begin
    if n_elements(inches) eq 0 then inches = 1
    configs[0].inches = keyword_set(inches)
  endif
  if not defaultset or n_elements(landscape) gt 0 then $
    configs[0].landscape = keyword_set(landscape)
  if not defaultset or n_elements(color) gt 0 then $
    configs[0].color = keyword_set(color)
  if not defaultset or n_elements(encapsulated) gt 0 then $
    configs[0].encapsulated = keyword_set(encapsulated)

  if not defaultset or n_elements(bits_per_pixel) gt 0 then begin
    if n_elements(bits_per_pixel) eq 0 then bpp = 8 else bpp = bits_per_pixel
    if bpp lt 1 then bpp = 2
    if bpp gt 2 and bpp lt 4 then bpp = 4
    if bpp gt 4 and bpp lt 8 then bpp = 8
    if bpp gt 8 then bpp = 8
    configs[0].bits_per_pixel = bpp
  endif

  if n_elements(filename) eq 0 then begin
    if not filechanged then begin
      cd, current = thisDir
      filename = filepath('idl.ps', root_dir = thisDir)
      filechanged = 0
      configs[0].filename = filename
    endif
  endif else begin
    configs[0].filename = filename
    filechanged = 1
  endelse

  ; Get the size of the page, based on the papersize keyword
  if n_elements(paperSize) gt 1 then begin
    xpagesize = float(paperSize[0])
    ypagesize = float(paperSize[1])
    pind = 0
  endif else begin
    if n_elements(paperSize) eq 0 then paperSize = defaultpaper
    ps_form_select_papersize, paperSize, xpagesize, ypagesize, $
      landscape = configs[0].landscape, inches = configs[0].inches, index = pind
  endelse

  convfactor = 1.0
  if configs[0].inches eq 0 then convfactor = convfactor * 2.54
  defmarginsize = 1.50 * convfactor ; 1 1/2 inch margins default

  if n_elements(marginsize) eq 0 then $
    marginsize = 0.25 * convfactor ; 1/4 inch margins "minimum"

  ; "Unconvert" the configuration xoff, yoff, etc. into human-readable format,
  ; which is also the format of the keywords xoff and yoff passed to ps_form()

  nconfigs = n_elements(configs)
  for j = 0, nconfigs - 1 do begin
    ps_form_select_papersize, papernames[j], tmpxpg, tmpypg, $
      landscape = configs[j].landscape, inches = configs[j].inches, $
      index = pind

    papersizes[j] = pind
    tmpc = configs[j]
    ps_form_conv_pscoord, tmpc, tmpxpg, tmpypg, /tohuman
    configs[j] = tmpc
  endfor

  if n_elements(aspect) gt 0 then aspect = aspect[0] > .001
  if n_elements(ysize) gt 0 then ysize = ysize[0]
  if n_elements(xsize) gt 0 then xsize = xsize[0]
  if n_elements(xsize) gt 0 and n_elements(ysize) gt 0 then $
    aspect = ysize / (xsize > (ysize * 0.001)) $
  else if n_elements(xsize) gt 0 and n_elements(aspect) gt 0 then $
    ysize = xsize * aspect $
  else if n_elements(ysize) gt 0 and n_elements(aspect) gt 0 then $
    xsize = ysize / aspect

  ; Compute an intelligent default X and Y size, if they aren't given
  pageaspect = xpagesize / ypagesize

  if not defaultset then begin
    if n_elements(xsize) eq 0 and n_elements(ysize) eq 0 then begin
      if n_elements(aspect) eq 0 then begin
        if !d.window ne -1 then $
          aspect = float(!d.x_vSize) / !d.y_vSize $
        else $
          aspect = 1.0
      endif

      if aspect gt 1.0 then begin
        configs[0].xsize = xpagesize - 2.0 * marginsize
        configs[0].ysize = configs[0].xsize / aspect
      endif else begin
        configs[0].ysize = ypagesize - 2.0 * marginsize
        configs[0].xsize = configs[0].ysize * aspect
      endelse
    endif
    if n_elements(xsize) eq 0 then $
      configs[0].xsize = 7.0 * convfactor
    if n_elements(ysize) eq 0 then $
      configs[0].ysize = 5.0 * convfactor
    if n_elements(xoff) eq 0 then $
      configs[0].xoff = (xpagesize - configs[0].xsize) / 2.0
    if n_elements(yoff) eq 0 then $
      configs[0].yoff = (ypagesize - configs[0].ysize) / 2.0

    configs[0].xsize = marginsize > configs[0].xsize < (xpagesize - 2. * marginsize)
    configs[0].ysize = marginsize > configs[0].ysize < (ypagesize - 2. * marginsize)
    configs[0].xoff = marginsize > configs[0].xoff < (xpagesize - configs[0].xsize)
    configs[0].yoff = marginsize > configs[0].yoff < (ypagesize - configs[0].ysize)
  endif

  if keyword_set(preserve_aspect) then begin
    if n_elements(xsize) eq 0 then xsize = configs[0].xsize
    if n_elements(ysize) eq 0 then ysize = configs[0].ysize
    aspect = ysize / (xsize > (ysize * 0.001))
  endif

  if n_elements(xsize) gt 0 then configs[0].xsize = xsize
  if n_elements(ysize) gt 0 then configs[0].ysize = ysize
  if n_elements(xoff) gt 0 then configs[0].xoff = xoff
  if n_elements(yoff) gt 0 then configs[0].yoff = yoff
  if n_elements(aspect) eq 0 then aspect = configs[0].ysize / configs[0].xsize

  ; Return the initialized information, if that's all they were asking
  ; for.  Must convert back to "IDL" coordinates.
  if keyword_set(initialize) then begin
    sel = 0
    if n_elements(select) gt 0 then begin
      selen = strlen(select)
      wh = where(strupcase(strmid(confignames, 0, selen)) eq $
        strupcase(select), ct)
      if ct gt 0 then sel = wh[0]
    endif
    ps_form_select_papersize, papernames[sel], tmpxpg, tmpypg, $
      landscape = configs[sel].landscape, inches = configs[sel].inches
    tmpc = configs[sel]
    xpagesize = tmpxpg
    ypagesize = tmpypg
    pagebox = [(0 - tmpc.xoff) / tmpc.xsize, $
      (0 - tmpc.yoff) / tmpc.ysize, $
      (xpagesize - tmpc.xoff) / tmpc.xsize, $
      (ypagesize - tmpc.yoff) / tmpc.ysize]
    ps_form_conv_pscoord, tmpc, tmpxpg, tmpypg, /toidl
    cancelButton = 0
    createButton = 0
    return, tmpc
  endif

  ; This program cannot work if the graphics device is already set
  ; to PostScript. So if it is, set it to the native OS graphics device.
  ; Remember to set it back later.

  if !d.name eq 'PS' then begin
    oldName = 'PS'
    thisDevice = byte(!version.os)
    thisDevice = strupcase(thisDevice[0 : 2])
    if thisDevice eq 'MAC' or thisDevice eq 'WIN' then set_plot, thisDevice $
    else set_plot, 'X'
  endif else oldName = !d.name

  ; Check for optional offset parameters and give defaults if not passed

  device, get_screen_size = screenSize
  if n_elements(xoffset) eq 0 then xoffset = (screenSize[0] - 600) / 2.
  if n_elements(yoffset) eq 0 then yoffset = (screenSize[1] - 400) / 2.

  ; The draw widget will have the following dimensions
  xpixwinsize = 174
  ypixwinsize = 174 ; Hopefully will fit 11" x 17" sized paper

  ; The conversion between length and pixels cannot always be set precisely,
  ; depending on the size of the paper
  dpp = 10.0 / convfactor ; Desire 10 pixels per inch
  if dpp * xpagesize gt xpixwinsize or dpp * ypagesize gt ypixwinsize then $
    dpp = min([float(xpixwinsize - 2) / xpagesize, $
      float(ypixwinsize - 2) / ypagesize])

  ; Start building the widgets
  thisRelease = strmid(!version.release, 0, 1)
  if thisRelease eq '5' and n_elements(parent) gt 0 then $
    extra_modal = {modal: 1, group_leader: parent[0]}
  tlb0 = widget_base(title = 'Configure PostScript Parameters', column = 1, $
    xoffset = xoffset, yoffset = yoffset, tlb_frame_attr = 9, $
    _extra = extra_modal)

  ; Sub-bases for layout
  tlb = widget_base(tlb0, column = 1, align_center = 1, frame = 1)

  sizebase = widget_base(tlb, row = 1, align_center = 1)

  numbase = widget_base(sizebase, column = 1)

  numsub1 = widget_base(numbase, row = 1)

  junk = widget_label(numsub1, value = ' Units: ')
  junksub = widget_base(numsub1, row = 1, /exclusive)
  inch = widget_button(junksub, value = 'Inches', uvalue = 'INCHES')
  cm = widget_button(junksub, value = 'Centimeters', $
    uvalue = 'CENTIMETERS')

  numsub2 = widget_base(numbase, row = 1, event_pro = 'ps_form_NumEvents')

  xbase = widget_base(numsub2, column = 1, base_align_right = 1)
  x1base = widget_base(xbase, row = 1)
  junk = widget_label(x1base, value = 'XSize: ')
  xsizew = widget_text(x1base, scr_xsize = 60, /editable, $
    value = '')

  x2base = widget_base(xbase, row = 1)
  junk = widget_label(x2base, value = 'XOffset: ')
  xoffw = widget_text(x2base, scr_xsize = 60, /editable, $
    value = '')

  ybase = widget_base(numsub2, column = 1, base_align_right = 1)
  y1base = widget_base(ybase, row = 1)
  junk = widget_label(y1base, value = 'YSize: ')
  ysizew = widget_text(y1base, scr_xsize = 60, /editable, $
    value = '')

  y2base = widget_base(ybase, row = 1)
  junk = widget_label(y2base, value = 'YOffset: ')
  yoffw = widget_text(y2base, scr_xsize = 60, /editable, $
    value = '')

  paperw = widget_label(numbase, $
    value = '                                        ')
  dummy = widget_base(numbase, column = 1, /nonexclusive)
  aspectw = widget_button(dummy, value = 'Preserve Aspect', uvalue = 'ASPECT')

  drawbase = widget_base(sizebase, row = 1, frame = 1)

  draw = widget_draw(drawbase, xsize = xpixwinsize, ysize = ypixwinsize, $
    event_pro = 'ps_form_Box_Events', button_events = 1)

  opttlb = widget_base(tlb, row = 1, align_center = 1, xpad = 20)

  orientbase = widget_base(opttlb, column = 1, base_align_center = 1)

  junk = widget_label(orientbase, value = 'Orientation: ')
  junkbase = widget_base(orientbase, column = 1, /frame, /exclusive)
  land = widget_button(junkbase, value = 'Landscape', uvalue = 'LANDSCAPE')
  port = widget_button(junkbase, value = 'Portrait', uvalue = 'PORTRAIT')

  optbase = widget_base(opttlb, column = 1, /nonexclusive, frame = 1)
  colorbut = widget_button(optbase, value = 'Color Output', $
    uvalue = 'COLOR')
  encap = widget_button(optbase, value = 'Encapsulated (EPS)', $
    uvalue = 'ENCAPSULATED')
  isolatin1 = widget_button(optbase, value = 'ISOLatin1 Encoding', $
    uvalue = 'ISOLATIN1')

  ; bitslabel = Widget_Label(opttlb, Value='   Color Bits:')

  bitsw = widget_base(opttlb, column = 1, /exclusive, /frame)

  bit2 = widget_button(bitsw, value = '2 Bit Color', uvalue = 'BITS2')
  bit4 = widget_button(bitsw, value = '4 Bit Color', uvalue = 'BITS4')
  bit8 = widget_button(bitsw, value = '8 Bit Color', uvalue = 'BITS8')

  filenamebase = widget_base(tlb, column = 1, align_center = 1)
  fbase = widget_base(filenamebase, row = 1)
  textlabel = widget_label(fbase, value = 'Filename: ')

  ; Set up text widget with an event handler that ignores any event.

  filenamew = widget_text(fbase, /editable, scr_xsize = 300, $
    value = '', event_pro = 'ps_form_Null_Events')
  filenameb = widget_button(fbase, value = 'Choose...', $
    event_pro = 'ps_form_select_file')

  ; This is a base for selection of predefined configurations and paper sizes
  predefbase = widget_base(tlb0, row = 1, /align_center, frame = 1)
  junk = widget_label(predefbase, value = 'Predefined:')
  predlist = widget_droplist(predefbase, value = confignames, $
    event_pro = 'ps_form_predef_events', uvalue = 'PREDEF')
  junk = widget_label(predefbase, value = '    Paper Sizes:')
  paplist = widget_droplist(predefbase, value = ps_form_papernames(), $
    event_pro = 'ps_form_predef_events', uvalue = 'PAPER')

  actionbuttonbase = widget_base(tlb0, row = 1, /align_center)
  cancel = widget_button(actionbuttonbase, value = 'Cancel', uvalue = 'CANCEL')
  if n_elements(buttons) gt 0 then begin
    for i = 0, n_elements(buttons) - 1 do begin
      but = widget_button(actionbuttonbase, value = buttons[i], $
        uvalue = 'ACCEPT')
    endfor
  endif else begin
    create = widget_button(actionbuttonbase, value = 'Create File', $
      uvalue = 'CREATE')
    accept = widget_button(actionbuttonbase, value = 'Accept', $
      uvalue = 'ACCEPT')
  endelse

  ; Modify the color table
  ; Get the colors in the current color table
  tvlct, r, g, b, /get

  ; Modify color indices N_Colors-2, N_Colors-3 and N_Colors-4 for
  ; drawing colors

  ; The number of colors in the session can be less then the
  ; number of colors in the color vectors on PCs (and maybe other
  ; computers), so take the smaller value. (Bug fix?)
  ncol = !d.n_colors < n_elements(r)
  red = r
  green = g
  blue = b
  red[ncol - 4 : ncol - 2] = [70b, 0b, 255b]
  green[ncol - 4 : ncol - 2] = [70b, 255b, 255b]
  blue[ncol - 4 : ncol - 2] = [70b, 0b, 0b]

  ; Load the newly modified colortable
  tvlct, red, green, blue

  ; Create a reserve pixmap for keeping backing store
  owin = !d.window
  Window, /free, xsize = xpixwinsize, ysize = ypixwinsize, /pixmap
  pixwid = !d.window

  ; Create a handle.  This will hold the result after the widget finishes
  ptr = Handle_Create()

  info = { $
    devconfig: configs[0], $
    iddraw: draw, $
    idpixwid: pixwid, $
    idwid: pixwid, $
    idtlb: tlb0, $
    idxsize: xsizew, $
    idysize: ysizew, $
    idxoff: xoffw, $
    idyoff: yoffw, $
    idfilename: filenamew, $
    idinch: inch, $
    idcm: cm, $
    idcolor: colorbut, $
    idbitbase: bitsw, $
    idbit2: bit2, $
    idbit4: bit4, $
    idbit8: bit8, $
    idisolatin1: isolatin1, $
    idencap: encap, $
    idlAnd: land, $
    idport: port, $
    idpaperlabel: paperw, $
    idaspect: aspectw, $
    idpaperlist: paplist, $
    xpagesize: xpagesize, $
    ypagesize: ypagesize, $
    paperindex: pind, $
    marginsize: marginsize, $
    xpixwinsize: xpixwinsize, $
    ypixwinsize: ypixwinsize, $
    drawpixperunit: dpp, $
    filechanged: filechanged, $
    pixredraw: 1, $
    imousex: 0.0, $
    imousey: 0.0, $
    ideltx: 0.0, $
    idelty: 0.0, $
    pagecolor: ncol - 2, $
    boxcolor: ncol - 3, $
    bkgcolor: ncol - 4, $
    red: r, $
    green: g, $
    blue: b, $
    ptrresult: ptr, $
    predefined: configs, $
    papersizes: papersizes, $
    defaultpaper: defaultpaper, $
    aspect: aspect, $
    preserve_aspect: keyword_set(preserve_aspect) $
    }

  ps_form_draw_form, info, /nobox
  widget_control, tlb0, /realize
  widget_control, draw, get_value = wid
  info.idwid = wid

  ; ; Make sure the current info is consistent
  ps_form_Update_Info, info
  ; Draw the remaining widgets
  widget_control, paplist, set_droplist_select = pind
  ps_form_draw_form, info

  ; Store the info structure in the top-level base

  widget_control, tlb0, set_uvalue = info, /no_copy

  ; Set this widget program up as a modal or blocking widget. What this means
  ; is that you will return to the line after this XManager call when the
  ; widget is destroyed.

  thisRelease = strmid(!version.release, 0, 1)
  if thisRelease eq '4' then $
    xmanager_modal = {modal: 1}
  xmanager, 'ps_form', tlb0, _extra = xmanager_modal

  ; Get the formInfo structure from the pointer location.

  Handle_Value, ptr, formInfo, /no_copy

  ; Make sure the user didn't click a close button.

  if n_elements(formInfo) eq 0 then begin
    Handle_Free, ptr
    RETURN, 0
  endif

  ; Strip the CANCEL field out of the formInfo structure so the
  ; cancelButton flag can be returned via the CANCEL keyword and the
  ; formInfo structure is suitable for passing directly to the DEVICE
  ; procedure through its _Extra keyword.

  cancelButton = formInfo.cancel
  createButton = formInfo.create
  if not cancelButton then begin
    xpagesize = formInfo.xpagesize
    ypagesize = formInfo.ypagesize
    paperindex = formInfo.paperindex
    if n_elements(buttons) gt 0 then $
      button_sel = formInfo.buttonname
    formInfo = formInfo.result

    paperSize = ps_form_papernames()
    paperSize = paperSize[paperindex]
    pagebox = [(0 - formInfo.xoff) / formInfo.xsize, $
      (0 - formInfo.yoff) / formInfo.ysize, $
      (xpagesize - formInfo.xoff) / formInfo.xsize, $
      (ypagesize - formInfo.yoff) / formInfo.ysize]
    ps_form_conv_pscoord, formInfo, xpagesize, ypagesize, /toidl
  endif else $
    formInfo = 0

  ; Free up the space allocated to the pointers and the data

  Handle_Free, ptr

  if owin ge 0 then wset, owin
  set_plot, oldName

  RETURN, formInfo
end
; *******************************************************************