! This macro is intended to test out the new PLOT keyword for ZPL.
! The calculations are meant to work with the sample file Cooke 40 degree field.zmx
! located in the \ZEMAX\Samples\Sequential\Objectives\ directory.
!
! Written by S. Gangadhara, 12/12/07
!

! Determine number of surfaces in the system

n_surf = NSUR()

! Define array variables to be plotted

DECLARE x1, DOUBLE, 1, 21					# Variables to store OPD data
DECLARE y1, DOUBLE, 1, 21
DECLARE X2, DOUBLE, 1, n_surf					# Variables to store OPL data
DECLARE y2, DOUBLE, 1, n_surf

! Compute array variables using RAYTRACE, OPDC and OPTH

FOR i, 1, 21, 1
	x1(i) = 0.1*i-1.1
	RAYTRACE 0, 0, 0, 0.1*i-1.1, 1
	y1(i) = OPDC()
NEXT i

FOR i, 1, n_surf, 1
	x2(i) = (i - 3.0)/2.0
	RAYTRACE 0, 0, 0, 0.1*i-1.1, 1
	y2(i) = OPTH(i)/50. - 2.0
NEXT i

! Plot results to the screen using PLOT keywords

title$ = "PLOT TEST WITH COOKE 40 DEGREE FIELD"			# Plot Title
xtitle$ = "PUPIL COORDINATE / RELATIVE SURFACE NUMBER"		# X-axis label
ytitle$ = "WAVES / LENGTH"					# Y-axis label
banner$ = "RESULTS GENERATED USING PLOT_TEST.ZPL"		# Plot Banner
comment1$ = "DATA FOR OPD ARE EXACT"				# Plot comment line 1
comment2$ = "DATA FOR OPTICAL PATH LENGTH (OPL) ARE NORMALIZED"	# Plot comment line 2
comment3$ = "X-AXIS FOR OPL DATA IS 0.5*(SURFACE NUMBER - 2)"	# Plot comment line 3
comment4$ = "Y-AXIS FOR OPL DATA IS LENGTH/50 - 2"		# Plot comment line 4
comment5$ = "NORMALIZATION OF THE OPL DATA WAS PERFORMED SO THAT IT"	# Plot comment line 5
comment6$ = "  COULD BE PLOTTED ON THE SAME GRAPH AS THE OPD DATA"	# Plot comment line 6
label1$ = "OPD FOR ON-AXIS FIELD POINT"				# Graphic label to go with OPD curve
label2$ = "OPTICAL PATH LENGTH (OPL)"				# Graphic label to go with OPL curve
x_min = -2.0				# X-axis minimum value
y_min = -2.0				# Y-axis minimum value
x_max = 2.0				# X-axis maximum value
y_max = 0.0				# Y-axis maximum value
x_chk = 0.005				# X-size of plot symbols (in display units, with 1.0 being full x-length of window)
y_chk = 0.005				# Y-size of plot symbols (in display units)
x_tick = 0.5				# X-axis distance between tick marks (in x-axis units)
y_tick = 0.4				# Y-axis distance between tick marks (in y-axis units)
x_form$ = "%4.2f"			# Format string for X-axis numbers
y_form$ = "%4.2f"			# Format string for Y-axis numbers
xl1 = 0.1				# X-position for start of line 1 (in display units)
yl1 = 0.6				# Y-position for start of line 1 (in display units)
xl2 = 0.8				# X-position for end of line 1 (in display units)
yl2 = 0.6				# Y-position for end of line 1 (in display units)
xl3 = 0.45				# X-position for start of line 2 (in display units)
yl3 = 0.3				# Y-position for start of line 2 (in display units)
xl4 = 0.45				# X-position for end of line 2 (in display units)
yl4 = 0.9				# Y-position for end of line 2 (in display units)
xlb1 = 0.15				# X-position for start of label 1 (in display units)
xlb2 = 0.30				# X-position for start of label 2 (in display units)
ylb1 = 0.65				# Y-position for start of label 1 (in display units)
ylb2 = 0.35				# Y-position for start of label 2 (in display units)
ang1 = 0				# Angle at which label 1 will be tilted in the +x direction (degrees)
ang2 = 15				# Angle at which label 2 will be tilted in the +x direction (degrees)
scl = 1					# Scale factor which sets the size of the labels

PLOT NEW				# Initialize a new plot
PLOT TITLE, title$			# Place the title on the plot
PLOT TITLEX, xtitle$			# Place the x-axis title on the plot
PLOT TITLEY, ytitle$			# Place the y-axis title on the plot
PLOT BANNER, banner$			# Place the banner on the plot
PLOT COMM1, comment1$			# Place the comments on the plot
PLOT COMM2, comment2$
PLOT COMM3, comment3$
PLOT COMM4, comment4$
PLOT COMM5, comment5$
PLOT COMM6, comment6$
PLOT RANGEX, x_min, x_max		# Define the x-range for the plot
PLOT RANGEY, y_min, y_max		# Define the y-range for the plot
PLOT CHECK, x_chk, y_chk		# Define the plot symbol sizes
PLOT TICK, x_tick, y_tick		# Define the distance between the x- and y-tick marks
PLOT FORMATX, x_form$			# Define the formatting for the x- and y-axis numbers
PLOT FORMATY, y_form$
PLOT DATA, x1, y1, 21, 1, 1, 1		# Plot the OPD data using all of the points in the array, using pen color 1 (blue), making the curve dashed type 1, and plotting both a line and symbols
PLOT DATA, x2, y2, n_surf, 3, 2, 0	# Plot the OPL data using all of the points in the array, using pen color 3 (red), making the curve dashed type 2, and plotting just a line
PLOT LINE, xl1, yl1, xl2, yl2		# Plot a horizontal line (line 1)
PLOT LINE, xl3, yl3, xl4, yl4		# Plot a vertical line (line 2)
PLOT LABEL, xlb1, ylb1, ang1, scl, label1$	# Place label 1 on the plot (for the OPD curve)
PLOT LABEL, xlb2, ylb2, ang2, scl, label2$	# Place label 2 on the plot (for the OPL curve)
PLOT GO					# Generate the plot

! Release memory associated with array variables

RELEASE x1
RELEASE y1
RELEASE x2
RELEASE y2

! End program

PRINT "End of program."

END
