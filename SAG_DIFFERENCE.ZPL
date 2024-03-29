
# Make sure your starting file is the nominal file.
# The macro takes your starting file as 'nominal' and extracts Sag data based on the inputs
# It then opens MC files from the path defined in Line 10 and extracts the same data.
# The arrays are subtracted to yield a difference and then PTV and RMS data is reported for each file.
# Assumes a rectangular aperture on a surface named "freeform"
# Written by C. Battersby, December 2015, Revision 1.

STARTFILE$ = $FILEPATH()
MCPATH$ = "C:\Users\Betsy.Goodwin\Documents\New Course Material\DMAFUO\MC_T"
INPUT "NUMBER OF FILES TO ANALYZE (1-9999): ", FILES
INPUT "X Sag Points: ", XP
INPUT "Y Sag Points: ", YP

!XP = 10  #number of x data points
!YP = 10  #number of y data points
PTS = XP * YP

DECLARE sag, DOUBLE, 1, PTS
DECLARE MCsag, DOUBLE, 1, PTS
DECLARE sagdiff, DOUBLE, 1, PTS

N = SURC("Freeform")
AX = APMN(N)
AY = APMX(N)
SD = SQRT((AX*AX)+(AY*AY))
SETSURFACEPROPERTY N, 6, SD

T1$ = $TAB()
T2$ = T1$ + T1$
T3$ = T2$ + T1$

! Define Scans for X and Y
AXstep = (AX*2)/(XP-1)
AXstart = (AX*-1) 
AXstop = AX

AYstep = (AY*2)/(YP-1)
AYstart = (AY*-1) 
AYstop = AY

! PRINT AXstart, T2$, AXstop, T2$, AXstep, T2$, XP, " steps", T2$, AYstart, T2$, AYstop, T2$, AYstep, T2$, YP, " steps"

! scan x and y
FOR Y, 1, YP, 1
	IF (Y == 1)
		FOR X, 1, XP, 1
			mult = X-1
			Ypos = AYstart
			Xpos = AXstart + (mult * (AXstep))
			sag(X) = SAGG(Xpos, Ypos, N)
			!PRINT X, T2$, Xpos, T3$, Ypos, T3$, sag(X)
		NEXT
	ENDIF
	IF (Y > 1)
		counterY = (XP * (Y-1))
		multY = Y-1
		FOR X, 1, XP, 1
			mult2 = X-1
			count = counterY + X
			Ypos = AYstart + (multY * AYstep)
			Xpos = AXstart + (mult2 * AXstep)
			sag(count) = SAGG(Xpos, Ypos, N)
			!PRINT count, T2$, Xpos, T3$, Ypos, T3$, sag(count), T3$
		NEXT
	ENDIF	
NEXT


!FOR k, 1, PTS, 1
!	FORMAT 8.3
!	PRINT sag(k)
!NEXT

! Extract data from MC Files, take difference from nominal, and report RMS Sag Difference
! Prep print

FORMAT 4.0 INT
PRINT "Number of Data Points used for Sag: ", T2$, PTS
PRINT "Number of Monte Carlo files Analyzed: ", FILES
PRINT "File", T2$, "Sag Diff Max", T2$, "Sag Diff Min", T1$, "Sag Diff PTV", T1$, "Sag Diff RMS"

FOR i, 1, FILES, 1
	FORMAT "%#04i" LIT
	MCFILE$ = MCPATH$ + $STR(i) + ".ZMX"
	LOADLENS MCFILE$

N1 = SURC("Freeform")
	! scan x and y
! scan x and y
FOR Y, 1, YP, 1
	IF (Y == 1)
		FOR X, 1, XP, 1
			mult = X-1
			Ypos = AYstart
			Xpos = AXstart + (mult * (AXstep))
			MCsag(X) = SAGG(Xpos, Ypos, N1)
			!PRINT X, T2$, Xpos, T3$, Ypos, T3$, sag(X)
		NEXT
	ENDIF
	IF (Y > 1)
		counterY = (XP * (Y-1))
		multY = Y-1
		FOR X, 1, XP, 1
			mult2 = X-1
			count = counterY + X
			Ypos = AYstart + (multY * AYstep)
			Xpos = AXstart + (mult2 * AXstep)
			MCsag(count) = SAGG(Xpos, Ypos, N1)
			!PRINT count, T2$, Xpos, T3$, Ypos, T3$, sag(count), T3$
		NEXT
	ENDIF	
NEXT

! Scan through arrays and create third array of the differences	
FOR m, 1, PTS, 1
	sagdiff(m) = sag(m) - MCsag(m)
NEXT

! Scan difference array calculating the RMS value
SumSQ = 0
FOR p, 1, PTS, 1
	SumSQ = SumSQ + (sagdiff(p)*sagdiff(p))
NEXT
div = SumSQ / PTS
RMS = SQRT(div)

! Scan P-V 
max = 0
min = 0
!MAX loop
FOR z, 1, PTS, 1
	IF (sagdiff(z) > max)
		max = sagdiff(z)
	ENDIF
	IF (sagdiff(z) < min)
		min = sagdiff(z)
	ENDIF
NEXT
PTV = max - min
FORMAT 5.0 INT
PRINT i, T2$,
FORMAT 8.5
PRINT max, T3$, min, T2$, PTV, T2$, RMS 
NEXT

! Clean up
LOADLENS STARTFILE$

RELEASE sag
RELEASE MCsag
RELEASE sagdiff

END
