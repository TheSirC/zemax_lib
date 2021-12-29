! Version: 2.06.4
! August 28, 2019

! NOTE: This Macro constructs Zemax OS Optical system using Code V seq file

! Code V file location(s):
A$ = $CALLSTR(1)
IF SLEN(A$) == 0
    A$ = "sample01.seq"													# Comment the next line if you prefer not to use dialog
    BROWSE "Enter Code V file name", A$, 0, ".seq"						# Dialog window to input the name of seq-file
ENDIF
ATITLE$ = A$

    
PRINT
PRINT  "Macro to convert Code V(TM) sequence files to ZEMAX OpticStudio(TM)"
PRINT  
PRINT  "Warning! Not all Code V commands are supported. Complex systems may require "
PRINT  "additional editing within ZEMAX OpticStudio."
PRINT  
PRINT  "E-mail conversion suggestions to support@zemax.com"
PRINT
	
! New Zemax OS File to save the Code V exported file:
IF (SLEN(ATITLE$))
	NEW$ = $LEFTSTRING(ATITLE$, SLEN(ATITLE$) -4)						# remove ".seq" extension
	NEW$ = NEW$ + ".zmx"
ELSE
	PRINT "No file name entered. Exiting macro..."
	GOTO END_OF_MACRO
ENDIF

TIMER

! Reload the initial Zemax OS file:
SAVELENS
CLOSEWINDOW 0
LOADLENS "LENS.ZMX" 
DELETEFILE NEW$
SAVELENS NEW$

!--------------------------------------------------------------------------
! List of Supported Code V Commands in an alphabetic order:
!--------------------------------------------------------------------------
!	"A"		#	4th order coefficient
!	"ADE"	# 	Tilt about X axis
!	"ADC"	#	X axis tilt coupling group control	
!	"ADX"	#	Aperture decenter in X direction
!	"ADY"	#	Aperture decenter in Y direction
!	"ASP"	#	Even Asphere surface type 
!	"B"		#	6th order coefficient
!	"BDE"	#	Tilt about Y axis
!	"BDC"	#	Y axis tilt coupling group control	
!	"BEN"	#	Decenter and bend for mirrors
!	"C"	    #	8th order coefficients
!	"CAU"	#	Cauchy dispersion formula for private glass catalog
!	"CCY"	#	Surface curvature control (rotational symmetry)
!	"CDE"	#	Tilt about Z axis
!	"CDC"	#	Z axis tilt coupling group control	
!	"CIR"	#	Setting up aperture semi-diameter
!	"CON"	#	Conic surface type (typ. followed by "K" operand)
!	"CYL"	#	Toroidal Surface type
!	"CUM"	# 	Mirror substrate backside curvature indicator
!	"D"		#	10th order coefficient
!	"DAR"	#	Decenter and return operand
!	"DER"	#	No equivalent control in OpticStudio
!	"DDM"	#	Default dimensions I-inch, C-cm, M-mm
!	"DIF"	#	First part of 2-word operands (example DIF GRT)
!	"DIM"	#	Override Dimensions
!	"E"		#	12th order coefficient
!	"END"	#	End of description of the Custom Glass Catalog
!	"EPD"	#	Entrance pupil diameter
!	"F"		#	14th order coefficient
!	"FNO"	#	F/no of image space cone
!	"G"		#	16th order coefficient
!	"GLA"	#	Glass insertion (Surface-Specific format)
!	"GLB"	#	Global coordinate data reference
!	"GL1"	#	Glass description, surface 1
!	"GL2"	#	Glass description, surface 2
!	"GML"	#	Glass Manufacturers Laurent dispersion formula for private glass catalog
!	"GMS"	#	Glass Manufacturers Sellmeier dispersion formula for private glass catalog
!	"GO"	#	Update system
!	"GRO"	#	Grating order
!	"GRS"	#	Grating spacing
!	"GRT"	#	Grating surface type
!	"H"		#   18th order coefficient
!	"HAR"	#	Hartmann dispersion formula for private glass catalog	
!	"HCC"	#	Aspheric phase variable control
!	"HCO"	#	Aspheric phase
!	"HOE"	#	Holographic Surface
!	"HV1"	#	Type of the 1st construction point 
!	"HV2"	#	Type of the 2nd construction point 
!	"HOR"	# 	Holographic Diffraction Order
!	"HWL"	#	Construction Wavelength (Hologram)
!	"HX1"	#	X-coordinate of the 1st construction point
!	"HX2"	#	X-coordinate of the 2nd construction point
!	"HY1"	#	Y-coordinate of the 1st construction point
!	"HY2"	#	Y-coordinate of the 2nd construction point
!	"HZ1"	#	Z-coordinate of the 1st construction point
!	"HZ2"	#	Z-coordinate of the 2nd construction point
!	"INI"	#	Designer initials
!	"INS"	#	Surface Insertion (Surface-Specific format)
!	"K"		#	Value of the conic constant (typ. after operand "CON")
!	"LAU"	#	Laurent dispersion formula for private glass catalog
!	"LEN"	#	Data initialization for a new lens
!	"NAO"	#	Numerical aperture in object space
!	"PIM"	#	Solve for paraxial image distance
!	"PRV"	#	Start of a Private Glass Catalog
!	"PWL"	#	Wavelengths in Private Glass Catalog
!	"RET"	#	Solve to coordinate return: Orientation, XYZ 
!	"REX"	#	Half-width of the rectangular aperture, X direction
!	"REY"	#	Half-width of the rectangular aperture, Y direction
!	"RDM"	#	Radius mode instead of curvature
!	"RDY"	#	Radius insertion (Surface-Specific format)
!	"REF"	#	Reference wavelength number
!	"RMD"	#	Reflective/ Refractive mode command
!	"S"		#	Increment surface pointer
!	"SCO"	#	Special Surface Operand
!	"SI"	#	Image plane surface definition	
!	"SK"	# 	Surface K
!	"SLB"	#	Attach a label to a surface (in ZOS: Comment Column)		
!	"SLM"	#	Sellmeier dispersion formula for private glass catalog
!	"SO"	#	Object plane surface definition	
!	"SPH"	#	Spherical surface type
!	"SPS"	#	Set surface type to special shapes
!	"STO"	#	Designate surf as a stop surface
!	"THC"	#	Thickness coupling group control
!	"THG"	#	Thermal gradient surface
!	"THI"	#	Thickness insertion (Surface-Specific format)
!	"THM"	#	Thickness of the mirror substrate 
!	"TIT"	#	Lens system title
!	"VLX"	#	Fract. Ent. Pupil radius clipped off -X
!	"VLY"	#	Fract. ent. Pupil radius clipped off -Y
!	"VUX"	#	Fract. Ent. Pupil radius clipped off +X
!	"VUY"	#	Fract. Ent. Pupil radius clipped off +Y
!	"WL"	#	Req. wavelength in nm
!	"WTF"	#	Field weight
!	"WTW"	#	Wavelength weight
!	"XAN"	#	X Angle (degree) in object space
!	"XDE"	#	X decenter 
!	"XDC"	#	X decenter coupling group control	
!	"XIM"	#	Field: X Paraxial Image Height
!	"XOB"	#	Field: X Object Height
!	"XRI"	#	Field: X Real Image Height
!	"XTO"	#	X Toroid Surface
!	"YAN"	#	Y Angle (degrees) in object space
!	"YDE"	#	Y decenter of axis
!	"YDC"	#	Y decenter coupling group control
!	"YIM"	#	Field: Y Paraxial Image Height
!	"YOB"	#	Field: Y Object Height
!	"YRI"	#	Field: Y Real Image Height
!	"YTO"	#	Y toroid surface
!	"ZDE"	#	Z decenter of axis
!	"ZDC"	#	Z decenter coupling group control
!	"ZOO"	#	Zoom position
!	"!"		#	Commented out line

!--------------------------------------------------------------------------
! Operands for the Multi-Configuration Editor (MCE)
!--------------------------------------------------------------------------
!	"ADE"	# 	Tilt about X axis
!	"BDE"	#	Tilt about Y axis
!	"CDE"	#	Tilt about Z axis
!	"CIR"	#   Circular Aperture 	
!	"CUY"	#   Curvature of Surface	
!	"EPD"	#   Entrance pupil diameter	
!	"FNO"	#	F/# System Aperture value
!	"GLA"	#   Glass	
!	"GL1"	#	Glass description, surface 1
!	"GL2"	#	Glass description, surface 2
!	"REF"	#	Primary wavelength number
!	"RMD"	#	Reflective/ Refractive mode command
!	"THI"	#   Thickness	
!	"TIT"	#	Title (per configuration)
!	"WL"	#	Wavelength
!	"WTF"	#	Field Weight
!	"WTW"	#	Wavelength Weight
!	"XDE"	#   X-decenter	
!	"XAN"	#	X Angle (degree) in object space
!	"XIM"	#	Field: X Paraxial Image Height
!	"XOB"	#	Field: X Object Height
!	"XRI"	#	Field: X Real Image Height
!	"YDE"	#   Y-decenter	
!	"YAN"	#	Y Angle (degree) in object space
!	"YIM"	#	Field: Y Paraxial Image Height
!	"YOB"	#	Field: Y Object Height
!	"YRI"	#	Field: Y Real Image Height
!	"VUX"	#	Fract. Ent. Pupil radius clipped off +X
!	"VUY"	#	Fract. Ent. Pupil radius clipped off +Y
!	"ZDE"	#	Z-Shift

! Starting with parsing any custom glass catalog data
PRV_CATALOG_PARSE = 0													# Flag for the glass catalog parsing
PRV_ENTRIES = 0
PRV_STACK$ = ""
ZTG_ENTRIES = 0
ZTG_INDEX = 0
ZTG_STACK$ = ""
ZTG_PATH$ = ""
PRV_ON = 0					        # Flag to signal that there is a PRV section in .seq file
PRV_MIN_LIMIT = 0.3
PRV_MAX_LIMIT = 15.0

# Global variables that should not be reset when parsing the SEQ file
NumberSurf = 0				        # Total Number of "new" Surfaces
PRV_GLASSES_USED_IN_FILE$ = ""      # Only parse PRV/ZTG glasses actually used in file
PRV_NUMBER_OF_GLASS_IN_FILE = 0     # Maximum number of glasses found in the SEQ surface section
PRV_GLASSES_ACTUALLY_USED = 0       # boolean to determine if PRV glass is actually used in the surface section
NumberConfigs = 0			        # Number of configurations (for the ZOO command)
PRV_RESTRICTED_CHARACTERS$ = "*"    # remove garbage characters from PRV file name
HCT_R = 0							# toggle for rotationally symmetric HCO holograms
HCT_wavelength = 0                  # HCO construction wavelength for converting coefficient to ZOS

LABEL STARTING_POINT
IF (PRV_ENTRIES > 0) 
	PRINT "Glass Catalog ", PRV_PATH$, " is created."
	FOR j, 1, PRV_ENTRIES, 1
		StrXTD$ = $GETSTRING(PRV_STACK$, j)
		PRINT "Glass ", StrXTD$, 
		PRINT " is added to ", PRV_GLASS_CATALOG$, " Catalog."
	NEXT j
	PRINT
	
	! Do this notification only once
	PRV_ENTRIES = 0
ENDIF



IF (SLEN(ZTG_STACK$) > 0)
	num_params = 0
	curLine$ = ZTG_STACK$
	GOSUB GetNumParamsInline
	num_params = num_params / 2
	IF (num_params > ZTG_ENTRIES)
		ZTG_INDEX = 2*(num_params - ZTG_ENTRIES) - 1
		StrXTD$ = $GETSTRING(ZTG_STACK$, ZTG_INDEX)
		PRINT "Table glass file ", StrXTD$, 
		PRINT ".ZTG is created."
        
        IF ZTG_ENTRIES == 0 THEN PRINT
	ENDIF
ENDIF


IF (ZTG_ENTRIES > 0)
	PRV_CATALOG_PARSE = ZTG_ENTRIES + 1
	ZTG_ENTRIES = ZTG_ENTRIES - 1
ENDIF

IF (PRV_CATALOG_PARSE > 1)
	ZTG_INDEX = 2*(num_params - ZTG_ENTRIES) - 1
	StrXTD$  = $GETSTRING(ZTG_STACK$, ZTG_INDEX)
	StrXTD$ = StrXTD$ + ".ZTG"
	ZTG_PATH$ = $GLASSPATH() + "\" + StrXTD$
ENDIF
    
A$ = ATITLE$															# Needed for the main conversion loop
	
!--------------------------------------------------------------------------
! CONSTANTS
!--------------------------------------------------------------------------
dtor = 0.01745329			# degree to radian conversion constant
rtod = 57.29577951			# radian to degree conversion constant
pi   = ACOS(0) * 2          # pi constant 
MAX_SLEN = 360				# Maximum string length allowed  
MAX_PARS = 50				# Maximum number of parameters to parse
MAX_FIELDS = 25 			# Maximum number of view fields
epsilon = 1e-99				# Very small number
BLANK$ = " "				# Blank character constant
SEMICOLON$ = ";"			# Semicolon character constant
DOT$ = "."					# Dot character constant
PARSE_CHAR$ = " "			# a character to parse 
PREV_CHAR$  = " "			# initialize the previous character buffer
AMPERSAND$ = "&"			# Ampersand character constant (line continuation)
LineContent$ = BLANK$		# string buffer for the pre-parsing (&-operand)
ZTG_NUM_STEPS = 100			# number of wavelength steps in a table glass file
PRV_PARSING_REQUIRED = 0    # determines if PRV is present (row number in SEQ stored if PRV present)

!--------------------------------------------------------------------------
! Global Non-Numeric Variables
!--------------------------------------------------------------------------
curLine$ = " "				# Current String
MCE_Operand$ = "MOFF"		# string for the multi-configuration operand name
remove1stChar$ = ""			# Input variable for RemoveFirstCharacter subroutine
CurSurfLine$ = ""			# Content of the Current Surface Line
StringToTrim$ = ""			# string buffer 
ZoomStr$ = ""				# string buffer for zoom commands
PRV_GLASS_CATALOG$ = "CODEV_CONVERTED"	# Name of the custom glass catalog
PRV_PATH$ = $GLASSPATH() + "\" + PRV_GLASS_CATALOG$
PRV_PATH$ = PRV_PATH$ + ".AGF"
PRV_PWL_STRING$ =""			# string buffer for the private glass catalog wavelengths
TEMP_STR$ = ""				# string buffer
line$ = "-------------------------"
!--------------------------------------------------------------------------
! Global Numeric Variables
!--------------------------------------------------------------------------
EXTENDED_FILE_LISTING = 1	# Mode switch between pre-parse and mapping
GoNextLine = 0				# Mode switch flag
RadiusModeOn = 1 			# Mode switch: using surface radius or curvature
# NumberSurf = 0				# Total Number of "new" Surfaces
SurfaceSpecific = 0         # Flag for a surf specific command (ex. RET S9)
ConfigSpecific = 0 			# Flag for a configuration specific command
SpecSurfNum = 0				# SurfaceSpecific Number variable
SpecConfigNum = 0			# Configuration Specific Number variable
CurConfigNum = 0			# Current Configuration Number
# NumberConfigs = 0			# Number of configurations (for the ZOO command)	
SurfStart = 0				# Start Surface for the SurfaceSpecific range
SurfStop = 0				# Stop Surface for the SurfaceSpecific range
CurSurfNum = 0				# Current Surfaces Number
CurSurfIsaMirror = 0		# Flag for a current surf to be a mirror
NumWaves = 0				# Number of Wavelengths
NumFields = 0				# Number of Fields
PLF = 0						# Previous line flag 
CurrentMConfRow = 0			# Current row number in the MCE
StopSurfNum = 0				# Stop Surface Number
CurSurfNumStore = 0 		# Storage of original surface number
MCE_SignValue = 1			# MCE sign conversion, -1 for ADE, BDE
XDEstore = 0.0				# Storage of the negated X decenter
YDEstore = 0.0				# Storage of the negated Y decenter 
ZDEstore = 0.0				# Storage of the negated Z decenter 
ADEstore = 0.0				# Storage of the negated tilt about X 
BDEstore = 0.0				# Storage of the negated tilt about Y  
CDEstore = 0.0				# Storage of the negated tilt about Z 
LeadDummySurfIn = 0			# =1 when a leading dummy surface is inserted
TrailDummySurfIn = 0		# =1 when a trailing dummy surface is inserted
TargetSurfPosition = 0		# Position to insert a dummy surface
NumCtrlInstance = 0			# Number of coupling-group command instances
CurCtrlInstance = 1			# Current coupling-group command instances
ExtraParameter = 0			# Flag for 4-parameter coupling-group commands
Parameter = 0				# Parameter number (LDE)
HV1_TYPE = 0 				# Holographic point source type
HV2_TYPE = 0 				# Holographic point source type
UPPER_XVIG_TYPE = 0			# Flow control flag for upper/lower vignetting in X 
UPPER_YVIG_TYPE = 0			# Flow control flag for upper/lower vignetting in Y 
WITHIN_PRV_SECTION = 0		# Flag to signal that parser is within a PRV section
PRV_WL_MIN = 1000			# Wavelength minumum used for the customer glass catalogs
PRV_WL_MAX = 0 				# Wavelength maximum used for the customer glass catalogs
zoom_params = 0				# Number of parameters in ZoomStr$ buffer
fieldHadToConcat = 0        # only for printing purposes; DOUBLE value parsed directly into ZOS
fieldNumToDisplay = 14      # number of digits to display in PRINT window (no affect on convert accuracy)

# field array to handle up to 25 field points
RELEASE fieldXYW
DECLARE fieldXYW, DOUBLE, 2, 3, MAX_FIELDS

!--------------------------------------------------------------------------
! Pre-parse
! Get a temporary file name:
ATEMP$ = $TEMPFILENAME()

! location of the temp file with the pre-parsing info
! this is useful for debugging
! PRINT "Pre-parser output file: ", ATEMP$

! Open the new file
OPEN ATEMP$
OUTPUT ATEMP$

! Parse and copy the file data into a temporary file:
	line_counter = 0
	PARSE_CHAR$ = SEMICOLON$
GOSUB SeqFileListing

IF (!PRV_CATALOG_PARSE)
	OUTPUT SCREEN
ELSE 
	IF (PRV_ON > 0) 
		IF (PRV_CATALOG_PARSE == 1)
			COPYFILE ATITLE$, PRV_PATH$
			OPEN PRV_PATH$
			OUTPUT PRV_PATH$
		ELSE
			COPYFILE ATITLE$, ZTG_PATH$
			OPEN ZTG_PATH$
			OUTPUT ZTG_PATH$
		ENDIF
	ENDIF
ENDIF

!--------------------------------------------------------------------------

EXTENDED_FILE_LISTING = 0
A$ = ATEMP$


IF (!PRV_CATALOG_PARSE)
	! Calculate general system parameters
	GOSUB GetSystemParameters
ENDIF

! Counting number of lines in the seq file, listing pre-parsed data:
line_counter = 0
    
IF ((PRV_PARSING_REQUIRED == 1) & (PRV_ON == 1)) 
    PRV_CATALOG_PARSE = 0
    PRV_ON = 0
    GOTO PRV_LABEL_AFTER_PARSING
ENDIF


GOSUB SeqFileListing

IF (PRV_CATALOG_PARSE)
	IF (PRV_ON == 0)
		PRV_CATALOG_PARSE = 0
		GOTO STARTING_POINT
	ENDIF
ELSE 
    
	FORMAT 5 INT
	PRINT "Number of Surfaces           : ", CurSurfNum + 2
    
	! Add all new surfaces:
	NumberSurf = CurSurfNum - NSUR() + 1

	IF (NumberSurf > 0)
			FOR i, 1, NumberSurf, 1
			INSERT i		# Zemax OS command to insert a new surf at position "i"
			NEXT i
	ELSE	
		PRINT "Error: (Main) - ", NumberSurf
		PRINT "There has to be at least three surfaces"
		GOTO 421
	ENDIF

	! Reset NumberSurf as the total number of surfaces (before CBs are included)
	NumberSurf = NSUR() + 1

	! Increase the VEC2 size in case the number of surfaces is larger than 1000:
	IF (NumberSurf > 1000) THEN SETVECSIZE NumberSurf

	! Add surface indexer (ZOS surface numbers)		
	FOR i, 0, NumberSurf - 1, 1
		VEC2(i) = i
	NEXT i

	! Declare arrays for coupling-group commands:
	IF (NumCtrlInstance > 0)
		DECLARE CtrlSurface, INT, 1, NumCtrlInstance +1
		DECLARE CtrlGroup, INT, 1, NumCtrlInstance +1
		DECLARE CtrlCommand, INT, 1, NumCtrlInstance +1	
		
		! populate arrays with zeros:
		FOR i, 1, NumCtrlInstance, 1
			CtrlSurface(i) = 0
			CtrlGroup(i) = 0
			CtrlCommand(i) = 0
		NEXT i
	ENDIF

	! Setup System properties
	
	IF (NumFields > 0)
		!Try to set up required number of fields
		SYSP 101, NumFields  											# Zemax OS System Property

		IF (NumFields > MAX_FIELDS)
			! Check if ZOS accommodated required number of fields		
			IF (NumFields > NFLD())
				PRINT "Warning: Check maximum fields, all fields may not be used."
				NumFields = NFLD()
				MAX_FIELDS = NFLD()
			ELSE
				! ZOS can accommodate! 
				MAX_FIELDS = NumFields
			ENDIF
		ENDIF
        
		! Create vignetting variable array:
		DECLARE VU_X, DOUBLE, 1, NumFields
		DECLARE VL_X, DOUBLE, 1, NumFields
		DECLARE VU_Y, DOUBLE, 1, NumFields
		DECLARE VL_Y, DOUBLE, 1, NumFields
		FOR  j, 1, NumFields , 1
			VU_X(j) = 0.0
			VL_X(j) = 0.0
			VU_Y(j) = 0.0
			VL_Y(j) = 0.0
		NEXT j
	ELSE
		NumFields = 1
	ENDIF
	
	PRINT "Number of Fields             : ", NumFields	
ENDIF

! Setup Surface properties
OPEN A$

IF (!PRV_CATALOG_PARSE)
	PRINT
	PRINT "Parsing seq file line by line..."
	PRINT

	prevPos = 0
	curPos = 0
	GOSUB GetLinePosWithComID
ENDIF

CLOSE

# need to parse PRV after rest of file in order prevent overflow with string buffer
IF (PRV_PARSING_REQUIRED == 1)
    PRV_CATALOG_PARSE = 1 
    PRV_PARSING_REQUIRED = 0
    GOTO STARTING_POINT
ENDIF

LABEL PRV_LABEL_AFTER_PARSING

IF (!PRV_CATALOG_PARSE)
	! Additional post-processing for Glass names
	GOSUB VerifyGlass

	! Post-processing for thickness parameter
	GOSUB CheckForThicknessTransfer
    
	! Reset Global Reference Surface to 1
	SYSP 21, 1	
    
    ! For circular aperture or circular obscuration:
	! Check if maximum radius exceeds semi-diameter
	! If yes - assign semi-diameter as the maximum radius
    UPDATE
	FOR i, 1, NSUR(), 1	
		IF ((APTP(i) == 1) | (APTP(i) == 2))
            IF (APMX(i) > SDIA(i))
				PRINT "WARNING: Maximum Aperture Radius exceeds Semi-Diameter"
				FORMAT 6 INT
				PRINT "Surf ", i,
				FORMAT 14.5
				PRINT "  Reset Max Radius : ", SDIA(i)
				SURP i, APP2, SDIA(i) 									# Set Max Radius to Semi-Diameter value
				PRINT 
			ENDIF
		ENDIF
	NEXT i

	! Release arrays for coupling-group commands:
	IF (NumCtrlInstance > 0)
		RELEASE CtrlSurface
		RELEASE CtrlGroup
		RELEASE CtrlCommand	
	ENDIF

	IF (NumFields > 0)
		RELEASE VU_X
		RELEASE VL_X
		RELEASE VU_Y
		RELEASE VL_Y
	ENDIF

	! Update current optical system
	UPDATE 

	! Surface mapping between Code V and ZOS
	FORMAT 3 INT
	IF (VEC2(NumberSurf - 1) != (NumberSurf - 1))
		PRINT "Surface Mapping"
		PRINT "  (Code V -> ZOS):"
		PRINT "******************"
		FOR i, 0, NumberSurf - 1 , 1
			PRINT "Surf ", i,
			PRINT "  -> ", VEC2(i)
		NEXT i
		PRINT
		
		! Add Code V Surface label to the Comment field:
		FOR i, 0, NumberSurf - 1 , 1
			IF (i != VEC2(i))
				dummy = SPRO(VEC2(i), 1)
				subR$ = $buffer()
				IF (i < 10) THEN FORMAT 1 INT
				IF ((i > 9) & (i < 100)) THEN FORMAT 2 INT
				IF (i > 100) THEN FORMAT 3 INT	
				subD$ = "CodeV S" + $STR(i)
				IF (SLEN(subR$) > 0)
					subD$ = subD$ + ", "
					subD$ = subD$ + subR$
				ENDIF
				SURP VEC2(i), COMM, subD$								# Zemax OS Surface Property
			ENDIF
		NEXT i
	ENDIF
    
    ! Always turn paraxial ray aiming ON:
	SYSP 70, 1															# Zemax OS System Property	
ENDIF

LABEL 421

IF ((PRV_CATALOG_PARSE > 0) & (PRV_ON >= 1))
	! Turn the glass catalog parse off and run main seq file converter
	!PRV_CATALOG_PARSE = PRV_CATALOG_PARSE - 1
	PRV_CATALOG_PARSE = 0
	
	OUTPUT SCREEN 
	GOTO STARTING_POINT
ENDIF

DELETEFILE  ATEMP$
SAVELENS NEW$

FORMAT 0.1
PRINT "Elapsed time ", ETIM(), " seconds."

LABEL END_OF_MACRO

END 

! End of the Macro

!---------------------------------------------------------------------------------------------
! List of Subroutines used in the Macro
!---------------------------------------------------------------------------------------------
!	SeqFileListing            	# Parsing Code V .seq file
!	GetNumSegmentsInline      	# Pre-parsing file into "one-command-per-line" format
!	GetLinePosWithComID       	# Parsing line position for each Code V command
!	GetNumParamsInline        	# Parsing number of parameters in a command string (num_params)
!	GetSystemParameters			# Calculate general system parameters
!	ParseOperands             	# ZOS mapping of Code V commands
	
!	CheckMCE_Operand          	# Mapping of the Multi-Configuration Editor commands
!	CleanupGlassName          	# Conversion of CodeV glass into ZOS-compatible format
!	CommandIsTiltOrDecenter   	# Setup of the On/Off flag for decenter or tilt commands
!	CommandIsZoomSpecifier		# Check if a parsed line is a zoom specifier
!	InsertLeadingDummySurface 	# Leading dummy surface insertion (COORDBRK)
!	InsertTrailingDummySurface 	# Trailing dummy surface insertion (COORDBRK)
		
!	PopulateMCE               	# Setting up operand values for all configurations in MCE
!	PopulateMCE_byString      	# Setting up string operand values for all configs in MCE
!	CheckForThicknessTransfer 	# Post-processing check for thickness transfer conditions
!	CheckForMCE_Operand			# Check if MCE contains operand stored in subSTR$
!	TrimString                	# Trimming of leading and trailing blanks
!	UpperStrXTD					# Replace StrXTD$ with its uppercase equivalent 
!	VerifyGlass               	# Post-processing of glass name for compatibility
!	CheckSurfRange				# Determine SurfStart and SurfStop for Surface-Specific commands
!	FormatInteger		   		# Calculates number of positions to format number to a string

!	WavelengthRange				# Determine wavelength range from the .seq prescription
!	AddGlassEntry				# Create a new glass catalog entry
!	CreateTableGlass			# Create table glass txt file
!	TableGlassOriginal			# Create ZOS table glass from wavelength and refractive indexes sets
!	TableGlassFromFormula		# Create table glass for dispersion formulas that have no ZOS equivalent
!   CleanupMCE_Vignet           # Calculates compression/decenter from upper/lower vignetting factors
!---------------------------------------------------------------------------------------------

SUB SeqFileListing
! Parsing Code V .seq file
! reset parameters for BEN/DAR pre-parsing
	PRE_BEN = 0
	POST_BEN = 0
	PRE_DAR = 0
	POST_DAR = 0
    
    # field data variables
    PRE_FIELD = 0
    fieldIndexIncrement = 1
    
	! The file listing:
	IF (!PRV_CATALOG_PARSE)
		IF (EXTENDED_FILE_LISTING == 0)
			PRINT
				PRINT "******************   FILE LISTING (",
			PRINT ATITLE$,
			PRINT ")   ******************"
			PRINT
		ENDIF
	ENDIF
	
	! Open the CodeV file
	OPEN A$

	FORMAT "%#04i" LIT
	! Create a loop to read and print the entire file 
    
	LABEL 3
	READSTRING B$														# read a string
	comHDR$ = $GETSTRING(B$, 1)			
    
    # places field commands into array rather than string (prevents string buffer overflow error)
    IF (PRE_FIELD > 0)
        CheckCHAR$ = $RIGHTSTRING(curString$, 1)
        IF (CheckCHAR$ $!= AMPERSAND$) 
            IF ((fieldType == 1) | (fieldType == 2))
                IF (fieldIndexIncrement - 1 > NumFields) THEN NumFields = fieldIndexIncrement - 1
            ENDIF
            
            PRE_FIELD = 0
            fieldIndexIncrement = 1
            PRINT 
        ENDIF
    ENDIF
		
	! If a line is empty, read the next line
	IF (PRV_CATALOG_PARSE == 0)
        IF (SLEN(comHDR$) == 0)
			IF (EOFF()) 
				GOTO 4
			ELSE
				GOTO 3
			ENDIF
        ELSE
            # turns on the PRV parsing; completed before surfaces are parsed
            IF (comHDR$ $== "PRV") THEN PRV_PARSING_REQUIRED = 1
		ENDIF
	ELSE
        
        IF (comHDR$ $== "PRV") THEN PRV_ON = 3
		IF (PRV_ON == 2) THEN PRV_ON = 1
		IF (comHDR$ $== "END") THEN PRV_ON = 2
		IF ((comHDR$ $== "WL") | (comHDR$ $== "PWL")) THEN GOTO OVER_PRV_PARSE_FILTER
        
        IF ((SLEN(comHDR$) == 0) | (PRV_ON <= 1))
			IF (EOFF()) 
				GOTO 4
			ELSE
				GOTO 3
			ENDIF
		ENDIF
		LABEL OVER_PRV_PARSE_FILTER
	ENDIF
	
	line_counter = line_counter + 1
	IF (EOFF()) THEN GOTO 4

	IF (EXTENDED_FILE_LISTING == 1)
		curString$ = B$
		StringToTrim$ = curString$
		GOSUB TrimString
		curString$ = StringToTrim$
        
        
        StrXTD$ = comHDR$
        GOSUB UpperStrXTD
        
        z$ = StrXTD$
        
        IF ((StrXTD$ $== "XAN") | (StrXTD$ $== "XRI") | (StrXTD$ $== "XOB") | (StrXTD$ $== "XIM") | (StrXTD$ $== "YAN") | (StrXTD$ $== "YRI") | (StrXTD$ $== "YOB") | (StrXTD$ $== "YIM") | (StrXTD$ $== "WTF") |(PRE_FIELD == 1))
            IF StrXTD$ $== "XAN" THEN fieldType = 1
            IF StrXTD$ $== "XRI" THEN fieldType = 1
            IF StrXTD$ $== "XOB" THEN fieldType = 1
            IF StrXTD$ $== "XIM" THEN fieldType = 1
            
            IF StrXTD$ $== "YAN" THEN fieldType = 2
            IF StrXTD$ $== "YRI" THEN fieldType = 2
            IF StrXTD$ $== "YOB" THEN fieldType = 2
            IF StrXTD$ $== "YIM" THEN fieldType = 2
            
            IF StrXTD$ $== "WTF" THEN fieldType = 3
            
            IF ((SLEN(comHDR$) > 0) & (PRE_FIELD == 0)) THEN PRINT comHDR$, "   ", 
            curLine$ = curString$                       
            GOSUB GetNumParamsInline
            fieldStartForLoop = 1
            IF PRE_FIELD == 0 THEN  fieldStartForLoop = 2     # exclude comHDR$ in num_params
            
            
            FOR i, fieldStartForLoop, num_params, 1
                curLine$ = $GETSTRING(curString$, i)
                CheckCHAR$ = $RIGHTSTRING(curLine$, 1)
                
                IF (CheckCHAR$ $== AMPERSAND$)
                    subSegLength = SLEN(curLine$) - 1
                    curLine$ = $LEFTSTRING(curLine$, subSegLength)
                ENDIF
                
                IF SLEN(curLine$) > fieldNumToDisplay 
                    curLine$ = $LEFTSTRING(curLine$, fieldNumToDisplay)
                    fieldHadToConcat = 1
                ENDIF
                PRINT curLine$, " ", 
                
                fieldXYW(fieldType, fieldIndexIncrement) = SVAL(curLine$)
                fieldIndexIncrement = fieldIndexIncrement + 1
            NEXT
            PRE_FIELD = 1
           
        ELSE
            IF (LineContent$ $!= BLANK$) 
                ! Make sure the length of the strings is within limits
                subLen = SLEN(LineContent$)
                subLen = subLen + SLEN(curString$)
                IF (subLen < MAX_SLEN)
                    curString$ = LineContent$ + curString$
                    StringToTrim$ = curString$
                    GOSUB TrimString
                    curString$ = StringToTrim$
                ELSE
                    !String exceeded maximum allowed length
                    curString$ = LineContent$
                    StringToTrim$ = curString$
                    GOSUB TrimString
                    curString$ = StringToTrim$
                ENDIF
            ENDIF
        ENDIF
        
        
        # does not parse curString$ if comHDR$ is a field value
        IF PRE_FIELD == 1 THEN GOTO SKIP_SEGMENTATION
        
        
        
		CheckCHAR$ = $RIGHTSTRING(curString$, 1)
		IF (CheckCHAR$ $== AMPERSAND$) 									# line to be continued...
			subSegLength = SLEN(curString$) - 1							# remove trailing ampersand		
			curString$ = $LEFTSTRING(curString$, subSegLength)
			LineContent$ = curString$ + BLANK$							# add a blank at the string end 
			GOTO 3
		ELSE
			LineContent$ = BLANK$
		ENDIF
		
        
        
		! Do not parse a string if it is commented
		StrXTD$ = $LEFTSTRING(curString$,1)
		IF (StrXTD$ $== "!")
			StrXTD$ = $RIGHTSTRING(curString$,SLEN(curString$) -1)
			curString$ = "! " + StrXTD$
            PRINT curString$
			GOTO SKIP_SEGMENTATION
		ENDIF
        
        ! Parse string based on semicolon delimiter
        GOSUB GetNumSegmentsInline
		
		LABEL SKIP_SEGMENTATION
	ELSE
		IF (!PRV_CATALOG_PARSE)
			PRINT line_counter, "    ", B$
		ELSE
			curLine$ = B$
			IF (comHDR$ $== "WL") THEN GOSUB WavelengthRange
			IF (comHDR$ $== "PWL") THEN PRV_PWL_STRING$ = curLine$

			! Check if the command start with a quotation mark:
			StrXTD$ = $LEFTSTRING(comHDR$,1)			
            
			IF (PRV_CATALOG_PARSE == 1)
				IF (StrXTD$ $== "'")
					subD$ = $RIGHTSTRING(comHDR$, SLEN(comHDR$) - 1)
					StrXTD$ = $LEFTSTRING(subD$, SLEN(subD$) - 1)
                    
                    # check to make sure glass is actually used in SEQ file
                    GOSUB GlassUsedInFile
                    
                    IF (PRV_GLASSES_ACTUALLY_USED)  
                        # remove invalid characters from ZTG name
                        subStr$ = StrXTD$
                        GOSUB RemoveInvalidCharacters
                        StrXTD$ = str$
                        
                        GOSUB AddGlassEntry
                        IF (subINT <= 0)
                            ZTG_ENTRIES = ZTG_ENTRIES + 1
                            subINT = -1 * subINT
                            
                            FORMAT 2 INT
                            IF(ZTG_ENTRIES == 1)	
                                ZTG_STACK$ = StrXTD$ + " " + $STR(subINT)
                            ELSE
                                ZTG_STACK$ = ZTG_STACK$ + " " + StrXTD$
                                ZTG_STACK$ = ZTG_STACK$ + " " + $STR(subINT)
                            ENDIF
                        ELSE
                            PRV_ENTRIES = PRV_ENTRIES + 1
                            
                            IF(PRV_ENTRIES == 1)
                                PRV_STACK$ = StrXTD$
                            ELSE
                                PRV_STACK$ = PRV_STACK$ + " " + StrXTD$
                            ENDIF
                        ENDIF
                    ENDIF 
                    
                    
				ENDIF
			ELSE
				IF (StrXTD$ $== "'")
					subD$ = $RIGHTSTRING(comHDR$, SLEN(comHDR$) - 1)
					StrXTD$ = $LEFTSTRING(subD$, SLEN(subD$) - 1)
					subD$ = $GETSTRING(ZTG_STACK$, ZTG_INDEX)
                    
                    # remove invalid characters from ZTG name
                    subStr$ = StrXTD$
                    GOSUB RemoveInvalidCharacters
                    StrXTD$ = str$
                    
					IF (StrXTD$ $== subD$)
						StrXTD$  = $GETSTRING(ZTG_STACK$, ZTG_INDEX + 1)
						subINT = SVAL(StrXTD$ ) * (-1)
                        
						GOSUB CreateTableGlass
					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDIF
	
	GOTO 3
	LABEL 4
	CLOSE
	
	! back off the EOF line
	line_counter = line_counter - 1
	
	! The end of file listing:
	IF ((!PRV_CATALOG_PARSE) & (EXTENDED_FILE_LISTING ==0))
            IF fieldHadToConcat == 1
                PRINT 
                PRINT "Some field values were truncated to display on the screen."
                PRINT "The full number of decimal places (up to 14 digits) were converted into OpticStudio."
            ENDIF
            
            PRINT
			PRINT "******************    END OF LISTING (",
			PRINT ATITLE$,
			PRINT ")   ******************"
			PRINT	
	ENDIF
RETURN

SUB GlassUsedInFile
    PRV_GLASSES_ACTUALLY_USED = 0
    FOR j, 1, PRV_NUMBER_OF_GLASS_IN_FILE, 1
        subD$ = $GETSTRING(PRV_GLASSES_USED_IN_FILE$, j)
        IF (StrXTD$ $== subD$)
            PRV_GLASSES_ACTUALLY_USED = 1
            GOTO END_PRV_GLASS_ACTUALLY_USED
        ENDIF
    NEXT
    LABEL END_PRV_GLASS_ACTUALLY_USED
RETURN

SUB GetNumSegmentsInline
	! Pre-parsing file into "one-command-per-line" format
	! line is placed in a string variable curString$
	! returns global variable num_segments
	
	! reset variables:
	num_segments = 0
	subCounter = 0
	subNumTrim = 0 	# number to trailing blanks to trim
	subSegLength = 0 													# length of a segment
	subT$ = " "															# temporary string variable 
	subN$ = " "															# temporary string variable 
	subWasLastSegment = 0												# last segment flag
	char_counter = 0
	PREV_CHAR$ = BLANK$		
	
	LABEL 611
	char_counter = char_counter + 1
    
    ! prevent run-away loop if char_counter is greater than the number of characters in a string variable (7703)
    IF (char_counter > MAX_SLEN)
        OUTPUT SCREEN
        PRINT ""
        PRINT "Maximum length for a string variable is exceeded.  Please shorten string and try again"
        PRINT "   SEQ Command: ", curString$
        GOTO END_OF_MACRO
    ENDIF
    
	subR$ = $LEFTSTRING(curString$, char_counter)
	subD$ = $RIGHTSTRING(subR$, 1)
	IF (subD$ $== PARSE_CHAR$) 
		LABEL 613
		subNumTrim = 0
		num_segments = num_segments + 1
		subSegLength= char_counter - subCounter
		subT$ = $RIGHTSTRING(subR$, subSegLength)
		IF (subWasLastSegment == 0) THEN subT$ = $LEFTSTRING(subT$, subSegLength-1)
		
		! remove trailing blanks
		LABEL 612						
		IF (subSegLength > 0)
			subN$ = $RIGHTSTRING(subT$, 1)
			IF (subN$ $== BLANK$)
				subSegLength = subSegLength - 1
				subT$ = $LEFTSTRING(subT$,subSegLength)
				GOTO 612
			ENDIF
		ENDIF
		
		! remove front blanks
		LABEL 615						
		IF (subSegLength > 0)
			subN$ = $LEFTSTRING(subT$, 1)
			IF (subN$ $== BLANK$)		
				subSegLength = subSegLength - 1
				subT$ = $RIGHTSTRING(subT$,subSegLength)
				GOTO 615
			ENDIF
		ENDIF
		
		! Debugging:
		# PRINT "SEG", num_segments,
		# PRINT "   end position :", char_counter,
		# PRINT "   length   :", char_counter - subCounter,
		# PRINT "   content  : '", subT$,"'"
		# PRINT line_counter, "    ", subT$
		
		! BEN/DAR position pre-parse manipulation
		StrXTD$ = = $GETSTRING(subT$, 1)								# Get the operand value
		GOSUB CommandIsTiltOrDecenter									# Check if this is T/D
		GOSUB CommandIsZoomSpecifier									
		
		! Check if BEN found before Tilt/Decenter Commands
		! If true - move it after Tilt/Decenter
		IF ((StrXTD$ $== "BEN") & (AZDE_FLAG == 0)) 
			PRE_BEN = 1
			subT$ = "! " + subT$
		ENDIF
			
		IF ((PRE_BEN == 1) & (AZDE_FLAG == 1)) THEN POST_BEN = 1
	
		IF ((POST_BEN == 1) & (AZDE_FLAG == 0))
			PRE_BEN = 0
			POST_BEN = 0
			PRINT "BEN"		# inserting BEN command after T/D
		ENDIF
		
		! Check if DAR found before Tilt/Decenter Commands
		! If true - move it after Tilt/Decenter
		IF ((StrXTD$ $== "DAR") & (AZDE_FLAG == 0)) 
			PRE_DAR = 1
			subT$ = "! " + subT$
		ENDIF
			
		IF ((PRE_DAR == 1) & (AZDE_FLAG == 1)) THEN POST_DAR = 1
	
		IF ((POST_DAR == 1) & (AZDE_FLAG == 0))
			PRE_DAR = 0
			POST_DAR = 0
			PRINT "DAR"		# inserting DAR command after T/D
		ENDIF
		
		PRINT subT$
		IF (subWasLastSegment == 0) THEN line_counter = line_counter + 1
		
		subCounter = char_counter
		IF (subWasLastSegment) THEN GOTO 614		# okay, we done!
	ENDIF
    
	IF (subR$ $!= curString$) 
		GOTO 611
	ELSE
	    subNumBlankCharTrim = 0
		subWasLastSegment = 1
		GOTO 613	
	ENDIF
	LABEL 614
RETURN

SUB GetLinePosWithComID
	! Parsing line position for each Code V command
	CurSurfNum = 0 # counter for the surfaces
	LABEL 202
	IF (curPos > 0) 
		num_params = 0 
		GOSUB GetNumParamsInline
	ENDIF
	LABEL 203 
	READSTRING curLine$													# read the lines one at a time.
	IF (EOFF()) THEN GOTO 204 
	curPos = curPos + 1
	comHDR$ = $GETSTRING(curLine$, 1)									# Get the first string on the line
    
	! Surface Increment command: remove number if attached to the first operand
	subR$ = $LEFTSTRING(comHDR$,1)
	IF (subR$ $== "S") | (subR$ $== "s")
		! check if the remainder of the parameter is a number larger than zero
		subD$ = $RIGHTSTRING(comHDR$,SLEN(comHDR$)-1)
		IF (SVAL(subD$) > 0) THEN comHDR$ = "S"
	ENDIF
	
	! If a line is empty, read the next line
	IF (SLEN(comHDR$) == 0)
		IF (EOFF()) 
			GOTO 204
		ELSE
			GOTO 203
		ENDIF
	ENDIF
	
	FORMAT "%#04i" LIT
	PRINT curPos, "    ", curLine$
	GOSUB ParseOperands													# Parse CodeV command
	IF (GoNextLine == 1)
		FORMAT "%#04i" LIT
		PRINT curPos, "    Not parsed"
		PRINT
		GOTO 203
    ELSE 
		PRINT
	ENDIF
	GOTO 202
	LABEL 204
RETURN

SUB GetNumParamsInline
    ! Parsing number of parameters in a command string (num_params)
	PREV_CHAR$ = $GETSTRING(curLine$, 1)
	IF (SLEN(PREV_CHAR$) == 0) THEN RETURN	# exit if line is empty
	char_counter = 0
    temp = SLEN(curLine$)
	FOR char_counter, 2, SLEN(curLine$), 1
		PREV_CHAR$ = $GETSTRING(curLine$, char_counter)
		IF (PREV_CHAR$ $== "")
			num_params = char_counter - 1
			RETURN 
		ENDIF
	NEXT char_counter
RETURN

SUB GetSystemParameters
! Calculate general system parameters
CurSurfNum = 0 # surface number counter

! Open pre-parsed file
	OPEN A$

	FORMAT "%#04i" LIT
	! Create a loop to read and print the entire file
	LABEL 30
	READSTRING B$														# read a string
	comHDR$ = $GETSTRING(B$, 1)											# read the first operand
	
	! Surface Increment command: remove number if attached to the first operand
	subR$ = $LEFTSTRING(comHDR$,1)
	IF (subR$ $== "S") | (subR$ $== "s")
		! check if the remainder of the parameter is a number larger than zero
		subD$ = $RIGHTSTRING(comHDR$,SLEN(comHDR$)-1)
		IF (SVAL(subD$) > 0) THEN comHDR$ = "S"
	ENDIF

	! If a line is empty, read the next line
	IF (SLEN(comHDR$) == 0)
		IF (EOFF()) 
			GOTO 40
		ELSE
			GOTO 30
		ENDIF
	ENDIF
	line_counter = line_counter + 1
	IF (EOFF()) THEN GOTO 40

	! Skip a string if it is commented out
	StrXTD$ = $LEFTSTRING(B$,1)
	IF (StrXTD$ $== "!") THEN GOTO 50
	
	curLine$ = $GETSTRING(B$, 2)
	StrXTD$ = $LEFTSTRING(curLine$,1)
	
	! Check if a command is configuration-specific:
	IF ((StrXTD$ $== "Z") | (StrXTD$ $== "z")) 
		StrXTD$ = $RIGHTSTRING(curLine$, SLEN(curLine$) -1)
		subINT = SVAL(StrXTD$)
		! Find the largest configuration number
		IF (SpecConfigNum < subINT) THEN SpecConfigNum = subINT
	ENDIF
	
	! Check if a command is surface-specific:
	IF ((StrXTD$ $== "S") | (StrXTD$ $== "s")) 
		! pre-processing of Surface-Specific operands
		opName$ = comHDR$ + "_SPP"
		GOTO opName$
		GOTO 50
			
		LABEL INS_SPP
		GOSUB CheckSurfRange
		IF ((SurfStart >=1) & (SurfStop >= SurfStart))
			CurSurfNum = SurfStop - SurfStart + 1
		ENDIF
		
		GOTO 50
		
		LABEL ADC_SPP
		LABEL BDC_SPP
		LABEL CDC_SPP
		LABEL XDC_SPP
		LABEL YDC_SPP
		LABEL ZDC_SPP
		LABEL CCY_SPP
		LABEL THC_SPP			# increment the number of group instances
			GOSUB CheckSurfRange
			NumCtrlInstance = NumCtrlInstance +SurfStop -SurfStart +1
			
		GOTO 50
		
	ELSE
		! pre-processing of system operands
		opName$ = comHDR$ + "_PP"
		GOTO opName$
		GOTO 50

		LABEL S_PP
			CurSurfNum = CurSurfNum + 1
		GOTO 50
		
		LABEL ADC_PP
		LABEL BDC_PP
		LABEL CDC_PP
		LABEL XDC_PP
		LABEL YDC_PP
		LABEL ZDC_PP
		LABEL CCY_PP
		LABEL THC_PP			# increment the number of group instances
				NumCtrlInstance = NumCtrlInstance +1
		GOTO 50
		
		LABEL XAN_PP
			curLine$ = B$
			num_params = 0
			GOSUB GetNumParamsInline
			NumFields = num_params - 1
		GOTO 50

		LABEL YAN_PP
			curLine$ = B$
			num_params = 0
			GOSUB GetNumParamsInline
			if (NumFields + 1 < num_params) THEN NumFields = num_params - 1
		GOTO 50

		LABEL XIM_PP
			curLine$ = B$
			num_params = 0
			GOSUB GetNumParamsInline
			NumFields = num_params - 1
		GOTO 50

		LABEL YIM_PP
			curLine$ = B$
			num_params = 0
			GOSUB GetNumParamsInline
			if (NumFields + 1 < num_params) THEN NumFields = num_params - 1
		GOTO 50

		LABEL XOB_PP
			curLine$ = B$
			num_params = 0
			GOSUB GetNumParamsInline
			NumFields = num_params - 1
		GOTO 50

		LABEL YOB_PP
			curLine$ = B$
			num_params = 0
			GOSUB GetNumParamsInline
			if (NumFields + 1 < num_params) THEN NumFields = num_params - 1
		GOTO 50

		LABEL XRI_PP
			curLine$ = B$
			num_params = 0
			GOSUB GetNumParamsInline
			NumFields = num_params - 1
		GOTO 50

		LABEL YRI_PP
			curLine$ = B$
			num_params = 0
			GOSUB GetNumParamsInline
			if (NumFields + 1 < num_params) THEN NumFields = num_params - 1
		GOTO 50
	ENDIF
	
		LABEL 50
		GOTO 30
		LABEL 40

	! close pre-parsed file
	CLOSE	
RETURN

SUB ParseOperands
! ZOS mapping of Code V commands
	! Reset ConfigSpecific flag (Configuration Number is in the command)
	ConfigSpecific = 0
	CurConfigNum = 0
	
	! Reset SurfaceSpecific flag (Surface Number is in the command) 	# flag for surface-specific commands
	SurfaceSpecific = 0
	SpecSurfNum = CurSurfNum							 				# reset to the current surface 
	
	! Get the second operand in the command string:
	! subSTR$ holds the second operand in the command string
	subSTR$ = $GETSTRING(curLine$, 2)						

	! Check if the command is configuration-specific:
	StrXTD$ = $LEFTSTRING(subSTR$,1)
	IF ((StrXTD$ $== "Z") | (StrXTD$ $== "z")) 
		ConfigSpecific = 1 
		StrXTD$ = $RIGHTSTRING(subSTR$,SLEN(subSTR$) -1) 
		CurConfigNum = SVAL(StrXTD$)
	ENDIF
	
	! Check if the command is surface-specific:
	StrXTD$ = $LEFTSTRING(subSTR$,1)
	IF ((StrXTD$ $== "S") | (StrXTD$ $== "s")) 
		SurfaceSpecific = 1 
		StrXTD$ = $RIGHTSTRING(subSTR$,SLEN(subSTR$) -1) 
		SpecSurfNum = SVAL(StrXTD$)
	ENDIF
		
	! GOTO with variable string ID works similar to a SWITCH statement in C:
	GOTO comHDR$

	! Check if line is a glass description for a private glass catalog
	IF ((!PRV_CATALOG_PARSE) & (WITHIN_PRV_SECTION)) 
			PRINT "Glass catalog data"
			GOTO 401
	ENDIF
	
    LABEL NO_MAPPING
	PRINT "No mapping for command ", comHDR$							# in case there is no match
	GOTO 411

	LABEL NO_MAPPING_COMPLEX	# include second identifier
	PRINT "No mapping for command ", comHDR$, 
	PRINT " ",subSTR$
	GOTO 411
	
	LABEL A				# 4th order coefficient
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)
			subNUM = SVAL(subSTR$)
			FORMAT 16.8 EXP
			PRINT "4th order coefficient : ", subNUM
			! PLF = 0 for even asphere and 1 for toroidal surfaces
			IF (abso(subNUM) > epsilon)
				SURP CurSurfNum, PARM, subNUM, (2 + PLF)  				# Zemax OS Surface Property 	
			ENDIF
		ENDIF
	GOTO 401

	LABEL ADE			# Tilt about X axis	
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)
			subNUM = SVAL(subSTR$)
			IF (abso(subNUM) < epsilon) THEN GOTO 411  # nothing to do
			
			! Check if an extra surface needs to be inserted
			GOSUB InsertLeadingDummySurface
			
			subNUM = -1.0 * subNUM										# reverse tilt orientation
			FORMAT 14.5
			PRINT "Tilt about X axis   : ", subNUM
			SURP CurSurfNum-1, PARM, subNUM, 3   						# Zemax OS Surface Property 
			
			ADEstore = subNUM											# store tilt
		ENDIF
	GOTO 401
	
	LABEL ADX			" Aperture decenter in X direction"
	LABEL ADY			" Aperture decenter in Y direction"
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)
			! Convert Semi-Diameter into Circular Aperture
			IF ((APTP(i) != 1) & (APTP(i) != 2))
                subNum = SDIA(CurSurfNum)
                IF (subNum > 0) 
                    SURP CurSurfNum, APP2, subNum							# Zemax OS Surface Property 
                ENDIF
                ! Setting semi-diameter solve to "automatic"
                ! removed the SOLVETYPE; for APTP=1/2, OS ignores SDIA column completely    
                # SOLVETYPE CurSurfNum, SA                              # 7144
            ENDIF
            
			num_params = 0 
			GOSUB GetNumParamsInline			
			IF (num_params > 2)
				subSTR$ = $GETSTRING(curLine$, num_params)
			ENDIF 
			
			subNUM = SVAL(subSTR$)
            
			IF (abso(subNUM) > 0.0)
				LABEL STANDARD_ADXY
				FORMAT 6 INT
				PRINT "Surf ", CurSurfNum,
				FORMAT 14.5
				IF (comHDR$ $== "ADX")
					PRINT "  Aperture X Decenter : ", subNUM
					SURP CurSurfNum, APDX, subNUM 						# Zemax OS Surface Property 
				ELSE
					PRINT "  Aperture Y Decenter : ", subNUM
					SURP CurSurfNum, APDY, subNUM 						# Zemax OS Surface Property 
				ENDIF
			ELSE
				IF (subSTR$ $== "EDG") 									# aperture
					subSTR$ = $GETSTRING(curLine$, 3)					# Get the decenter value
					subNUM = SVAL(subSTR$)
					IF (abso(subNUM) < epsilon)
						PRINT "Unsupported format: third parameter is not a number."
						GOTO 401
					ELSE 
						GOTO STANDARD_ADXY
					ENDIF
				ELSE
					comHDR$ = comHDR$ + " "
					comHDR$ = comHDR$ + subSTR$
					GOTO NO_MAPPING
					GOTO 411
				ENDIF	
			ENDIF
			GOTO 401
		ENDIF
	GOTO 411

	LABEL ASP			# Even Asphere surface type 
		! NOTE: typically followed by "A,B,C,D,E,F,G,H" operands
		IF (!SurfaceSpecific)
			FORMAT 5 INT
			PRINT "Adding Even Asphere, surf  : ", CurSurfNum
			SURP CurSurfNum, TYPE, EVENASPH   							# Zemax OS Surface Property 
			PLF = 0
		ENDIF
	GOTO 401

	LABEL B				# 6th order coefficient
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)
			subNUM = SVAL(subSTR$)
			FORMAT 16.8 EXP
			PRINT "6th order coefficient : ", subNUM
			! PLF = 0 for even asphere and 1 for toroidal surfaces
			IF (abso(subNUM) > epsilon)
				SURP CurSurfNum, PARM, subNUM, (3 + PLF) 				# Zemax OS Surface Property 
			ENDIF
		ENDIF
	GOTO 401

	LABEL BDE			# Tilt about Y axis
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)
			subNUM = SVAL(subSTR$)
			IF (abso(subNUM) < epsilon) THEN GOTO 411  # nothing to do
			
			! Check if an extra surface needs to be inserted
			GOSUB InsertLeadingDummySurface
			
			subNUM = -1.0 * subNUM										# reverse tilt orientation
			FORMAT 14.5
			PRINT "Tilt about Y axis   : ", subNUM
			SURP CurSurfNum-1, PARM, subNUM, 4   						# Zemax OS Surface Property 
			
			BDEstore = subNUM											# store tilt
		ENDIF
	GOTO 401
	
	LABEL BEN			# Decenter and bend for mirrors 
		IF (subSTR$ $== "")												# no second operand
			! Avoid inserting a dummy surface behind the image plane
			IF (CurSurfNum == NSUR()) 
				PRINT "Dummy surface is not allowed beyond the image plane."
				GOTO 401
			ENDIF
		
			IF (abso(ADEstore) > epsilon) 
				GOSUB InsertTrailingDummySurface
				FORMAT 14.5
				PRINT "Tilt about X axis   : ", ADEstore
				SURP CurSurfNum+1, PARM, ADEstore, 3  					# Zemax OS Surface Property 
				SOLVETYPE CurSurfNum+1, PP_3, CurSurfNum-1, 1			# Zemax OS Surface Pickup
			ENDIF
			
			IF (abso(BDEstore) > epsilon)
				GOSUB InsertTrailingDummySurface
				FORMAT 14.5
				PRINT "Tilt about Y axis   : ", BDEstore
				SURP CurSurfNum+1, PARM, BDEstore, 4  					# Zemax OS Surface Property 
				SOLVETYPE CurSurfNum+1, PP_4, CurSurfNum-1, 1			# Zemax OS Surface Pickup  
			ENDIF
			
			IF (abso(CDEstore) > epsilon)
				FORMAT 14.5
				PRINT "Tilt about Z axis   : ", CDEstore
				SURP CurSurfNum+1, PARM, CDEstore, 5  					# Zemax OS Surface Property 
				SOLVETYPE CurSurfNum+1, PP_5, CurSurfNum-1, 1			# Zemax OS Surface Pickup 
			ELSE
				IF ((abso(ADEstore) > epsilon) | (abso(BDEstore) > epsilon))
				! Additional rotation about Z axis to keep meridional ray in plane
				subADE = dtor * ADEstore 
				subBDE = dtor * BDEstore
				subCDE = COSI(subADE) + COSI(subBDE)
				subCDE = subCDE / (1 + COSI(subADE)*COSI(subBDE))
				subCDE = -1.0* ACOS(subCDE) * rtod
				IF (abso(subCDE) > epsilon)
					FORMAT 14.5
					PRINT "Tilt about Z axis   : ", subCDE
					SURP CurSurfNum-1, PARM, subCDE, 5   				# Zemax OS Surface Property 
					SURP CurSurfNum+1, PARM, subCDE, 5   				# Zemax OS Surface Property 
					SOLVETYPE CurSurfNum+1, PP_5, CurSurfNum-1, 1		# Zemax OS pick-up setup
				ENDIF
			ENDIF
		ENDIF
	GOTO 401
	
	LABEL C				# 8th order coefficient
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)		
			subNUM = SVAL(subSTR$)
			FORMAT 16.8 EXP
			PRINT "8th order coefficient ", subNUM
			! PLF = 0 for even asphere and 1 for toroidal surfaces
			IF (abso(subNUM) > epsilon)
				SURP CurSurfNum, PARM, subNUM, (4 + PLF)  				# Zemax OS Surface Property 
			ENDIF
		ENDIF
	GOTO 401
	
	LABEL CDE			# Tilt about Z axis
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)	
			subNUM = SVAL(subSTR$)
			IF (abso(subNUM) < epsilon) THEN GOTO 411  # nothing to do

			! Check if an extra surface needs to be inserted
			GOSUB InsertLeadingDummySurface
			
			FORMAT 14.5
			PRINT "Tilt about Z axis   : ", subNUM
			SURP CurSurfNum-1, PARM, subNUM, 5   						# Zemax OS Surface Property 
			
			CDEstore = subNUM											# store tilt
		ENDIF
	GOTO 401

	LABEL CIR			# Setting up aperture semi-diameter
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)
			! Check if the surface is a mirror
			dummy = SPRO(CurSurfNum, 4)
			subR$ = $buffer()
			! Set default aperture type to circular
			SURP CurSurfNum, ATYP, 1 									# Zemax OS Surface Property 
			IF (subR$ $== "MIRROR")
				CurSurfIsaMirror = 1	
			ELSE 
				CurSurfIsaMirror = 0
			ENDIF
		
			subNUM = SVAL(subSTR$)
			IF (subNUM > 0.0)
				FORMAT 6 INT
				PRINT "Surf ", CurSurfNum,
				FORMAT 14.5
				PRINT "  Fixed Aperture   : ", subNUM
				SURP CurSurfNum, SDIA, subNUM 							# Zemax OS Surface Property 	
				SURP CurSurfNum, APP2, subNUM 		     				# Max Radius for CIR OBS
                IF (CurSurfIsaMirror)
					SURP CurSurfNum, APP2, subNUM 						# Max Radius for CIR OBS 
				ENDIF
				GOTO 401
			ELSE
				IF (subSTR$ $== "OBS") 									# aperture obscuration
					subSTR$ = $GETSTRING(curLine$, 3)					# Get the aperture value
					subNUM = SVAL(subSTR$)
					IF (abso(subNUM) < epsilon)
						PRINT "Unsupported format for aperture obscuration."
						GOTO 401										# third parameter is not a number
					ENDIF
					FORMAT 6 INT
					PRINT "Surf ", CurSurfNum,
					FORMAT 14.5
					PRINT "  CIR Obscuration  : ", subNUM			    # 
                    IF (SPRO(CurSurfNum, 20) == 1)                      # obscuration becomes min radius
                        SURP CurSurfNum, APP1, subNum
                    ELSE
                        SURP CurSurfNum, ATYP, 2
                        SURP CurSurfNum, APP2, subNUM
                    ENDIF
					# IF (!CurSurfIsaMirror)
						# SURP CurSurfNum, ATYP, 2 						# Zemax OS Surface Property 
						# SURP CurSurfNum, APP2, subNUM 					# Max Radius for CIR OBS 
					# ELSE
						# SURP CurSurfNum, ATYP, 1 						# Zemax OS Surface Property 
						# SURP CurSurfNum, APP1, subNUM 					# Min Radius for CIR OBS 
					# ENDIF
					GOTO 401
				ENDIF
				IF (subSTR$ $== "CLR")| (subSTR$ $== "EDG") 			# aperture
					subSTR$ = $GETSTRING(curLine$, 3)					# Get the aperture value
					subNUM = SVAL(subSTR$)
					IF (abso(subNUM) < epsilon)
						PRINT "Unsupported format: third parameter is not a number."
						GOTO 401										
					ENDIF
					FORMAT 6 INT
					PRINT "Surf ", CurSurfNum,
					FORMAT 14.5
					PRINT "  Fixed Aperture   : ", subNUM
					SURP CurSurfNum, SDIA, subNUM 						# Zemax OS Surface Property 	
					IF (CurSurfIsaMirror)
						SURP CurSurfNum, ATYP, 1 						# Zemax OS Surface Property 
						SURP CurSurfNum, APP2, subNUM 					# Max Radius
					ENDIF
					GOTO 401
				ELSE
					comHDR$ = comHDR$ + " "
					comHDR$ = comHDR$ + subSTR$
					GOTO NO_MAPPING
					GOTO 411
				ENDIF
			ENDIF
		ENDIF
	GOTO 401
	
	LABEL CON			# Conic surface type (typ. followed by "K" operand)
		IF (!SurfaceSpecific)	
			FORMAT 5 INT
			PRINT "Adding Conic Constant, surf : ", CurSurfNum
		ENDIF
	GOTO 401
	
	LABEL CYL			# Toroidal surface type
	! NOTE: typically followed by "A,B,C,D,E,F,G,H" operands 
		IF (!SurfaceSpecific)	
			FORMAT 5 INT
			PRINT "Adding Toroidal Surface, surf : ", CurSurfNum
			SURP CurSurfNum, TYPE, TOROIDAL   							# Zemax OS Surface Property 
			PLF = 1
		ENDIF
	GOTO 401
	
	LABEL CUM		# mirror substrate shape
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)	
			subNUM = SVAL(subSTR$)
			FORMAT 3 INT
			IF (abso(subNUM) < epsilon)
				PRINT "Setting flat mirror substrate at surface ", CurSurfNum
				SURP CurSurfNum, 97, 1									# Zemax OS Surface Property 
			ELSE
				PRINT "Setting curved mirror substrate at surface ", CurSurfNum
				SURP CurSurfNum, 97, 2									# Zemax OS Surface Property 
			ENDIF
		ENDIF
	GOTO 401
	
	LABEL D				# 10th order coefficient
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)	
			subNUM = SVAL(subSTR$)
			FORMAT 16.8 EXP
			PRINT "10th order coefficient : ", subNUM
			! PLF = 0 for even asphere and 1 for toroidal surfaces
			IF (abso(subNUM) > epsilon)
				SURP CurSurfNum, PARM, subNUM, (5 + PLF)  				# Zemax OS Surface Property 
			ENDIF
		ENDIF
	GOTO 401

	LABEL DAR			# Decenter and return operand
		IF (subSTR$ $== "")												# no second operand
			! Avoid inserting a dummy surface behind the image plane
			IF (CurSurfNum == NSUR()) 
				PRINT "Dummy surface is not allowed beyond the image plane."
				GOTO 401
			ENDIF
			
			IF (abso(ADEstore) > epsilon)
				GOSUB InsertTrailingDummySurface
				FORMAT 14.5
				PRINT "Tilt about X axis   : ", -1.0*ADEstore
				SURP CurSurfNum+1, PARM, -1.0*ADEstore, 3  				# Zemax OS Surface Property 
				SOLVETYPE CurSurfNum+1, PP_3, CurSurfNum-1, -1			# Zemax OS Surface pickup
			ENDIF
			
			IF (abso(BDEstore) > epsilon)
				GOSUB InsertTrailingDummySurface
				FORMAT 14.5
				PRINT "Tilt about Y axis   : ", -1.0*BDEstore
				SURP CurSurfNum+1, PARM, -1.0*BDEstore, 4  				# Zemax OS Surface Property 
				SOLVETYPE CurSurfNum+1, PP_4, CurSurfNum-1, -1			# Zemax OS Surface pickup
			ENDIF
			
			IF (abso(CDEstore) > epsilon)
				GOSUB InsertTrailingDummySurface
				FORMAT 14.5
				PRINT "Tilt about Z axis   : ", -1.0*CDEstore
				SURP CurSurfNum+1, PARM, -1.0*CDEstore, 5  				# Zemax OS Surface Property 
				SOLVETYPE CurSurfNum+1, PP_5, CurSurfNum-1, -1			# Zemax OS Surface pickup
			ENDIF
			
			IF (abso(XDEstore) > epsilon)
				GOSUB InsertTrailingDummySurface
				FORMAT 14.5
				PRINT "X decenter return   : ", -1.0*XDEstore
				SURP CurSurfNum+1, PARM, -1.0*XDEstore, 1  				# Zemax OS Surface Property 
				SOLVETYPE CurSurfNum+1, PP_1, CurSurfNum-1, -1			# Zemax OS Surface pickup
			ENDIF
			
			IF (abso(YDEstore) > epsilon)
				GOSUB InsertTrailingDummySurface
				FORMAT 14.5
				PRINT "Y decenter return   : ", -1.0*YDEstore
				SURP CurSurfNum+1, PARM, -1.0*YDEstore, 2  				# Zemax OS Surface Property 
				SOLVETYPE CurSurfNum+1, PP_2, CurSurfNum-1, -1			# Zemax OS Surface pickup
			ENDIF

			IF (abso(ZDEstore) > epsilon)
				GOSUB InsertTrailingDummySurface
				FORMAT 14.5
				PRINT "Z position return   : ", -1.0*ZDEstore
				SURP CurSurfNum+1, THIC, -1.0*ZDEstore	  				# Zemax OS Surface Property 
				SOLVETYPE CurSurfNum+1, TP, CurSurfNum-1, -1			# Zemax OS Surface pickup
				SURP CurSurfNum+1 , COMM, "Return Z-shift"	
			
				! Change surface type to STANDARD if all other T/D are zero
				subNUM = XDEstore + YDEstore 
				subNUM = subNUM + ADEstore + BDEstore + CDEstore
				IF (abso(subNUM) < epsilon)
				SURP CurSurfNum +1, TYPE, STANDARD   					# Zemax OS Surface Property
				ENDIF
			ENDIF
			
			FORMAT 3 INT
			PRINT "Order of precedence", 1
			SURP CurSurfNum+1, PARM, 1, 6  								# Zemax OS Surface Property 
		ENDIF
	GOTO 401
	
	LABEL DER
		PRINT "Not converted. No equivalent control in OpticStudio."
	GOTO 401
	
	LABEL DDM			# Default dimensions I-inch, C-cm, M-mm
		LABEL 403
		subSTR$ = $GETSTRING(curLine$, 2)								# Get the aperture value
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF ((subSTR$ $== "M") | (subSTR$ $== "m")) 
			subNUM = 0
			PRINT "Lens units: mm"
		ENDIF
		IF ((subSTR$ $== "MM") | (subSTR$ $== "mm"))
			subNUM = 0
			PRINT "Lens units: mm"
		ENDIF
		IF  ((subSTR$ $== "I") | (subSTR$ $== "i"))
			subNUM = 2
			PRINT "Lens units: inch"
		ENDIF
		IF ((subSTR$ $== "C") |  (subSTR$ $== "c"))
			subNUM = 1
			PRINT "Lens units: cm"
		ENDIF
		SYSP 30, subNUM 												# Zemax OS System Property 
	GOTO 401

	LABEL DIF			# 2-word operands (example DIF GRT))
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		DIF_DOE = 0
		IF (SurfaceSpecific) 
			CurSurfNum =  VEC2(SpecSurfNum)
			subSTR$ = $GETSTRING(curLine$, 3)	
		ENDIF
		IF (subSTR$ $== "GRT") THEN GOTO 410
		IF (subSTR$ $== "HOE") THEN GOTO HOE
		IF (subSTR$ $== "DOE") THEN DIF_DOE = 1
	GOTO 401												
	
	LABEL DIM			# Override dimensions I-inch, C-cm, M-mm
	PRINT "Override Dimensions"
	GOTO 403															# same as DDM

	LABEL E				# 12th order coefficient
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)	
			subNUM = SVAL(subSTR$)
			FORMAT 16.8 EXP
			PRINT "12th order coefficient : ", subNUM
			! PLF = 0 for even asphere and 1 for toroidal surfaces
			IF (abso(subNUM) > epsilon)
				SURP CurSurfNum, PARM, subNUM, (6 + PLF)  				# Zemax OS Surface Property 
			ENDIF
		ENDIF
	GOTO 401
	
	LABEL END			# End of a custom glass catalog description
			PRINT "End Private glass catalog section"
			WITHIN_PRV_SECTION = 0
	GOTO 401
	
	LABEL EPD			# Entrance Pupil Diameter
		! 0 for EPD
		! 1 for F/#
		! 2 for NA
		! 3 for "float by stop size"
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)	
			! Set up the type of system aperture to "EPD"
			SYSP 10, 0													# Zemax OS System Property
			subNUM = SVAL(subSTR$)
			FORMAT 14.5
			PRINT "System Aperture  :      EPD"
			PRINT "Aperture value   : ", subNUM
			SYSP 11, subNUM 											# Zemax OS System Property 
		ENDIF
	GOTO 401
	
	LABEL F				# 14th order coefficient
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)	
			subNUM = SVAL(subSTR$)
			FORMAT 16.8 EXP
			PRINT "14th order coefficient : ", subNUM
			! PLF = 0 for even asphere and 1 for toroidal surfaces
			IF (abso(subNUM) > epsilon)
				SURP CurSurfNum, PARM, subNUM, (7 + PLF)  				# Zemax OS Surface Property 
			ENDIF
		ENDIF
	GOTO 401
	
	LABEL FNO			#F/# of image space cone
		! 0 for EPD
		! 1 for F/#
		! 2 for NA
		! 3 for "float by stop size"
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)	
			! Set up the type of system aperture to "EPD"
			SYSP 10, 1													# Zemax OS System Property 
			! Set-up the aperture value
			subNUM = SVAL(subSTR$)
			FORMAT 14.5
			PRINT "System Aperture  :      F/#"
			PRINT "Aperture value   : ", subNUM
			SYSP 11, subNUM 											# Zemax OS System Property 
		ENDIF
	GOTO 401
	
	LABEL G				# 16th order coefficient
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)
			subNUM = SVAL(subSTR$)
			FORMAT 16.8 EXP
			PRINT "16th order coefficient : ", subNUM
			! PLF = 0 for even asphere and 1 for toroidal surfaces
			IF (abso(subNUM) > epsilon)
				IF (PLF == 0) 
					SURP CurSurfNum, PARM, subNUM, 8  					# Zemax OS Surface Property
				ELSE
				ENDIF
			ENDIF
		ENDIF
	GOTO 401
	
	LABEL GLA			# Glass insertion (Surface-Specific format)
	LABEL GL1			# same as GLA in sequential mode
	LABEL GL2			# same as GLA in sequential mode
	LABEL RMD			# Reflective / Refractive mode command 
		IF subSTR$ $== "" THEN GOTO 401
		IF (SurfaceSpecific)			
			subSTR$ = $GETSTRING(curLine$, 3)
			IF subSTR$ $!= ""
				PRINT "Glass Insertion",
				FORMAT 7 INT
				IF (VEC2(SpecSurfNum) == SpecSurfNum)
					PRINT " at surf : ", SpecSurfNum
				ELSE
					PRINT " at surf : ", VEC2(SpecSurfNum),
					IF (VEC2(SpecSurfNum) < 10) THEN FORMAT 1 INT
					IF ((VEC2(SpecSurfNum) > 9) & (VEC2(SpecSurfNum) < 100)) THEN FORMAT 2 INT
					IF (VEC2(SpecSurfNum) > 100) THEN FORMAT 3 INT				
					PRINT "   (CodeV S", SpecSurfNum,
					PRINT ")"
				ENDIF
				
				IF (subSTR$ $== "REFL")
					CurSurfIsaMirror = 1
					PRINT "Surface is a Mirror"
					SURP VEC2(SpecSurfNum), GLAS, MIRROR				# Zemax OS Surface Property
					GOTO GLA_407
				ENDIF
				IF (subSTR$ $!= "AIR") 
					GOSUB CleanupGlassName
					IF (subD$ $!= "Model")
						PRINT "Glass            :      ", subSTR$
					ELSE
						FORMAT 5.2
						PRINT "Glass            :      ", SVAL(subR$),
						FORMAT 4.1
						PRINT ",",subNUM
					ENDIF
					SURP VEC2(SpecSurfNum), GLAS, subSTR$				# Zemax OS Surface Property
				ELSE
					PRINT "Glass            :            N/A"
				ENDIF
					
			ENDIF
		ENDIF
		
		LABEL GLA_407
	GOTO 401
	
	LABEL GLB 			# Global coordinate data reference
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		
		
		IF (SurfaceSpecific)
			subXTR$ = $GETSTRING(curLine$, 3)
			subINT = SpecSurfNum
		ELSE
			subXTR$ = $GETSTRING(curLine$, 2)
			subINT = CurSurfNum
		ENDIF 
	
		IF (SLEN(subXTR$) > 1)
			subXTR$ = $RIGHTSTRING(subXTR$ , SLEN(subXTR$) -1)
			subVal= SVAL(subXTR$)
		ENDIF

		FORMAT 3 INT
		PRINT "Inserting dummy surface at position: ", subINT
		INSERT subINT													# Zemax OS inserts in front of a surface
		PRINT "Change surface type to COORDBRK..."
		SURP subINT, TYPE, COORDBRK										# Zemax OS Surface Property
		IF (!SurfaceSpecific)
			CurSurfNum = CurSurfNum + 1									# Increment the Current Surface
		ENDIF
		! Re-mapping surfaces due to surface insertion
		IF (CurSurfNumStore > VEC2(NumberSurf-1)) THEN PRINT "Error: InsertSecondTrailingCB_Surface)"
		FOR i, CurSurfNumStore, NumberSurf - 1, 1 
			VEC2(i) = VEC2(i) + 1
		NEXT i
		PRINT
		PRINT "Surface Re-Mapping"
		PRINT "  (Code V -> ZOS):"
		PRINT "******************"
			PRINT "Surf ", CurSurfNumStore,
			PRINT "  -> ", VEC2(CurSurfNumStore)
		PRINT
		
		FORMAT 3 INT
		PRINT "Surface: ", subINT
		PRINT "Coordinate return solve: Orientation, XYZ to surf: ", VEC2(subVal),
		IF (subVal < 10) THEN FORMAT 1 INT
		IF (subVal > 9) & (subVal < 100) THEN FORMAT 2 INT
		IF (subVal > 100) THEN FORMAT 3 INT				
		PRINT "   (CodeV S", subVal,
		PRINT ")"
		SURP subINT, 76, 3												# Zemax OS Surface Property
		SURP subINT, 77, VEC2(subVal)						        	# Zemax OS Surface Property

	GOTO 401	
	
	
	LABEL GO			# Update system
		PRINT "Update System"											# Zemax OS System Command
        GOSUB CleanupMCE_Vignet                                         # runs a check on ZOO vignetting
	GOTO 401

	LABEL GRO			# Grating Order
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)
			subNUM = SVAL(subSTR$)
			FORMAT 16.8 EXP
			PRINT "Grating Order          : ", subNUM
			SURP CurSurfNum, PARM, subNUM, 2 							# Zemax OS Surface Property
		ENDIF
	GOTO 401

	LABEL GRS			# Grating Spacing 
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)
            # GRS is in lens unit spacing; Par1 is in lp/um
            scaling_factor = 1
            IF SYPR(30) == 0 THEN scaling_factor = 0.001
            IF SYPR(30) == 1 THEN scaling_factor = 0.0001
            IF SYPR(30) == 2 THEN scaling_factor = 3.9370078740157E-5
        
			subNUM = scaling_factor / SVAL(subSTR$)
			FORMAT 16.8 EXP
			PRINT "Grating Spacing        : ", subNUM
			SURP CurSurfNum, PARM, subNUM, 1 							# Zemax OS Surface Property
		ENDIF
	GOTO 401
	
	LABEL GRT			# Diffraction Grating 
		LABEL 410
		PRINT "Diffraction Grating"
		SURP CurSurfNum, TYPE, DGRATING									# Zemax OS Surface Property 
	GOTO 401

	LABEL H				# 18th order coefficient 
		IF (PLF == 0)
			PRINT "18th order is not supported for Even Asphere Surface."
		ELSE
			PRINT "18th order is not supported for Toroidal Surface."
		ENDIF
	GOTO 401
	
	LABEL HCT
		StrXTD$ = subSTR$
		GOSUB UpperStrXTD
		IF StrXTD$ $== "R"
			HCT_R = 1
			PRINT "Rotationally symmetric even hologram"
		ENDIF
		IF StrXTD$ $== "XY" THEN PRINT "OpticStudio does not support general X and Y polynomial"
		IF StrXTD$ $== "AXY" THEN PRINT "OpticStudio does not support Absolute X and Y polynomial"
		IF StrXTD$ $== "XAY" THEN PRINT "OpticStudio does not support X and Absolute Y polynomial"
		IF StrXTD$ $== "XYA" THEN PRINT "OpticStudio does not support Absolute X and Absolute Y polynomial"
		IF StrXTD$ $== "AR" THEN PRINT "OpticStudio does not support odd powers of r"
		IF (StrXTD$ $== "USR") | (StrXTD$ $== "US2") THEN PRINT "OpticStudio does not support user-defined phase polynomial"
		
	GOTO 401
	
	LABEL HCO			# Aspheric Phase Surface
		IF subSTR$ $== "" THEN GOTO 401										# No operand read
		StrXTD$ = subSTR$
		subSTR$ = $LEFTSTRING(subSTR$, 1)									# must not have have surface specifier
		IF (!SurfaceSpecific)
			surf_type = SPRO(CurSurfNum, 17)								# determine surface type
			IF surf_type != 41												# sets up initial BINARY_2 surface
				IF surf_type == 1											# even asphere, need to map
					DECLARE EvenAsphereCoeffs, DOUBLE, 1, 8
					FOR i, 1, 8, 1
						EvenAsphereCoeffs(i) = PARM(i, CurSurfNum)
					NEXT
				ENDIF
				
				PRINT "Aspheric Phase Surface"
				SURP CurSurfNum, TYPE, BINARY_2								# Zemax OS Surface Property 
					
				! map aspheric coefficients
				IF surf_type == 1
					FOR i = 1, 8, 1
						SURP CurSurfNum, 10, EvenAsphereCoeffs(i), i
					NEXT

					Release EvenAsphereCoeffs
				ENDIF
				
				! set diffraction order
				SURP CurSurfNum, 10, DIF_ORD, 0
				
				
				! sets Norm Radius to 1 so coefficients map directly
				SURP CurSurfNum, 10, 1, 14
			ENDIF
			
			! parse subStr$ value for 'Maximum Term #'
			IF HCT_R == 1
				temp = SPRX(CurSurfNum, 10, 13)								# gets current aspheric term
				StrXTD$ = $RIGHTSTRING(StrXTD$, SLEN(StrXTD$) - 1)
				subVal = SVAL(StrXTD$)
				IF subVal > temp THEN temp = subVal
				SURP CurSurfNum, 10, temp, 13								# increase maximum terms
				temp = subVal												# store coefficient number
				
				# StrXTD$ = $GETSTRING(curLine$, 3)
				# subVal = 
				# print curLine$
				GOSUB GetNumParamsInline
				IF num_params >= 3
					subSTR$ = $GETSTRING(curLine$, 3)
					subVal = SVAL(subSTR$)
                    
                    factor = 1000                                           # default value
                    StrXTD$ = $UNITS()
                    IF (StrXTD$ $== "MM") THEN factor = 1000
                    IF (StrXTD$ $== "CM") THEN factor = 100
                    IF (StrXTD$ $== "I") THEN factor = 25400
                    IF (HCT_wavelength == 0) THEN HCT_wavelength = PWAV()
                    NR = SPRX(curSurfNum, 10, 14)
                    
                    subVal = POWR(NR, 2 * temp) * (2 * pi / (HCT_wavelength / factor)) * subVal
                    
					SURP CurSurfNum, 10, subVal, 14 + temp
				ENDIF
			ELSE
				PRINT "Holographic element is not rotationally symmetric"
				PRINT "no mapping available"
			ENDIF
		ELSE
			FORMAT "%#04i" LIT
			PRINT curPos, "    Nothing to convert"
		ENDIF
	GOTO 401
	
	LABEL HCC
		GOSUB GetNumParamsInline
		IF num_params >= 3
			subStr$ = $GETSTRING(curLine$, 2)
			subStr$ = $RIGHTSTRING(subStr$, SLEN(subStr$) - 1)
			subVal = SVAL(subStr$)
			subVal = subVal + 14
			
			subStr$ = $GETSTRING(curLine$, 3)
			FORMAT .0
			IF (subStr$ $== "0")
				subStr$ = "PV_" + $STR(subVal)
				subVal = subVal - 14
				PRINT "Holographic Coefficient ", $STR(subVal), " set to variable"
			ELSE
				subStr$ = "PF_" + $STR(subVal)
				subVal = subVal - 14
				PRINT "Holographic Coefficient ", $STR(subVal), " set to fixed"
			ENDIF
			
			SOLVETYPE CurSurfNum, subStr$
		ENDIF
	GOTO 401
	
	LABEL HOE			# Holographic Surface
		FORMAT 3 INT
		PRINT "Surface ", CurSurfNum, 
		PRINT " set to HOLOGRAM 1"
		PRINT "Warning: May need to change to HOLOGRAM 2."
		SURP CurSurfNum, TYPE, HOLOGRM1										# Zemax OS Surface Property 
		! Set point source types to zeros:
		HV1_TYPE = 0
		HV2_TYPE = 0
	GOTO 401
	
	LABEL HV1			#	Type of the 1st construction point 
	LABEL HV2			#	Type of the 2nd construction point 
		
		subR$ = $GETSTRING(curLine$, 1)
		subD$ = $RIGHTSTRING(subR$,1)
		subNUM = SVAL(subD$)
		
		IF (SurfaceSpecific)
			IF (CurSurfNum  !=  VEC2(SpecSurfNum))
				CurSurfNum =  VEC2(SpecSurfNum)
				HV1_TYPE = 0
				HV2_TYPE = 0 
			ENDIF
			subSTR$ = $GETSTRING(curLine$, 3)
		ENDIF
		IF subSTR$ $== "" THEN GOTO 401	
		
		subD$ = $LEFTSTRING(subSTR$,1)
		IF (subD$ $== "V") | (subD$ $== "v")
			IF subNUM < 2 
				PRINT "Type of the 1st construction point: Virtual "
				HV1_TYPE = 1
			ELSE 
				PRINT "Type of the 2nd construction point: Virtual "
				HV2_TYPE = 1
			ENDIF
		ELSE
			IF subNUM < 2 
				PRINT "Type of the 1st construction point: Real "
				HV1_TYPE = 0
			ELSE 
				PRINT "Type of the 2nd construction point: Real "
				HV2_TYPE = 0
			ENDIF
		ENDIF
		
		! Change the type of holographic surface:
		FORMAT 3 INT
		PRINT "Type of Surface ", CurSurfNum,
		IF (HV1_TYPE + HV2_TYPE == 1)									# Opposite types
			PRINT " : HOLOGRAM 2"
			SURP CurSurfNum, TYPE, HOLOGRM2	
		ELSE
			PRINT " : HOLOGRAM 1"
			SURP CurSurfNum, TYPE, HOLOGRM1	
		ENDIF
			
	GOTO 401
	
	! Holographic Parameters:
	LABEL HOR			# Diffraction Order
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		PRINT "Holographic Diffraction Order"
		DIF_ORD = SVAL(subSTR$)
		IF DIF_DOE 
			Parameter = 0												# Binary 2 surface
		ELSE
			Parameter = 8
		ENDIF
		GOTO HOLOGRAPHIC
	LABEL HWL			# Construction Wavelength
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		PRINT "Holographic Construction Wavelength [um]"
		Parameter = 7
		GOTO HOLOGRAPHIC
	LABEL HX1			# X-coordinate of the 1st construction point
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		PRINT "X-coordinate of the 1st construction point"
		Parameter = 1
		GOTO HOLOGRAPHIC
	LABEL HX2			# X-coordinate of the 2nd construction point
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		PRINT "X-coordinate of the 2nd construction point"
		Parameter = 4
		GOTO HOLOGRAPHIC		
	LABEL HY1			# Y-coordinate of the 1st construction point
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		PRINT "Y-coordinate of the 1st construction point"
		Parameter = 2
		GOTO HOLOGRAPHIC
	LABEL HY2			# Y-coordinate of the 2nd construction point
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		PRINT "Y-coordinate of the 2nd construction point"
		Parameter = 5
		GOTO HOLOGRAPHIC			
	LABEL HZ1			# Z-coordinate of the 1st construction point
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		PRINT "Z-coordinate of the 1st construction point"
		Parameter = 3
		GOTO HOLOGRAPHIC
	LABEL HZ2			# Z-coordinate of the 2nd construction point
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		PRINT "Z-coordinate of the 2nd construction point"
		Parameter = 6	
		
	LABEL HOLOGRAPHIC
	! Same command format, different parameter number in the LDE:	
		FORMAT 3 INT
		IF (!SurfaceSpecific)
			subNUM = SVAL(subSTR$)
			
			IF (Parameter == 8) THEN PRINT "Order               : ", subNUM
			FORMAT 8.5
			IF (Parameter > 0) & (Parameter < 7) THEN PRINT "Value               : ", subNUM
			IF (Parameter == 7) 
				subNUM = subNUM/ 1000.0									# convert to micrometers
				PRINT "Wavelength          : ", subNUM
			ENDIF
			
            IF (DIF_DOE == 1)
                HCT_wavelength = subNum                                 # Binary 2 construction wavelength for conversion
            ENDIF
            
			IF !((DIF_DOE == 1) & (Parameter == 7))
				SURP CurSurfNum, PARM, subNUM, Parameter  					# Zemax OS Surface Property 
			ENDIF
		ELSE
			subSTR$ = $GETSTRING(curLine$, 3)
			subNUM = SVAL(subSTR$)
			IF (Parameter == 7) THEN subNUM = subNUM/ 1000.0		
			
			IntToStr = VEC2(SpecSurfNum)
			GOSUB FormatInteger
			
			IF (VEC2(SpecSurfNum) == SpecSurfNum)
				PRINT "Surface             : ", SpecSurfNum
				IF DIF_DOE == 0
					SURP SpecSurfNum, PARM, subNUM, Parameter  
				ENDIF
			ELSE
				PRINT "Surface             : ", VEC2(SpecSurfNum),
				IntToStr = SpecSurfNum
				GOSUB FormatInteger				
				PRINT "   (CodeV S", SpecSurfNum,
				PRINT ")"
				IF DIF_DOE == 0
					SURP VEC2(SpecSurfNum), PARM, subNUM, Parameter  
				ENDIF
			ENDIF	
			
			IF (Parameter == 8 || (DIF_DOE && Parameter == 0)) THEN PRINT "Order               : ", subNUM
			FORMAT 8.5
			IF (Parameter > 0) & (Parameter < 7) THEN PRINT "Value               : ", subNUM
			IF (Parameter == 7) THEN PRINT "Wavelength          : ", subNUM			
		ENDIF
	GOTO 401

	LABEL INI   		# Designer initials
		PRINT "Designer Initials"
	GOTO 411															# Nothing to convert

	LABEL INS  			# Insert surfaces (Surface-Specific format)
		PRINT "Surface Insertion command"
		FORMAT 3 INT
		PRINT "Current Number of Surfaces ", NSUR() +1
	GOTO 411
	
	LABEL K				# adding the value of the conic constant) 
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)
			subNUM = SVAL(subSTR$)
			FORMAT 14.5
			PRINT "Conic Constant      : ", subNUM
			SURP CurSurfNum, CONI, subNUM 								# Zemax OS Surface Property 
		ENDIF
	GOTO 401
	
	LABEL LEN			# Initialize a new lens file
		PRINT "Mode switch: initialize a new lens file."
	GOTO 411
	
	LABEL NAO			# Numeric Aperture in object space
		! Set up the type of system aperture to "NA"
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)
			SYSP 10, 2													# Zemax OS System Property 
			! Set-up the aperture value
			subNUM = SVAL(subSTR$)
			FORMAT 14.5
			PRINT "System Aperture  :      NA "
			PRINT "Aperture value   : ", subNUM
			SYSP 11, subNUM 											# Zemax OS System Property 
		ENDIF
	GOTO 401
	
	LABEL PIM			# Solving for paraxial image distance
		IF ((subSTR$ $== "") | (subSTR$ $== "YES"))
			PRINT "Solving for Paraxial Image Distance"
			SOLVETYPE NSUR()-1, TM, 0, 0								# Zemax OS Solve Command
		ENDIF
	GOTO 401
	
	LABEL PRV			# Start of a custom glass catalog description
			PRINT "Private glass catalog section"
			WITHIN_PRV_SECTION = 1
	GOTO 401
	
	LABEL PWL
		PRINT "Wavelength for the following indexes"
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
	GOTO 401
	
	LABEL RET 			# Solve to coordinate return: Orientation, XYZ 
	IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (SurfaceSpecific)
			PRINT "Change surface type to COORDBRK..."
			SURP CurSurfNum, TYPE, COORDBRK								# Zemax OS Surface Property
			FORMAT 3 INT
			PRINT "Current surface: ", CurSurfNum
			PRINT "Coordinate return solve: Orientation, XYZ to surf: ", VEC2(SpecSurfNum)
			SURP CurSurfNum, 76, 3										# Zemax OS Surface Property
			SURP CurSurfNum, 77, VEC2(SpecSurfNum)						# Zemax OS Surface Property
		ENDIF
	GOTO 401

	LABEL REX			# Half-width of the rectangular aperture
	LABEL REY 
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)
			! Setting type of aperture to rectangular:
			SURP CurSurfNum, ATYP, 4 	
			
			num_params = 0 
			GOSUB GetNumParamsInline			
			IF (num_params > 2)
				subSTR$ = $GETSTRING(curLine$, num_params)
			ENDIF 
			
			subNUM = SVAL(subSTR$)
			
			IF (subNUM > 0.0)
				PRINT "Rectangular Aperture"
				FORMAT 6 INT
				PRINT "Surf ", CurSurfNum,
				FORMAT 8.5

				IF (comHDR$ $== "REX")
					PRINT "  Half-width, X direction  : ", subNUM
					SURP CurSurfNum, APP1, subNUM 						# Max Radius for CIR OBS 
				ELSE	# "REY"
					PRINT "  Half-width, Y direction  : ", subNUM
					SURP CurSurfNum, APP2, subNUM 						# Max Radius for CIR OBS 
				ENDIF
			ENDIF
		ENDIF
	GOTO 401
	
	LABEL RDM			# Use surface radius instead of curvature
		IF (subSTR$ $== "") 
			PRINT "Mode switch: use surface radius instead of curvature."
			RadiusModeOn = 1				# Toggle radius/curvature flag
			GOTO 401	
		ENDIF
		
		subD$ = $LEFTSTRING(subSTR$, 1)
		IF (subD$ $== "N") | (subD$ $== "n") 
			PRINT "Mode switch: use surface curvature instead of radius."
			RadiusModeOn = 0				# Toggle radius/curvature flag
			GOTO 401
		ELSE
			PRINT "Mode switch: use surface radius instead of curvature."
			RadiusModeOn = 1				# Toggle radius/curvature flag
			GOTO 401
		ENDIF
	GOTO 401
	
	LABEL RDY			# Radius insertion (Surface-Specific format)
		IF subSTR$ $== "" THEN GOTO 401	
		IF (SurfaceSpecific)
			subSTR$ = $GETSTRING(curLine$, 3)
			IF subSTR$ $!= ""
				PRINT "Radius Insertion",
				FORMAT 6 INT
				IF (VEC2(SpecSurfNum) == SpecSurfNum)
					PRINT " at surf : ", SpecSurfNum
				ELSE
					PRINT " at surf : ", VEC2(SpecSurfNum),
					IF (VEC2(SpecSurfNum) < 10) THEN FORMAT 1 INT
					IF ((VEC2(SpecSurfNum) > 9) & (VEC2(SpecSurfNum) < 100)) THEN FORMAT 2 INT
					IF (VEC2(SpecSurfNum) > 100) THEN FORMAT 3 INT				
					PRINT "   (CodeV S", SpecSurfNum,
					PRINT ")"
				ENDIF
				
				subNUM = SVAL(subSTR$)
				IF (abso(subNUM) > epsilon) 
					FORMAT 14.5
					IF (RadiusModeOn)
						PRINT "Radius           : ", subNUM
						subNUM = 1.0 / subNUM 							# use Curvature not radius
					ELSE
						PRINT "Curvature        : ", subNUM
					ENDIF
					SURP VEC2(SpecSurfNum), CURV, subNUM				# Zemax OS Surface Property
				ELSE
					PRINT "Radius           :       Infinity"
				ENDIF
	
			ENDIF
		ENDIF
	
	GOTO 401
	
	LABEL REF			# The primary wavelength number
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		subNUM = SVAL(subSTR$)
		FORMAT 5 INT
		PRINT "Primary Wave Number : ", subNUM
		SYSP 200, subNUM 												# Zemax OS System Property 
	GOTO 401
	
	LABEL S				# Surface Increment, also Radius-Curvature, Thickness, Glass
	    CurSurfIsaMirror = 0
		CurSurfNumStore = CurSurfNumStore  + 1
		CurSurfNum = VEC2(CurSurfNumStore)
		CurSurfLine$  = curLine$										# buffer the current line content
				
		! Reset stored tilt/decenter parameters
		ADEstore = 0.0				
		BDEstore = 0.0				
		CDEstore = 0.0		
		XDEstore = 0.0				
		YDEstore = 0.0				
		ZDEstore = 0.0			
		LeadDummySurfIn = 0			
		TrailDummySurfIn = 0		
		TargetSurfPosition = 0	
		
		FORMAT 14 INT
		IF (CurSurfNum == CurSurfNumStore)
			PRINT "Surface          : ", CurSurfNum
		ELSE
			PRINT "Surface          : ", CurSurfNum, 
			IF (CurSurfNumStore < 10) THEN FORMAT 1 INT
			IF ((CurSurfNumStore > 9) & (CurSurfNumStore < 100)) THEN FORMAT 2 INT
			IF (CurSurfNumStore > 100) THEN FORMAT 3 INT
			PRINT "   (CodeV S", CurSurfNumStore,
			PRINT ")"
		ENDIF
		LABEL 406
		subSTR$ = $GETSTRING(curLine$, 2)								# Get the radius
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		subNUM = SVAL(subSTR$)
		IF (abso(subNUM) > epsilon) 
			FORMAT 14.5
			IF (RadiusModeOn)
				PRINT "Radius           : ", subNUM
				subNUM = 1.0 / subNUM 									# use Curvature not radius
			ELSE
				PRINT "Curvature        : ", subNUM
			ENDIF
			SURP CurSurfNum, CURV, subNUM								# Zemax OS Surface Property
		ELSE
			PRINT "Radius           :       Infinity"
		ENDIF
		
		subSTR$ = $GETSTRING(curLine$, 3)								# Get the thickness
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		subNUM = SVAL(subSTR$)
		
		! For Image Surface: Transfer non-zero thickness to a dummy surface:	
		subSTR$ = $GETSTRING(curLine$, 1)
		
		IF (subSTR$ $!= "SI") 
			IF (abso(subNUM) < 1.0e9) 
				FORMAT 14.5
				SURP CurSurfNum, THIC, subNUM							# Zemax OS Surface Property
				PRINT "Thickness        : ", subNUM
			ELSE
				PRINT "Thickness        :       Infinity"
			ENDIF
		ELSE				
			IF (abso(subNUM) > epsilon)
				PRINT "Thickness        : ", subNUM
				GOSUB InsertLeadingDummySurface
				SURP CurSurfNum -1, TYPE, STANDARD   					# Zemax OS Surface Property

				SURP CurSurfNum -1, THIC, subNUM						# Zemax OS Surface Property
				PRINT "Transfer Image Surface Thickness"
			
				SURP CurSurfNum -1, COMM, "IMA Surf Thickness Transfer"	# Zemax OS Surface Property
			ENDIF	
		ENDIF	
		
		subSTR$ = $GETSTRING(curLine$, 4)								# Get the glass type
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (subSTR$ $== "REFL")
			CurSurfIsaMirror = 1
			PRINT "Surface is a Mirror"
			SURP CurSurfNum, GLAS, MIRROR
			GOTO 407
		ENDIF
		IF (subSTR$ $!= "AIR") 
			GOSUB CleanupGlassName
			IF (subD$ $!= "Model")
				PRINT "Glass            :      ", subSTR$
			ELSE
				FORMAT 5.2
				PRINT "Glass (model)    :      ", SVAL(subR$),
				FORMAT 4.1
				PRINT ",",subNUM
			ENDIF
            PRV_GLASSES_USED_IN_FILE$ = PRV_GLASSES_USED_IN_FILE$ + " " subSTR$            
            PRV_NUMBER_OF_GLASS_IN_FILE = PRV_NUMBER_OF_GLASS_IN_FILE + 1
			SURP CurSurfNum, GLAS, subSTR$								# Zemax OS Surface Property
		ELSE
			PRINT "Glass            :            N/A"
		ENDIF
		LABEL 407
	GOTO 401

	LABEL SI			# Image Plane Surface
		PRINT "Image Plane Surface"
	    CurSurfNumStore = CurSurfNumStore  + 1
		CurSurfNum = VEC2(CurSurfNumStore)
		
		! Reset stored tilt/decenter parameters
		ADEstore = 0.0				
		BDEstore = 0.0				
		CDEstore = 0.0		
		XDEstore = 0.0				
		YDEstore = 0.0				
		ZDEstore = 0.0			
		LeadDummySurfIn = 0			
		TrailDummySurfIn = 0			
		TargetSurfPosition = 0		
	GOTO 406
	
	LABEL SK			# Surface K
		PRINT "Surface K "
	    CurSurfNumStore = CurSurfNumStore  + 1
		CurSurfNum = VEC2(CurSurfNumStore)
	GOTO 406
	
	LABEL SLB			# Attach a label to a surface (in ZOS: Comment Column)	
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)
			PRINT "Placing Surface Label into Comment column"
			SURP CurSurfNum, COMM, subSTR$								# Zemax OS Surface Property
		ENDIF
	GOTO 401
	
	LABEL SO			# Object Plane Surface	
		PRINT "Object Plane Surface"
	GOTO 406
	
	LABEL SPH			# This is to set the surface type to Standard
		IF (!SurfaceSpecific)
			FORMAT 5 INT
			PRINT "Standard Surface             : ", CurSurfNum
			SURP CurSurfNum, TYPE, STANDARD								# Zemax OS Surface Property 
		ENDIF
	GOTO 401
	
	LABEL SPS			# Set special surface
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)	
			FORMAT 5 INT
		
			IF (subSTR$ $== "CN1")
				PRINT "Fresnel Parallel Back Cut, surf  : ", CurSurfNum
				TEMP_STR$ = ""
				GOTO NO_MAPPING_COMPLEX
			ENDIF	

			IF (subSTR$ $== "CN2")
				PRINT "Fresnel Non-Parallel Back Cut, surf  : ", CurSurfNum
				TEMP_STR$ = ""
				GOTO NO_MAPPING_COMPLEX
			ENDIF

			IF (subSTR$ $== "CPC")
				PRINT "Compound Parabolic Concentrator, surf  : ", CurSurfNum
				TEMP_STR$ = ""
				GOTO NO_MAPPING_COMPLEX
			ENDIF		

			IF (subSTR$ $== "ESP")
				PRINT "Extended Spline, surf  : ", CurSurfNum
				TEMP_STR$ = ""
				GOTO NO_MAPPING_COMPLEX
			ENDIF
			
			IF (subSTR$ $== "FRS")
				PRINT "Fresnel Planar Substrate, surf  : ", CurSurfNum
				TEMP_STR$ = ""
				GOTO NO_MAPPING_COMPLEX
			ENDIF

			IF (subSTR$ $== "ODD")
				!TEMP_STR$ = "Odd Polynomial"
				!SURP CurSurfNum, TYPE, "ODDASPHE"							# Zemax OS Surface Property		
				TEMP_STR$ = ""
				GOTO NO_MAPPING_COMPLEX
			ENDIF

			IF (subSTR$ $== "QBF")
				TEMP_STR$ = "Q-Type Asphere (QBF)"
				SURP CurSurfNum, TYPE, "QED_TYPE"								# Zemax OS Surface Property
				SURP CurSurfNum, 11, 0, 1 										# Set Data Type = 0
			ENDIF

			IF (subSTR$ $== "QCN")
				TEMP_STR$ = "Q-Type Asphere (QCN)"
				SURP CurSurfNum, TYPE, "QED_TYPE"								# Zemax OS Surface Property
				SURP CurSurfNum, 11, 1, 1 										# Set Data Type = 1
			ENDIF

			IF (subSTR$ $== "SCN")
				!TEMP_STR$ = "Superconic"
				!SURP CurSurfNum, TYPE, "SUPERCON"							# Zemax OS Surface Property		
				TEMP_STR$ = ""
				GOTO NO_MAPPING_COMPLEX
			ENDIF

			IF (subSTR$ $== "SEG")
				PRINT "Segmented Conic, surf  : ", CurSurfNum
				TEMP_STR$ = ""
				GOTO NO_MAPPING_COMPLEX	
			ENDIF

			IF (subSTR$ $== "XYP")
				!TEMP_STR$ = "XY Polynomial"
				!SURP CurSurfNum, TYPE, "POLYNOMI"							# Zemax OS Surface Property
				TEMP_STR$ = ""
				GOTO NO_MAPPING_COMPLEX
			ENDIF

			IF (subSTR$ $== "ZFR")
				TEMP_STR$ = "Fringe Zernike"
				SURP CurSurfNum, TYPE, "FZERNSAG"							# Zemax OS Surface Property
			ENDIF
				
			IF (subSTR$ $== "ZRN") 
				!TEMP_STR$ = "Standard Zernike"
				!SURP CurSurfNum, TYPE, "SZERNSAG"							# Zemax OS Surface Property					
				TEMP_STR$ = ""
				GOTO NO_MAPPING_COMPLEX
			ENDIF
			
			PRINT "Adding ", TEMP_STR$, ", surf  : ", CurSurfNum		
		ENDIF
	GOTO 401
	
	LABEL SCO			# Special Surface Operands
		IF TEMP_STR$ $== "" THEN GOTO 401								# surface mapping is not supported 
		
		IF (!SurfaceSpecific)
			
			FORMAT 8.5
			! change to upper case subSTR$
			StrXTD$ = subSTR$
			GOSUB UpperStrXTD
			
			subXTR$ = $GETSTRING(curLine$, 3)
			subNUM = SVAL(subXTR$)										# Num value
			
			IF (subSTR$ $== "NRADIUS")									# Normalization radius 
				FORMAT 8.5
				IF (abso(subNUM) < epsilon) THEN GOTO 411  # nothing to do
				PRINT TEMP_STR$,", Norm Radius : ", subNUM
				
				IF ((TEMP_STR$ $== "Q-Type Asphere (QBF)") | (TEMP_STR$ $== "Q-Type Asphere (QCN)"))
					SURP CurSurfNum, 11, subNUM, 3							# Zemax OS Surface Property
				ENDIF
				
				IF (TEMP_STR$ $== "Fringe Zernike")
					SURP CurSurfNum, 11, subNUM, 2							# Zemax OS Surface Property
				ENDIF				
			ENDIF
			
			IF (subSTR$ $== "K")
				FORMAT 8.5
				IF (abso(subNUM) < epsilon) THEN GOTO 411  # nothing to do
				IF (abso(subNUM) < epsilon) THEN GOTO 411  # nothing to do
				PRINT TEMP_STR$, ", Conic: ", subNUM 
				SURP CurSurfNum, CONI, subNUM							# Zemax OS Surface Property
			ENDIF
			
			! Check for surface coefficients:
			StrXTD$ = $LEFTSTRING(subSTR$,1)
	
			IF (StrXTD$ $== "C") 
				! Extract number from C-identifier
				StrXTD$ = $RIGHTSTRING(subSTR$,SLEN(subSTR$) -1) 
				subINT = SVAL(StrXTD$)
				
				! Q-Type Asphere
				IF ((TEMP_STR$ $== "Q-Type Asphere (QBF)") | (TEMP_STR$ $== "Q-Type Asphere (QCN)"))
					IF (subINT == 17)
						FORMAT 16.8 EXP
						PRINT TEMP_STR$, ", Conic: ", subNUM 
						SURP CurSurfNum, CONI, subNUM		 				# Zemax OS Surface Property
						GOTO 401
					ENDIF 
					
					IF ((subINT > 2) & (subINT < 17))
						subINT = subINT - 3
						
						!Check if the Maximum Term is sufficient
						subVal = SPRX(CurSurfNum, 11, 2)
                        
                        ! only increaes by 1 for QBF (8056)
                        IF (TEMP_STR$ $== "Q-Type Asphere (QBF)") THEN subINT = subINT + 1
						
                        IF (subINT < 10) 
							FORMAT 1 INT 
						ELSE 
							FORMAT 2 INT 
						ENDIF
						IF (subVal < subINT) 
							PRINT "Increasing Maximum Term to ", subINT
							SURP CurSurfNum, 11, subINT, 2	
						ENDIF
						subINT = subINT - 1
						
						IF ((subINT >= 0) & (subINT <= 100)) 
							IF (subINT < 10) 
								FORMAT 1 INT 
							ELSE 
								IF (subINT < 100) 
									FORMAT 2 INT 
								ELSE 
									FORMAT 3 INT 
								ENDIF
							ENDIF
							PRINT TEMP_STR$, ", A",
							PRINT subINT,
							FORMAT 16.8 EXP
							PRINT " coefficient : ", subNUM
							
							subINT = subINT + 4
                            
							SURP CurSurfNum, 11, subNUM, subINT 			# Zemax OS Surface Property
						ENDIF
					ELSE
						GOTO 411
					ENDIF
				ENDIF
				
				! Zernike Fringe Surface:
				if (TEMP_STR$ $== "Fringe Zernike")
					IF (subINT == 1)
						FORMAT 16.8 EXP
						PRINT TEMP_STR$, ", Conic: ", subNUM 
						SURP CurSurfNum, CONI, subNUM		 				# Zemax OS Surface Property
						GOTO 401
					ENDIF 
					
					IF (subINT == 3)
						FORMAT 8.5
						IF (abso(subNUM) < epsilon) THEN GOTO 411  # nothing to do
						PRINT TEMP_STR$,", Norm Radius : ", subNUM
						SURP CurSurfNum, 11, subNUM, 2							# Zemax OS Surface Property
					ENDIF 
					
					IF ((subINT > 3) & (subINT < 41))
						subINT = subINT - 3
						
						!Check if the Maximum Term is sufficient
						subVal = SPRX(CurSurfNum, 11, 1)
				
						IF (subINT < 10) 
							FORMAT 1 INT 
						ELSE 
							FORMAT 2 INT 
						ENDIF
						IF (subVal < subINT) 
							PRINT "Increasing Maximum Term to ", subINT
							SURP CurSurfNum, 11, subINT, 1	
						ENDIF
						
						IF ((subINT >= 0) & (subINT <= 100)) 
							IF (subINT < 10) 
								FORMAT 1 INT 
							ELSE 
								IF (subINT < 100) 
									FORMAT 2 INT 
								ELSE 
									FORMAT 3 INT 
								ENDIF
							ENDIF
							PRINT TEMP_STR$, ", A",
							PRINT subINT,
							FORMAT 16.8 EXP
							PRINT " coefficient : ", subNUM
							
							subINT = subINT + 2
							SURP CurSurfNum, 11, subNUM, subINT 			# Zemax OS Surface Property
						ENDIF
					ELSE
						GOTO 411
					ENDIF
					
				ENDIF
				
				
			ENDIF
		ENDIF
	GOTO 401
	
	LABEL STO			# Stop Surface
		IF (!SurfaceSpecific)
			PRINT "Stop Surface"
			STOPSURF CurSurfNum 										# Zemax OS System Property 
			StopSurfNum = CurSurfNum
		ELSE 
			subSTR$ = $GETSTRING(curLine$, 2)
			IF subSTR$ $!= ""
				PRINT "Stop Insertion",
				FORMAT 6 INT
				IF (VEC2(SpecSurfNum) == SpecSurfNum)
					PRINT " at surf :   ", SpecSurfNum
				ELSE
					PRINT " at surf :   ", VEC2(SpecSurfNum),
					IF (VEC2(SpecSurfNum) < 10) THEN FORMAT 1 INT
					IF ((VEC2(SpecSurfNum) > 9) & (VEC2(SpecSurfNum) < 100)) THEN FORMAT 2 INT
					IF (VEC2(SpecSurfNum) > 100) THEN FORMAT 3 INT				
					PRINT "   (CodeV S", SpecSurfNum,
					PRINT ")"
				ENDIF
				
				STOPSURF VEC2(SpecSurfNum) 								# Zemax OS System Property 
				StopSurfNum = VEC2(SpecSurfNum)
			ENDIF
		ENDIF
	GOTO 401
	
	! Group Control Parameters:
	LABEL CCY			# Surface curvature control (rotational symmetry)
		IF subSTR$ $== "" THEN GOTO 401	
		PRINT "Curvature Group Control"
		Parameter = -1
		GOTO GROUP_CONTROL
		
	LABEL THC			# Thickness coupling group control
		IF subSTR$ $== "" THEN GOTO 401	
		PRINT "Thickness Group Control"
		Parameter = 0
		GOTO GROUP_CONTROL
		
	LABEL ADC			# X axis tilt coupling group control
	IF subSTR$ $== "" THEN GOTO 401										# No operand read
		PRINT "X-axis tilt Group Control"
		Parameter = 3
		GOTO GROUP_CONTROL
			
	LABEL BDC			# Y axis tilt coupling group control
	IF subSTR$ $== "" THEN GOTO 401										# No operand read
		PRINT "Y-axis tilt Group Control"
		Parameter = 4
		GOTO GROUP_CONTROL
		
	LABEL CDC			# Z axis tilt coupling group control
	IF subSTR$ $== "" THEN GOTO 401										# No operand read
		PRINT "Z-axis tilt Group Control"
		Parameter = 5
		GOTO GROUP_CONTROL
		
	LABEL XDC			# X decenter coupling group control		
	IF subSTR$ $== "" THEN GOTO 401										# No operand read
		PRINT "X decenter Group Control"
		Parameter = 1
		GOTO GROUP_CONTROL
		
	LABEL YDC			# Y decenter coupling group control		
	IF subSTR$ $== "" THEN GOTO 401										# No operand read
		PRINT "Y decenter Group Control"
		Parameter = 2
		GOTO GROUP_CONTROL
		
	LABEL ZDC			# Z decenter coupling group control
	IF subSTR$ $== "" THEN GOTO 401										# No operand read
		PRINT "Z decenter Group Control"
		Parameter = 3

	LABEL GROUP_CONTROL
	ExtraParameter = 0
		
	IF (SurfaceSpecific)
		GOSUB CheckSurfRange											# SurfStart, SurfStop
		num_params = 0 
		GOSUB GetNumParamsInline
		IF ((num_params == 3) | (num_params == 4))
			subSTR$ = $GETSTRING(curLine$, 3)
			subNUM = SVAL(subSTR$)	
			
			! Update the index arrays:
			FORMAT 5 INT
			FOR i, SurfStart, SurfStop, 1
				CtrlSurface(CurCtrlInstance) = i						# Code V surface number
				CtrlGroup(CurCtrlInstance) = subNUM						# Group encoded
				CtrlCommand(CurCtrlInstance) = Parameter  				# Type of the instance
				
				IF ((subNUM == 0) | (subNUM == 100))
					FORMAT 14 INT
					IF (VEC2(i) == i)
						PRINT "Surface          : ", VEC2(i)
					ELSE 
						PRINT "Surface          : ", VEC2(i),
						IntToStr = i
						GOSUB FormatInteger
						PRINT "   (CodeV S", i,
						PRINT ")"
					ENDIF
				ENDIF 
				
				IF (subNUM == 0)
					PRINT "Set Solve Type to Variable"
					subINT = VEC2(i)
					IF (Parameter == -1) & (subINT >= 0) 
						SOLVETYPE subINT, CV
					ENDIF
					IF (Parameter == 0) & (subINT >= 0) 
						SOLVETYPE subINT, TV
					ENDIF
					IF (subINT < 0) 									# error trap
						FORMAT 3 INT
						PRINT "WARNING: Invalid Surface Number: ", subINT 
					ENDIF
					IF (((Parameter > 0) & (Parameter < 7)) | (Parameter > 14))	# variable decenter/ tilt
						IntToStr = Parameter
						GOSUB FormatInteger
						subD$ = $STR(Parameter)
						subR$ = "PV_" + subD$
						SOLVETYPE subINT, subR$
					ENDIF
				ENDIF
				IF (subNUM == 100) 
					PRINT "Set Solve Type to Fixed"
					subINT = VEC2(i)
					IF (Parameter == -1) & (subINT >= 0) 
						SOLVETYPE subINT, CF							# remove solve for thickness 
					ENDIF
					IF (Parameter == 0) & (subINT >= 0) 
						SOLVETYPE subINT, TF							# remove solve for thickness 
					ENDIF
					IF (subINT < 0) 									# error trap
						FORMAT 3 INT
						PRINT "WARNING: Invalid Surface Number: ", subINT 
					ENDIF
					IF (((Parameter > 0) & (Parameter < 7)) | (Parameter > 14))	# remove solve for decenter/ tilt
						IntToStr = Parameter
						GOSUB FormatInteger
						subD$ = $STR(Parameter)
						subR$ = "PF_" + subD$
						SOLVETYPE subINT, subR$
					ENDIF
				ENDIF
				
				IF (CtrlGroup(CurCtrlInstance) > 0) & (CtrlGroup(CurCtrlInstance) < 100)
					PRINT "Current Group Instance: ", CurCtrlInstance
					IF (Parameter == -1) 
						IF (RadiusModeOn)
							PRINT "Radius of surface ", VEC2(i), 
						ELSE
							PRINT "Curvature of surface ", VEC2(i), 
						ENDIF
					ENDIF
					IF (Parameter == 0) THEN PRINT "Thickness of surface ", VEC2(i), 
					IF (Parameter > 0) & (Parameter < 7) THEN "Parameter of surface ", VEC2(i), 
					IF (Parameter > 14) THEN "Coefficient of surface ", VEC2(i), 
					PRINT " coupled to control group ", subNUM
				ENDIF
				!Increment the current coupling-group instance number
				CurCtrlInstance = CurCtrlInstance +1
			NEXT i
		ENDIF
		
		! Add a surface to a control group and change thickness
		IF (num_params == 4)
			ExtraParameter = 1
			GOTO THC_4_Params
		ENDIF
	ELSE
		num_params = 0 
		GOSUB GetNumParamsInline
		
		IF (num_params == 2)
			IF (LeadDummySurfIn == 1) THEN CurSurfNum = CurSurfNum - 1
			FORMAT 14 INT
			PRINT "Surface          : ", CurSurfNum
			subNUM = SVAL(subSTR$)

			IF (subNUM == 0) 
				PRINT "Set Solve Type to Variable"
				subINT = CurSurfNum
				IF (Parameter == -1) & (subINT >= 0) 
					SOLVETYPE subINT, CV
				ENDIF
				IF (Parameter == 0) & (subINT >= 0) 
					SOLVETYPE subINT, TV
				ENDIF
				IF (subINT < 0) 										# error trap
					FORMAT 3 INT
					PRINT "WARNING: Invalid Surface Number: ", subINT 
				ENDIF
				IF (((Parameter > 0) & (Parameter < 7)) | (Parameter > 14))	# variable decenter/ tilt
					IntToStr = Parameter
					GOSUB FormatInteger
					subD$ = $STR(Parameter)
					subR$ = "PV_" + subD$
					SOLVETYPE subINT, subR$
				ENDIF
			ENDIF
			IF (subNUM == 100) 
				PRINT "Set Solve Type to Fixed"
				subINT = CurSurfNum
				IF (Parameter == -1) & (subINT >= 0) 
					SOLVETYPE subINT, CF								# remove solve for thickness 
				ENDIF
				IF (Parameter == 0) & (subINT >= 0) 
					SOLVETYPE subINT, TF								# remove solve for thickness 
				ENDIF
				IF (subINT < 0) 										# error trap
					FORMAT 3 INT
					PRINT "WARNING: Invalid Surface Number: ", subINT 
				ENDIF	
				IF (((Parameter > 0) & (Parameter < 7)) | (Parameter > 14))	# remove solve for decenter/ tilt
					IntToStr = Parameter
					GOSUB FormatInteger
					subD$ = $STR(Parameter)
					subR$ = "PF_" + subD$
					SOLVETYPE subINT, subR$
				ENDIF
			ENDIF
			
			IF (LeadDummySurfIn == 1) THEN CurSurfNum = CurSurfNum + 1 
		ENDIF

		! Control by a group
		LABEL THC_4_Params
		IF (num_params == 3 + ExtraParameter)
			FORMAT 3 INT
			PRINT "Control Command:   1"
			subINT = SVAL(subSTR$)	

			PRINT "Control Group: ", subINT
			subSTR$ = $GETSTRING(curLine$, 3 + ExtraParameter)
			subNUM = SVAL(subSTR$)	

			FOR i, 1, NumCtrlInstance, 1
				IF ((CtrlGroup(i) == subINT) & (CtrlCommand(i) == 1))
					IntToStr = CtrlSurface(i)
					GOSUB FormatInteger	
					PRINT "Surface : ", CtrlSurface(i)
			
					subVal = SPRO(CtrlSurface(i), 3)
					IF (abso(subNum) > epsilon)	
						FORMAT 10.5
						IF (Parameter == -1) 
							IF (abso(subNUM + subVal) > epsilon)
								IF (RadiusModeOn)
									PRINT " Radius changed from ", subVal,
									PRINT "     to ", subNUM + subVal
									SURP CtrlSurface(i), CURV, 1/(subNUM + subVal)	# Zemax OS Surface Property
								ELSE
									PRINT " Curvature changed from ", subVal,
									PRINT "     to ", subNUM + subVal
									SURP CtrlSurface(i), CURV, subNUM + subVal		# Zemax OS Surface Property
								ENDIF
							ENDIF
						ENDIF
						IF (Parameter == 0) 
							PRINT " Thickness changed from ", subVal,
							PRINT "     to ", subNUM + subVal
							SURP CtrlSurface(i), THIC, subNUM + subVal	# Zemax OS Surface Property
						ENDIF
						IF (((Parameter > 0) & (Parameter < 7)) | (Parameter > 14)) 
							PRINT " Parameter changed from ", subVal,
							PRINT "     to ", subNUM + subVal
							SURP CtrlSurface(i), PARM, subNUM + subVal, Parameter	# Zemax OS Surface Property
						ENDIF
					ENDIF
				ENDIF
			NEXT i
		ENDIF
	ENDIF
	GOTO 401
	
	LABEL THG			# Thermal gradient surface -- not supported
		IF (!SurfaceSpecific)
			PRINT "Thermal gradient not supported, converted to even asphere."
			PRINT " adding even asphere surf    : ", CurSurfNum
			SURP CurSurfNum, TYPE, EVENASPH   							# Zemax OS Surface Property 
		ENDIF
	GOTO 401

	LABEL THI 			# Thickness insertion (Surface-Specific format)
		IF subSTR$ $== "" THEN GOTO 401	
		IF (SurfaceSpecific)
			subSTR$ = $GETSTRING(curLine$, 3)
			IF subSTR$ $!= ""
				PRINT "Thickness Insert",
				FORMAT 6 INT
				IF (VEC2(SpecSurfNum) == SpecSurfNum)
					PRINT " at surf : ", SpecSurfNum
				ELSE
					PRINT " at surf : ", VEC2(SpecSurfNum),
					IF (VEC2(SpecSurfNum) < 10) THEN FORMAT 1 INT
					IF ((VEC2(SpecSurfNum) > 9) & (VEC2(SpecSurfNum) < 100)) THEN FORMAT 2 INT
					IF (VEC2(SpecSurfNum) > 100) THEN FORMAT 3 INT				
					PRINT "   (CodeV S", SpecSurfNum,
					PRINT ")"
				ENDIF
				
				subNUM = SVAL(subSTR$)
				IF (abso(subNUM) < 1.0e9) 
					FORMAT 14.5
					SURP VEC2(SpecSurfNum), THIC, subNUM				# Zemax OS Surface Property
					PRINT "Thickness        : ", subNUM
				ELSE
					PRINT "Thickness        :       Infinity"
				ENDIF
			
				! Insert stop if thickness is zero and no previous assignement
				IF (abso(subNUM) < epsilon) & (!StopSurfNum)
					PRINT "Insert Stop Surface"
					STOPSURF VEC2(SpecSurfNum) 							# Zemax OS System Property 
					StopSurfNum = VEC2(SpecSurfNum)
				ENDIF
			ENDIF
		ENDIF
	GOTO 401
	
	LABEL THM 			#	Thickness of the mirror substrate 
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
			IF (!SurfaceSpecific)	
				subNUM = SVAL(subSTR$)
				FORMAT 3 INT
				PRINT "Surface ", CurSurfNum,
				FORMAT 16.8 EXP
				PRINT " Setting mirror substrate thickness to ", subNUM
				SURP CurSurfNum, 98, subNUM								# Zemax OS Surface Property 
			ENDIF
		ENDIF
	GOTO 401 
	
	LABEL TITLE 		# Lens system title
		GOTO TIT	
	GOTO 401
		
	LABEL TIT			# Update lens title 
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (ConfigSpecific)
			MCE_Operand$ = $GETSTRING(curLine$, 3)
			GOTO TIT_MCE
		ENDIF
	
		num_params = 0 
		GOSUB GetNumParamsInline
		IF (num_params > 2)
			FOR j, 3, num_params, 1
				subSTR$ = subSTR$ + BLANK$ + $GETSTRING(curLine$, j)	# Get the lens title
			NEXT j
		ENDIF
	
		! Excluding the header operand:
		curLine$ = subSTR$
		num_params = 0 
		GOSUB GetNumParamsInline
		
		subGSTR$ = $LEFTSTRING(subSTR$, 1)
		if (subGSTR$ $== "'") THEN subSTR$ = $RIGHTSTRING(subSTR$, SLEN(subSTR$) - 1)
		subGSTR$ = $RIGHTSTRING(subSTR$,1)
		if (subGSTR$ $== "'") THEN subSTR$ = $LEFTSTRING(subSTR$, SLEN(subSTR$) - 1)
		PRINT "Lens Title       : ", subSTR$
		SYSP 16, subSTR$ 												# Zemax OS System Property 
	GOTO 401
	
	LABEL VLX			# Fract. Ent. Pupil radius clipped off -X
		UPPER_XVIG_TYPE = 0
		GOTO X_VIGNETTING_SET
		
	LABEL VUX			# Fract. Ent. Pupil radius clipped off +X
        UPPER_XVIG_TYPE = 1
		
	LABEL X_VIGNETTING_SET
	! Determine the number of the parameters:
    
		num_params = 0 
		GOSUB GetNumParamsInline
		num_params = num_params - 1		# exclude the header operand
		
		FORMAT 2 INT,
		PRINT "Number of parameters : ", num_params  		
		
		IF (num_params < 1) 
			PRINT "Error: (Vignetting Compression X operand mapping)."
			PRINT "Number of fields is undefined."
			GOTO 411
		ENDIF
		
		! Limit the maximum number of parameters to parse
		IF (num_params >= NumFields) THEN num_params = NumFields
		FOR  j, 1, num_params , 1
			subSTR$ = $GETSTRING(curLine$, j+1)							# Get the j-th value
			IF subSTR$ $== "" THEN GOTO 401								# No operand read
			IF (UPPER_XVIG_TYPE)
				VU_X(j) = SVAL(subSTR$)
				IF (VU_X(j) > 1) | (VU_X(j) < 0) THEN VU_X(j) = 0 			
			ELSE
				VL_X(j) = SVAL(subSTR$) 
				IF (VL_X(j) > 1) | (VL_X(j) < 0) THEN VL_X(j) = 0 
			ENDIF	
			! Convert vignetting only when factor are between 0 and 1
 
			IF (VU_X(j) != 0) | (VL_X(j) != 0)
				IntToStr = j
				GOSUB FormatInteger	
				PRINT "For field X ", j,
				FORMAT 8.5
				PRINT " compression is ", (VL_X(j) + VU_X(j))/2.0
				SYSP 107, j, (VL_X(j) + VU_X(j))/2.0					# Zemax OS System Property
				PRINT "              decenter is    ", (VL_X(j) - VU_X(j))/2.0		
				SYSP 105, j, (VL_X(j) - VU_X(j))/2.0					# Zemax OS System Property 
			ENDIF
		NEXT j 
	GOTO 401
	
	LABEL VLY			# Fract. ent. Pupil radius clipped off -Y
		UPPER_YVIG_TYPE = 0
		GOTO Y_VIGNETTING_SET
		
	LABEL VUY			# Fract. Ent. Pupil radius clipped off +Y
		UPPER_YVIG_TYPE = 1
		
	LABEL Y_VIGNETTING_SET
		! Determine the number of the parameters:
		num_params = 0 
		GOSUB GetNumParamsInline
		num_params = num_params - 1										# exclude the header operand	
		
		FORMAT 2 INT,
		PRINT "Number of parameters : ", num_params  	
		
		IF (num_params < 1) 
			PRINT "Error: (Vignetting Compression Y operand mapping)."
			PRINT "Number of fields is undefined."
			GOTO 411
		ENDIF
		
		! Limit the maximum number of parameters to parse
		IF (num_params >= NumFields) THEN num_params = NumFields
		
		FOR  j, 1, num_params , 1
			subSTR$ = $GETSTRING(curLine$, j+1)							# Get the j-th value
			IF subSTR$ $== "" THEN GOTO 401								# No operand read
			IF (UPPER_YVIG_TYPE)
				VU_Y(j) = SVAL(subSTR$) 
				IF (VU_Y(j) > 1) | (VU_Y(j) < 0) THEN VU_Y(j) = 0 
			ELSE
				VL_Y(j) = SVAL(subSTR$) 
				IF (VL_Y(j) > 1) | (VL_Y(j) < 0) THEN VL_Y(j) = 0 
			ENDIF
			! Convert vignetting only when factor are between 0 and 1

			IF (VU_Y(j) != 0) | (VL_Y(j) != 0)
				IntToStr = j
				GOSUB FormatInteger	
				PRINT "For field Y ", j,
				FORMAT 8.5
				PRINT " compression is ", (VL_Y(j) + VU_Y(j))/2.0
				SYSP 108, j, (VL_Y(j) + VU_Y(j))/2.0					# Zemax OS System Property 
				PRINT "              decenter is    ", (VL_Y(j) - VU_Y(j))/2.0							
				SYSP 106, j, (VL_Y(j) - VU_Y(j))/2.0					# Zemax OS System Property 
			ENDIF
		NEXT j 
	GOTO 401
	
	LABEL WL			# Update the wavelength list 
		IF subSTR$ $== "" THEN GOTO 401		
		IF (ConfigSpecific)
			IF (!PRV_CATALOG_PARSE)
				FORMAT 8.5 
				MCE_Operand$ = $GETSTRING(curLine$, 3)
				PRINT "MCE_Operand ", SVAL(MCE_Operand$)
				GOTO WL_MCE
			ELSE
				GOTO 411
			ENDIF
		ENDIF
		
		num_params = 0 
		GOSUB GetNumParamsInline
		! NumWaves is a global variable
		NumWaves = num_params - 1										# exclude the header operand 
			
		! Set up the number of wavelengths:
		IF (!PRV_CATALOG_PARSE)
			SYSP 201, NumWaves 											# Zemax OS System Property		  		
			IF (NumWaves < 1) 
				PRINT "Error: (Wavelength operand mapping)."
				PRINT "Number of wavelengths is undefined."
				GOTO 411
			ENDIF
			PRINT "Wavelength is in micrometers"
		ENDIF
		
		DECLARE subWL, DOUBLE, 1, num_params  
		FOR  j, 2, num_params , 1
			subSTR$ = $GETSTRING(curLine$, j)							# Get the j-th wavelength
			IF subSTR$ $== "" THEN GOTO 401								# No operand read
			subWL(j) = SVAL(subSTR$) / 1000.0							# make sure the units are um
			IF (!PRV_CATALOG_PARSE)
				FORMAT 2 INT
				PRINT "Wavelength ", j-1,
				FORMAT 8.5
				PRINT "  is ", subWL(j)
				SYSP 202, j-1, subWL(j)									# Zemax OS System Property 
			ELSE
				! Determine min and max wavelengths for the glass catalog
				IF (PRV_WL_MIN > subWL(j)) THEN PRV_WL_MIN = subWL(j)
				IF (PRV_WL_MAX < subWL(j)) THEN PRV_WL_MAX = subWL(j)
			ENDIF
		NEXT j 
		RELEASE subWL
	GOTO 401

	LABEL WTF			# Update the field weight list:
        if (NumFields <= 0) THEN GOTO 401 								# No fields
		DECLARE subWTF, DOUBLE, 1, NumFields+1  
		FOR  j, 2, NumFields+1 , 1
			FORMAT .14
            temp = fieldXYW(3, j - 1)                                   # Get the j'th field
            subSTR$ = $STR(temp)
			IF subSTR$ $== "" THEN GOTO 401								# No operand read
			subWTF(j) = SVAL(subSTR$)
			FORMAT 2 INT
			PRINT "Field ", j-1,
			FORMAT 5.2
			PRINT " weight is ", subWTF(j)
			SYSP 104, j-1, subWTF(j)									# Zemax OS System Property 
		NEXT j 
		RELEASE subWTF
	GOTO 401
	
	LABEL WTW			# Update the wavelength weight list:
		if (NumWaves <= 0) THEN GOTO 401 								# No wavelengths
	
		DECLARE subWLW, DOUBLE, 1, NumWaves+1  
		FOR  j, 2, NumWaves+1 , 1
			subSTR$ = $GETSTRING(curLine$, j)							# Get the j'th wavelength
			IF subSTR$ $== "" THEN GOTO 401								# No operand read
			subWLW(j) = SVAL(subSTR$)
			FORMAT 2 INT
			PRINT "Wavelength ", j-1,
			FORMAT 5.2
			PRINT " weight is ", subWLW(j)
			SYSP 203, j-1, subWLW(j)									# Zemax OS System Property 
		NEXT j 
		RELEASE subWLW
	GOTO 401
	
	LABEL XAN			# X angle (degrees) in object space
		! Set field type to field angle in object space
		SYSP 100, 0														# Zemax OS System Property
		PRINT "Field is set to angle (degrees) in object space."
		
		LABEL 408
		! Test for maximum number of fields
		IF (NumFields > MAX_FIELDS)
			PRINT "Warning: Check maximum fields, all fields may not be used."
			NumFields = MAX_FIELDS
		ENDIF
		DECLARE fieldX, DOUBLE, 1, NumFields
		FOR j, 2, NumFields+1, 1
			#subSTR$   = $GETSTRING(curLine$, j)	
            # Get Xfield value
            FORMAT .14
            temp = fieldXYW(1, j - 1)
            subSTR$ = $STR(temp)
			IF subSTR$ $== "" THEN GOTO 401								# No operand read
			fieldX(j-1) = SVAL(subSTR$)
			IF (j-1 < 10) THEN FORMAT 1 INT
			IF ((j-1 > 9) & (j-1 < 100)) THEN FORMAT 2 INT
			IF (j-1 > 100) THEN FORMAT 3 INT
			PRINT "Field X ", j-1,
			FORMAT 8.5
			PRINT " is ", fieldX(j-1)
			SYSP 102, j-1, fieldX(j-1) 									# Zemax OS System Property 
		NEXT j
		RELEASE fieldX
	GOTO 401
	
	LABEL XDE			# X decenter
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)
			subNUM = SVAL(subSTR$)
			IF (abso(subNUM) < epsilon) THEN GOTO 411  # nothing to do
			
			! Check if an extra surface needs to be inserted
			GOSUB InsertLeadingDummySurface
		
			FORMAT 14.5
			PRINT "X Decenter          : ", subNUM
			SURP CurSurfNum -1, PARM, subNUM, 1   						# Zemax OS Surface Property 
			
			XDEstore = subNUM											# store decenter
		ENDIF
	GOTO 401
	
	LABEL XIM			# Field: X Paraxial Image Height
		! Set field type to 
		SYSP 100, 2 													# Zemax OS System Property 
		PRINT "Field X is set to Paraxial Image Height."
	GOTO 408
		
	LABEL XOB			# Field: X Object Height
		! Set field type to 
		SYSP 100, 1														# Zemax OS System Property 
		PRINT "Field X is set to Object Height."
	GOTO 408
	
	LABEL XRI			# Field: X Real Image Height
		! Set field type to 
		SYSP 100, 3 													# Zemax OS System Property 
		PRINT "Field X is set to Real Image Height."
	GOTO 408
		
	LABEL XTO			# X Toroid Surface
	    PRINT "Warning: X-TOROID used, may need to rotate TOROIDAL surface."
		IF (!SurfaceSpecific)
			FORMAT 5 INT
			PRINT "Adding toroidal surface, surf : ", CurSurfNum
			SURP CurSurfNum, TYPE, TOROIDAL   							# Zemax OS Surface Property 
		ENDIF
	GOTO 401

	LABEL YAN			# Y angle (degrees) in object space
		! Set field type to field angle in object space
		SYSP 100, 0														# Zemax OS System Property
		PRINT "Field Y is set to Angle (degrees) in object space."
		
		LABEL 409
		! Test for maximum number of fields
		IF (NumFields > MAX_FIELDS)
			PRINT "Warning: Check maximum fields, all fields may not be used."
			NumFields = MAX_FIELDS
		ENDIF
		DECLARE fieldY, DOUBLE, 1, NumFields
		FOR j, 2, NumFields+1, 1
			# subSTR$   = $GETSTRING(curLine$, j)							# Get Yfield value
            FORMAT .14
            temp = fieldXYW(2, j - 1)
            subSTR$ = $STR(temp)
			IF subSTR$ $== "" THEN GOTO 401								# No operand read
			fieldY(j-1) = SVAL(subSTR$)
			IF (j-1 < 10) THEN FORMAT 1 INT
			IF ((j-1 > 9) & (j-1 < 100)) THEN FORMAT 2 INT
			IF (j-1 > 100) THEN FORMAT 3 INT
			PRINT "Field Y ", j-1,
			FORMAT 8.5
			PRINT " is ", fieldY(j-1)
			SYSP 103, j-1, fieldY(j-1) 									# Zemax OS System Property 
		NEXT j
		RELEASE fieldY
	GOTO 401
	
	LABEL YDE			# Y decenter
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		IF (!SurfaceSpecific)
			subNUM = SVAL(subSTR$)
			IF (abso(subNUM) < epsilon) THEN GOTO 411  # nothing to do
			
			! Check if an extra surface needs to be inserted
			GOSUB InsertLeadingDummySurface
			
			FORMAT 14.5
			PRINT "Y Decenter          : ", subNUM
			SURP CurSurfNum -1, PARM, subNUM, 2   						# Zemax OS Surface Property 
			YDEstore = subNUM											# store decenter
		ENDIF
	GOTO 401

	LABEL YIM			# Field: Y Paraxial Image Height
		! Set field type to 
		SYSP 100, 2 													# Zemax OS System Property 
		PRINT "Field Y is set to Paraxial Image Height."
	GOTO 409

	LABEL YOB			# Field: Y Object Height
		! Set field type to 
		SYSP 100, 1 													# Zemax OS System Property 
		PRINT "Field Y is set to Object Height."
	GOTO 409
	
	LABEL YRI			# Field: Y Real Image Height
		! Set field type to 
		SYSP 100, 3 													# Zemax OS System Property 
		PRINT "Field Y is set to Real Image Height."
	GOTO 409
	
	LABEL YTO			# Y Toroid Surface 
		FORMAT 5 INT
		PRINT "Adding Toroidal Surface, surf : ", CurSurfNum
		SURP CurSurfNum, TYPE, TOROIDAL   								# Zemax OS Surface Property 
	GOTO 401
	
	LABEL ZDE			# Z decenter of axis
		IF subSTR$ $== "" THEN GOTO 401		# No operand read
		IF (!SurfaceSpecific)
			subNUM = SVAL(subSTR$)
			IF (abso(subNUM) < epsilon) THEN GOTO 411  # nothing to do
			
			! Check if an extra surface needs to be inserted
			GOSUB InsertLeadingDummySurface
	
			FORMAT 14.5
			PRINT "Z position shift    : ", subNUM
			SURP CurSurfNum -1, THIC, subNUM   							# Zemax OS Surface Property 
			SURP CurSurfNum -1, COMM, "Z-shift"							# Zemax OS Surface Property
		
			ZDEstore = subNUM 											# store decenter
			
			! Change surface type to STANDARD if all other T/D are zero
			subNUM = XDEstore + YDEstore 
			subNUM = subNUM + ADEstore + BDEstore + CDEstore
			IF (abso(subNUM) < epsilon)
                SURP CurSurfNum -1, TYPE, STANDARD   						# Zemax OS Surface Property
			ENDIF
		ENDIF
	GOTO 401
	
	LABEL ZOO			# Zoom positions / Multi-Configuration Setup
		! How many parameters?
		num_params = 0 
		GOSUB GetNumParamsInline

		subSTR$ = $GETSTRING(curLine$, 2)								# Get the value
		IF subSTR$ $== "" THEN GOTO 401									# No operand read
		! get the number of configurations:
		IF (num_params == 2) 
			FORMAT 3 INT
			NumberConfigs = SVAL(subSTR$)
			IF (NumberConfigs  < 1) 
                NumberConfigs = NCON()
				IF (SpecConfigNum > 0)
					IF (SpecConfigNum - NCON() +1 > 1)
						FOR  j, 2, SpecConfigNum - NCON() +1 , 1
							INSERTCONFIG j								# Zemax OS Configuration Insert 
						NEXT j
					ENDIF
					FORMAT 2 INT
					PRINT "Number of Configurations : ", NCON()
					
					CurrentMConfRow = CurrentMConfRow + 1  				# increment current row counter
					IF (CurrentMConfRow > 1) 
						INSERTMCO CurrentMConfRow 						# Zemax OS Insert row into MCE
					ENDIF
					
					MCE_Operand$ = " "
					subR$ = subSTR$ + "_MCE"
					GOTO subR$
					GOTO 401
				ELSE
					PRINT "Error: (ZOO operand mapping)."
					PRINT "NumberConfigs is less than 1."
					PRINT "Setting NumberConfigs to 1."
					NumberConfigs = 1									# default value
				ENDIF				
			ENDIF
			FORMAT 2 INT
			PRINT "Number of Configurations : ", NumberConfigs
            
            # setup array variables for MCE vignetting factors 
            IF ((NumFields > 0) & (NumberConfigs > 0))
                DECLARE VU_X_MCE, DOUBLE, 3, NumFields, NumberConfigs, 2
                DECLARE VL_X_MCE, DOUBLE, 3, NumFields, NumberConfigs, 2
                DECLARE VU_Y_MCE, DOUBLE, 3, NumFields, NumberConfigs, 2
                DECLARE VL_Y_MCE, DOUBLE, 3, NumFields, NumberConfigs, 2
                FOR  j, 1, NumFields , 1
                    FOR jj, 1, NumberConfigs, 1
                        VU_X_MCE(j, jj, 1) = 0.0
                        VL_X_MCE(j, jj, 1) = 0.0
                        VU_Y_MCE(j, jj, 1) = 0.0
                        VL_Y_MCE(j, jj, 1) = 0.0
                        
                        VU_X_MCE(j, jj, 2) = 0.0
                        VL_X_MCE(j, jj, 2) = 0.0
                        VU_Y_MCE(j, jj, 2) = 0.0
                        VL_Y_MCE(j, jj, 2) = 0.0
                    NEXT
                NEXT j
            ENDIF
            
			IF (NumberConfigs - NCON() +1 > 1)
				FOR  j, 2, NumberConfigs - NCON() +1 , 1
					INSERTCONFIG j										# Zemax OS Configuration Insert 
				NEXT j
			ENDIF
		ENDIF
		
		! Create a new row in the MultiConfig Editor
		! Get the operand value
		subD$ = " "
		IF (num_params > 2) 
			CurrentMConfRow = CurrentMConfRow + 1  						# increment current row counter
			IF (CurrentMConfRow > 1) 
				INSERTMCO CurrentMConfRow 								# Zemax OS Insert row into MCE
			ENDIF
			
			MCE_Operand$ = subSTR$
			subD$ = $GETSTRING(curLine$, 3)
			
			GOSUB CheckMCE_Operand
			IF (GoNextLine == 1) 										# Operand did not convert
				IF (CurrentMConfRow > 1) 
					DELETEMCO CurrentMConfRow 							# Zemax OS Delete row in MCE
					CurrentMConfRow = CurrentMConfRow - 1
				ENDIF
				GoNextLine = 0
				GOTO 401
			ENDIF		
		ENDIF
	GOTO 401
	
	LABEL !				# commented out line
		PRINT "Line is commented out"
	GOTO 411
	
	GoNextLine = 1
	GOTO 402
	LABEL 411
	FORMAT "%#04i" LIT
	PRINT curPos, "    Nothing to convert"
	
	LABEL 401
	GoNextLine = 0
	LABEL 402
RETURN

SUB CheckMCE_Operand
! Mapping of the Multi-Configuration Editor commands
	
	MCE_opName$ = MCE_Operand$ + "_MCE"
	GOTO MCE_opName$
	GOTO 711
	
	LABEL ADE_MCE		# 	Tilt about X axis
		PRINT "Tilt about X axis      : ", MCE_Operand$
		SETMCOPERAND CurrentMConfRow, 0, "PRAM", 0						# Zemax OS MCE command
		SETMCOPERAND CurrentMConfRow, 0, 3, 2							# Zemax OS MCE command
		GOSUB PopulateMCE
	GOTO 701
	
	LABEL BDE_MCE		#	Tilt about Y axis
		PRINT "Tilt about Y axis      : ", MCE_Operand$
		SETMCOPERAND CurrentMConfRow, 0, "PRAM", 0						# Zemax OS MCE command
		SETMCOPERAND CurrentMConfRow, 0, 4, 2							# Zemax OS MCE command
		GOSUB PopulateMCE
	GOTO 701
	
	LABEL CDE_MCE		#	Tilt about Z axis
		PRINT "Tilt about Z axis      : ", MCE_Operand$
		SETMCOPERAND CurrentMConfRow, 0, "PRAM", 0						# Zemax OS MCE command
		SETMCOPERAND CurrentMConfRow, 0, 5, 2							# Zemax OS MCE command
		GOSUB PopulateMCE
	GOTO 701
	
	LABEL CIR_MCE		#   Circular Aperture
		PRINT "Semi-Diameter operand : ", MCE_Operand$
		remove1stChar$ = $GETSTRING(curLine$, 3)
		IF (SLEN(remove1stChar$) > 1)
			remove1stChar$ = $RIGHTSTRING(remove1stChar$ , SLEN(remove1stChar$) -1)
		ENDIF
		
		SETMCOPERAND CurrentMConfRow, 0, "SDIA", 0						# Zemax OS MCE command
		IF (remove1stChar$ $!= "S")
			GOSUB PopulateMCE
		ELSE															# use the STO number:			
			FOR  j, 1, NumberConfigs , 1
				subGSTR$ = $GETSTRING(curLine$, j + 3)
				SETMCOPERAND CurrentMConfRow, j, SVAL(subGSTR$), 0
			NEXT j

			SETMCOPERAND CurrentMConfRow, 0, VEC2(StopSurfNum), 1		# Zemax OS MCE command
		ENDIF
	GOTO 701

	LABEL CUY_MCE		#   Curvature of Surface
		PRINT "Curvature of Surface  ", MCE_Operand$
		remove1stChar$ = $GETSTRING(curLine$, 3)
		IF (SLEN(remove1stChar$) > 1)
			remove1stChar$ = $RIGHTSTRING(remove1stChar$ , SLEN(remove1stChar$) -1)
		ENDIF
		
		SETMCOPERAND CurrentMConfRow, 0, "CRVT", 0						# Zemax OS MCE command
		GOSUB PopulateMCE
	GOTO 701

	LABEL EPD_MCE		#   Entrance pupil diameter
		PRINT "Entrance Pupil Diameter : ", MCE_Operand$
		SETMCOPERAND CurrentMConfRow, 0, "APER", 0						# Zemax OS MCE command
		FOR  j, 1, NumberConfigs , 1
			subGSTR$ = $GETSTRING(curLine$, j + 2)
			SETMCOPERAND CurrentMConfRow, j, SVAL(subGSTR$), 0			# Zemax OS MCE command
		NEXT j
	GOTO 701
	
	LABEL FNO_MCE		#	F/# System Aperture value
		PRINT "F/# System Aperture    : ", MCE_Operand$
		SETMCOPERAND CurrentMConfRow, 0, "APER", 0						# Zemax OS MCE command
		GOSUB PopulateMCE
	GOTO 701
	
	LABEL GLA_MCE		#   Glass	
	LABEL GL1_MCE
	LABEL GL2_MCE
	LABEL RMD_MCE
		PRINT "Glass operand         : ", MCE_Operand$
		
		remove1stChar$ = $GETSTRING(curLine$, 3)
		
		subXTR$ = $LEFTSTRING(remove1stChar$ , 1)
		IF ((subXTR$ $== "S") | (subXTR$ $== "s"))
			! check if there is data beyond surface speficier
			subXTR$ = $GETSTRING(curLine$, 4)
			IF (subXTR$ $== "") 
				DELETEMCO CurrentMConfRow 								# Zemax OS Delete row in MCE
				CurrentMConfRow = CurrentMConfRow - 1
				GOTO 401
			ENDIF
		ENDIF
		
		IF (SLEN(remove1stChar$) > 1)
			remove1stChar$ = $RIGHTSTRING(remove1stChar$ , SLEN(remove1stChar$) -1)
		ENDIF
		

		
		subXTR$ =  $GETSTRING(curLine$, 2)
		IF (subXTR$ $== "RMD")
			remove1stChar$ = $GETSTRING(curLine$, 3)
			IF (SLEN(remove1stChar$) > 1)
				subGSTR$ = $LEFTSTRING(remove1stChar$,1)
				remove1stChar$ = $RIGHTSTRING(remove1stChar$ , SLEN(remove1stChar$) -1)
			ENDIF
			subINT = SVAL(remove1stChar$)
			FORMAT 3 INT
			IF (SVAL(remove1stChar$) <= NumberSurf) 
				subSTR$ = "GLSS"
				GOSUB CheckForMCE_Operand
				IF (subINT == -1) 
					! there is no GLSS command for the particular surface
					GOTO 771
				ELSE
					DELETEMCO CurrentMConfRow		# delete current MCE operand
					CurrentMConfRow = CurrentMConfRow - 1
					ConfRow = k 		# MCE operand line from the SUB CheckForMCE_Operand
					GOSUB PopulateMCE_byString
				ENDIF
			ENDIF
			GOTO 701
		ENDIF
		
		LABEL 771
		SETMCOPERAND CurrentMConfRow, 0, "GLSS", 0						# Zemax OS MCE command
		ConfRow = CurrentMConfRow
		GOSUB PopulateMCE_byString
	GOTO 701
		
	LABEL REF_MCE		#	Primary wavelength number
	 	PRINT "Primary Wave Number    : ", MCE_Operand$
		SETMCOPERAND CurrentMConfRow, 0, "PRWV", 0						# Zemax OS MCE command
		GOSUB PopulateMCE
	GOTO 701
	
	LABEL THI_MCE		#   Thickness
	LABEL ZDE_MCE
		PRINT "Thickness              : ", MCE_Operand$
		SETMCOPERAND CurrentMConfRow, 0, "THIC", 0						# Zemax OS MCE command
		GOSUB PopulateMCE
	GOTO 701
	
	LABEL TIT_MCE		#	Configuration Title
		IF (ConfigSpecific)
			FORMAT 3 INT
			PRINT "Configuration          : ", CurConfigNum
			PRINT "Title                  : ", MCE_Operand$
			SETMCOPERAND CurrentMConfRow, CurConfigNum, MCE_Operand$, 0	# Zemax OS MCE command
		ELSE
			IF (MCE_Operand$ $== " ")
				SETMCOPERAND CurrentMConfRow, 0, "LTTL", 0
				dummy = SYPR(16)
				subR$ = $buffer()
				PRINT "Title :", subR$
				FOR  j, 1, NCON(), 1
					SETMCOPERAND CurrentMConfRow, j, subR$, 0			# Zemax OS Configuration Insert 
				NEXT j
			ELSE
				PRINT "Title                  : ", MCE_Operand$
				SETMCOPERAND CurrentMConfRow, 0, "LTTL", 0
				GOSUB PopulateMCE
			ENDIF
		ENDIF
	GOTO 701
	
	LABEL WL_MCE		#	Wavelength
		IF (ConfigSpecific)
			FORMAT 3 INT
			PRINT "Configuration          : ", CurConfigNum
			FORMAT 8.5
			subVal = SVAL(MCE_Operand$) /1000.0 
			PRINT "Wavelength             : ", subVal
			SETMCOPERAND CurrentMConfRow, CurConfigNum, subVal, 0 		#Zemax OS MCE command
		ELSE
			IF (MCE_Operand$ $== " ")
				SETMCOPERAND CurrentMConfRow, 0, "WAVE", 0
			ELSE
				PRINT "Wavelength             : ", MCE_Operand$
				SETMCOPERAND CurrentMConfRow, 0, "WAVE", 0
				GOSUB PopulateMCE
			ENDIF
		ENDIF		
	GOTO 701
	
	LABEL WTF_MCE		#	Wavelength Weight
		PRINT "Field Weight           : ", MCE_Operand$
		SETMCOPERAND CurrentMConfRow, 0, "FLWT", 0						# Zemax OS MCE command
		GOSUB PopulateMCE
	GOTO 701
	
	LABEL WTW_MCE		#	Wavelength Weight
		PRINT "Wavelength Weight      : ", MCE_Operand$
		SETMCOPERAND CurrentMConfRow, 0, "WLWT", 0						# Zemax OS MCE command
		GOSUB PopulateMCE
	GOTO 701
	
	LABEL XDE_MCE		#   X-decenter
		PRINT "X-decenter             : ", MCE_Operand$
		SETMCOPERAND CurrentMConfRow, 0, "PRAM", 0						# Zemax OS MCE command
		SETMCOPERAND CurrentMConfRow, 0, 1, 2							# Zemax OS MCE command
		GOSUB PopulateMCE
	GOTO 701

	LABEL XAN_MCE		#	X Angle (degree) in object space
	LABEL XIM_MCE		#	Field: X Paraxial Image Height
	LABEL XOB_MCE		#	Field: X Object Height
	LABEL XRI_MCE		#	Field: X Real Image Height
		PRINT "Field X value          : ", MCE_Operand$
		SETMCOPERAND CurrentMConfRow, 0, "XFIE", 0						# Zemax OS MCE command
		GOSUB PopulateMCE
	GOTO 701
		
	LABEL YDE_MCE		#   Y-decenter
		PRINT "Y-decenter             : ", MCE_Operand$
		SETMCOPERAND CurrentMConfRow, 0, "PRAM", 0						# Zemax OS MCE command
		SETMCOPERAND CurrentMConfRow, 0, 2, 2							# Zemax OS MCE command
		GOSUB PopulateMCE
	GOTO 701
	
	LABEL YAN_MCE		#	Y Angle (degree) in object space
	LABEL YOB_MCE		#	Field: Y Object Height
	LABEL YIM_MCE		#	Field: Y Paraxial Image Height
	LABEL YRI_MCE		#	Field: Y Real Image Height
		PRINT "Field Y value          : ", MCE_Operand$
		SETMCOPERAND CurrentMConfRow, 0, "YFIE", 0						# Zemax OS MCE command
		GOSUB PopulateMCE
	GOTO 701
	
	LABEL VUX_MCE		#	Fract. Ent. Pupil radius clipped off +X
		PRINT "Field X compression    : ", MCE_Operand$
		SETMCOPERAND CurrentMConfRow, 0, "FVCX", 0						# Zemax OS MCE command
		GOSUB PopulateMCE
	GOTO 701
	
	LABEL VUY_MCE		#	Fract. Ent. Pupil radius clipped off +Y
		PRINT "Field Y compression    : ", MCE_Operand$
		SETMCOPERAND CurrentMConfRow, 0, "FVCY", 0						# Zemax OS MCE command
		GOSUB PopulateMCE
	GOTO 701
    
    LABEL VLX_MCE
        PRINT "Field X decenter       : ", MCE_Operand$
        SETMCOPERAND CurrentMConfRow, 0, "FVDX", 0						# Zemax OS MCE command
        GOSUB PopulateMCE
    GOTO 701
    
    LABEL VLY_MCE
        PRINT "Field Y decenter       : ", MCE_Operand$
        SETMCOPERAND CurrentMConfRow, 0, "FVDY", 0						# Zemax OS MCE command
        GOSUB PopulateMCE
    GOTO 701
	
	LABEL 711
	PRINT "MCE operand was not converted."
	GoNextLine = 1
	GOTO 702
	
	LABEL 701
	GoNextLine = 0
	LABEL 702
RETURN

SUB CleanupGlassName
	! Conversion of CodeV glass into ZOS-compatible format
	! Uses subSTR$ from the ParseOperands subroutine

	! Check if glass name is in quatation marks:
	StrXTD$ = $LEFTSTRING(subSTR$,1)
	IF ((StrXTD$ $== "'") & (SLEN(subSTR$) > 2) ) 
			StrXTD$ = $RIGHTSTRING(subSTR$, SLEN(subSTR$) - 1)
			StrXTD$ = $LEFTSTRING(StrXTD$, SLEN(StrXTD$) - 1)
			subSTR$ = StrXTD$
	ENDIF

	subFlag = 0															# flag for MIL number glass
	subLen = 0
	subVal = 0.0
	LABEL 501
	subLen = subLen + 1
	curR$ = subSTR$
	subR$ = $LEFTSTRING(subSTR$, subLen)
	subD$ = $RIGHTSTRING(subR$, 1)
	IF (subD$ $== DOT$) 
		subFlag = subLen
	ENDIF
	IF (subR$ $!= subSTR$) THEN GOTO 501

	IF (subFlag > 0) & (subFlag < 5) & (SLEN(subSTR$) < 8)				# MIL number glass
		subR$ = $LEFTSTRING(subSTR$, 3)
		subVal = subLen - subFlag
		subD$ = $RIGHTSTRING(subSTR$, subVal)
		
		IF (subVal < 3)
			FOR j, subVal, 2, 1											# populate missing digits with zeros
				subD$ = subD$ + "0"
			NEXT j
		ENDIF
		
		IF (subVal > 3) THEN subD$ = $LEFTSTRING(subD$, 3)				# truncate code to three digits 

		subSTR$ = subR$+subD$
		GOTO 502
	ENDIF
	
	IF (subFlag > 4)													# Model glass
		subR$ = $LEFTSTRING(subSTR$, subFlag -1)
		subR$ = "1."+subR$
		subVal = subLen - subFlag
		subD$ = $RIGHTSTRING(subSTR$, subVal)
		subD$ = "."+subD$
		subNUM = SVAL(subD$) * 100.0
		
		SOLVETYPE CurSurfNum, GM, SVAL(subR$), subNUM, 0.0				# Model glass parameters
		subD$ = "Model"
		GOTO 502
	ENDIF
	
	! Glass Name Filters
	subGSTR$ = $RIGHTSTRING(subR$, 7)
	IF (subGSTR$ $== "_SCHOTT") 
	    subLen = subLen - 7
		subSTR$ = $LEFTSTRING(subSTR$, subLen)
	ENDIF
	
	subGSTR$ = $RIGHTSTRING(subR$, 8)
	IF (subGSTR$ $== "_SPECIAL") 
		subLen = subLen - 8
		subSTR$ = $LEFTSTRING(subSTR$, subLen)
	ENDIF
	
	subGSTR$ = $RIGHTSTRING(subR$, 6)
	IF (subGSTR$ $== "_OHARA") 
		subLen = subLen - 6
		subSTR$ = $LEFTSTRING(subSTR$, subLen)	
	ENDIF
	
	subGSTR$ = $RIGHTSTRING(subR$, 5)
	IF (subGSTR$ $== "_HOYA") 
		subLen = subLen - 5
		subSTR$ = $LEFTSTRING(subSTR$, subLen)
	ENDIF
		
	subGSTR$ = $RIGHTSTRING(subR$, 5)
	IF (subGSTR$ $== "_CDGM") 
		subLen = subLen - 5
		subSTR$ = $LEFTSTRING(subSTR$, subLen)
		
		! Additional refinement
		subD$ = $LEFTSTRING(subSTR$, 1)
		if (subD$ $== "H") | (subD$ $== "h")
			subD$ = $LEFTSTRING(subSTR$, 2)
			subD$ = $RIGHTSTRING(subD$, 1)
			if (subD$ $!= "-")
				subLen = subLen - 1
				subSTR$ = "H-" + $RIGHTSTRING(subSTR$, subLen)
			ENDIF
		ENDIF
	ENDIF
	
	LABEL 502
	PRINT "Warning! Glass name has been modified."
	curR$ = curR$ + " glass name modified"
	IF (!SurfaceSpecific)
		SURP CurSurfNum, COMM, curR$									# Zemax OS Surface Property
	ELSE
		SURP VEC2(SpecSurfNum), COMM, curR$								# Zemax OS Surface Property
	ENDIF
RETURN

SUB CommandIsTiltOrDecenter
	! Setup of the On/Off flag for decenter or tilt commands
	! Uses global variable StrXTD$
	IF (StrXTD$ $== "XDE") THEN GOTO TiltOrDecenter
	IF (StrXTD$ $== "YDE") THEN GOTO TiltOrDecenter
	IF (StrXTD$ $== "ZDE") THEN GOTO TiltOrDecenter
	IF (StrXTD$ $== "XDC") THEN GOTO TiltOrDecenter
	IF (StrXTD$ $== "YDC") THEN GOTO TiltOrDecenter
	IF (StrXTD$ $== "ZDC") THEN GOTO TiltOrDecenter
	IF (StrXTD$ $== "ADE") THEN GOTO TiltOrDecenter
	IF (StrXTD$ $== "BDE") THEN GOTO TiltOrDecenter
	IF (StrXTD$ $== "CDE") THEN GOTO TiltOrDecenter
	IF (StrXTD$ $== "ADC") THEN GOTO TiltOrDecenter
	IF (StrXTD$ $== "BDC") THEN GOTO TiltOrDecenter
	IF (StrXTD$ $== "CDC") THEN GOTO TiltOrDecenter
	
	AZDE_FLAG == 0														# Decenter / Tilt operand flag Off
	GOTO TD_EXIT
	
	LABEL TiltOrDecenter
	AZDE_FLAG == 1														# Decenter / Tilt operand flag On
	
	LABEL TD_EXIT
RETURN

SUB CommandIsZoomSpecifier
	! Check if a parsed line is a zoom specifier 
	! SubT$ is the string 
	! StrXTD$ is the first parameter 
	
	subS2$ = $GETSTRING(subT$, 2)
	subS3$ = $GETSTRING(subT$, 3)
	subS4$ = $GETSTRING(subT$, 4)
	
	IF (StrXTD$ $!= "ZOO") 
		IF (zoom_params > 1)
			FOR i, 1, zoom_params - 1, 2
				subD$ = $GETSTRING(ZoomStr$, i)
				subR$ = $GETSTRING(ZoomStr$, i + 1)
				
				IF ((StrXTD$ $== subD$) & (subS2$ $== subR$))
					! Update command
					subT$ = "ZOO " + subT$
				ENDIF
			NEXT i
		ENDIF
		GOTO END_CheckForZoomSpecifier
	ELSE
		IF ((SLEN(subS3$) > 0) & (SLEN(subS4$) == 0))
			ZoomStr$ = ZoomStr$ + " " + subS2$ + " " + subS3$
			zoom_params = zoom_params + 2
		ENDIF
	ENDIF
    
    
	
	LABEL END_CheckForZoomSpecifier
RETURN

SUB InsertLeadingDummySurface
	! Leading dummy surface insertion (COORDBRK)
	TargetSurfPosition = CurSurfNum - 1									# Point to the target surface position 
	
	IF (LeadDummySurfIn == 0)
		FORMAT 3 INT
		PRINT "Inserting dummy surface at position: ", CurSurfNum
		
		INSERT CurSurfNum												# Zemax OS inserts in front of a surface
		SURP CurSurfNum, TYPE, COORDBRK   								# Zemax OS Surface Property
		CurSurfNum = CurSurfNum + 1										# Increment the Current Surface
		LeadDummySurfIn = 1												# Switch on the flag
		
		! Re-mapping surfaces due to surface insertion
		IF (CurSurfNumStore > VEC2(NumberSurf-1)) THEN PRINT "Error: InsertSecondTrailingCB_Surface)"
		FOR i, CurSurfNumStore, NumberSurf - 1, 1 
			VEC2(i) = VEC2(i) + 1
		NEXT i
		PRINT
		PRINT "Surface Re-Mapping"
		PRINT "  (Code V -> ZOS):"
		PRINT "******************"
			PRINT "Surf ", CurSurfNumStore,
			PRINT "  -> ", VEC2(CurSurfNumStore)
		PRINT
	ELSE
		! Check that the dummy surface is a CoordinateBreak surface
		dummy = SPRO(TargetSurfPosition, 0)
		subSTR$ = $buffer()
		IF (subSTR$ $!= "COORDBRK")
			SURP TargetSurfPosition, TYPE, COORDBRK   					# Zemax OS Surface Property
		ENDIF
		
		PRINT "No surface insertion is needed."
		PRINT "Leading dummy surface already in place."
	ENDIF
RETURN

SUB InsertTrailingDummySurface
	! Trailing dummy surface insertion (COORDBRK)
	TargetSurfPosition = CurSurfNum + 1									# Point to the target surface position 
	
	IF (TrailDummySurfIn == 0)
		FORMAT 3 INT
		PRINT "Inserting dummy surface at position: ", TargetSurfPosition
		
		INSERT TargetSurfPosition
		SURP TargetSurfPosition, TYPE, COORDBRK   						# Zemax OS Surface Property
		TrailDummySurfIn = 1											# Switch on the flag
		
		! Transfer thickness to the dummy surface (unless it is z-shift)
		IF (abso(ZDEstore) < epsilon)
			subINT = SPRO(CurSurfNum, 3)
			IF (abso(subINT) > epsilon)		
				SURP CurSurfNum, THIC, 0.0								# Zemax OS Surface Property
				SURP TargetSurfPosition, THIC, subINT					# Zemax OS Surface Property
			ENDIF
		ENDIF
		
		! Re-mapping surfaces due to surface insertion
		IF (CurSurfNumStore + 1 > VEC2(NumberSurf - 1)) THEN PRINT "Error: InsertSecondTrailingCB_Surface"
		FOR i, CurSurfNumStore + 1, NumberSurf - 1, 1 
			VEC2(i) = VEC2(i) + 1
		NEXT i
	ELSE
		IF (TrailDummySurfIn == 1)
			! Check that the dummy surface is a CoordinateBreak surface
			dummy = SPRO(TargetSurfPosition, 0)
			subSTR$ = $buffer()
			IF (subSTR$ $!= "COORDBRK")
				SURP TargetSurfPosition, TYPE, COORDBRK   				# Zemax OS Surface Property
			ENDIF
		ENDIF
	
		PRINT "No surface insertion is needed."
		PRINT "Trailing dummy surface already in place."
	ENDIF
RETURN

SUB PopulateMCE
	! Setting up operand values for all configurations in MCE
	MCE_SignValue = 1
	subINT = 0	
	
	! Check in the sign needs to be reversed:
	subGSTR$ = $GETSTRING(curLine$, 2)
	IF (subGSTR$ $== "ADE") THEN MCE_SignValue = -1
	IF (subGSTR$ $== "BDE") THEN MCE_SignValue = -1
	IF (subGSTR$ $== "REF") THEN subINT = -1	# Different starting position
	
	
	! Do I need to insert a surface?
	subINT = 0															# reset variable
	subNUM = 0															# reset variable
	subXTR$ = $GETSTRING(curLine$, 2)
	
	IF (subXTR$ $== "XDE")
		subINT = 1														# parameter 1
		subXTR$ = "Dummy Surface"
	ENDIF
	IF (subXTR$ $== "YDE") 
		subINT = 2														# parameter 2
		subXTR$ = "Dummy Surface"
	ENDIF
	IF (subXTR$ $== "THI")
		subNUM = 2
	ENDIF
	IF (subXTR$ $== "ZDE") 
		subINT = -1														# thickness
		subNUM = 2
		subXTR$ = "Dummy Surface"
	ENDIF
	IF (subXTR$ $== "ADE") 
		subINT = 3														# parameter 3
		subXTR$ = "Dummy Surface"
	ENDIF
	IF (subXTR$ $== "BDE") 
		subINT = 4														# parameter 4
		subXTR$ = "Dummy Surface"
	ENDIF
	IF (subXTR$ $== "CDE") 
		subINT = 5														# parameter 5
		subXTR$ = "Dummy Surface"
	ENDIF
	
	remove1stChar$ = $GETSTRING(curLine$, 3)
    subGSTR$ = $LEFTSTRING(remove1stChar$,1)
	
	! Only map surface positions 
	IF (subGSTR$ $== "W") 
		subGSTR$ = "DO NOT MAP SURFACE ID"
		subINT = NumWaves
	ENDIF
	
	IF (subGSTR$ $== "F") 
		subGSTR$ = "DO NOT MAP SURFACE ID"
		subINT = -NumFields
	ENDIF
	
	IF (SLEN(remove1stChar$) > 1)
		remove1stChar$ = $RIGHTSTRING(remove1stChar$ , SLEN(remove1stChar$) -1)
	ENDIF
	subVal = SVAL(remove1stChar$)
	
	IF (subGSTR$ $== "DO NOT MAP SURFACE ID")
		IF (abso(subINT) < subVal)
			IF subINT > 0 
				PRINT "Wavelength number is larger than the maximum."
			ELSE
				PRINT "Field number is larger than the maximum."
			ENDIF
			DELETEMCO CurrentMConfRow		# delete current MCE operand
			CurrentMConfRow = CurrentMConfRow - 1
			GOTO 711
		ENDIF
	ENDIF
	
	! Check if the actual surface type is COORDBRK 
	TargetSurfPosition = VEC2(subVal)									# default target surface value
	
	IF (subXTR$ $== "Dummy Surface")
		! Check if the previous surface is already a CB
		j = VEC2(subVal) - 1
		dummy = SPRO(j, 0)
		subXTR$ = $buffer()

		IF (subXTR$ $!= "COORDBRK")
			j = VEC2(subVal)		# use Zemax OS surface positions
			! Insert a dummy surface:
			FORMAT 3 INT
			PRINT "MCE: Inserting dummy surface at position: ", j
			INSERT j
			SURP j, TYPE, COORDBRK   									# Zemax OS Surface Property
			
			! This is the inserted surface, no need to map
			subGSTR$ = "DO NOT MAP SURFACE ID"
			
			! Re-map surface positions beyond the insertion place
			IF (j <= NumberSurf-1) 
				FOR i, subVal , NumberSurf - 1, 1 
					VEC2(i) = VEC2(i) + 1
					PRINT
					PRINT "Surface Re-Mapping"
					PRINT "  (Code V -> ZOS):"
					PRINT "******************"
					PRINT "Surf ", i,
					PRINT "  -> ", VEC2(i)
				NEXT i
				PRINT					
			ENDIF
            subVal = j                                               # remaps subVal to actual COORDBRK
		ELSE 
			IF (subINT == -1) 
				! Thickness for the Z-shift
				subNUM = 2
				TargetSurfPosition = j
				GOTO MCE_dummy_surface_exit
			ENDIF
			
			! Check for a pick-up solve attached
			! If yes, assume DAR or BEN, use the leading coodinate break surface for MCE
			subNUM = SOLV(j, 100 + subINT, 0)
	
			IF (subNUM == 2)											# there is a pick-up solve
				! Get the target surface:
				TargetSurfPosition = SOLV(j, 100 + subINT, 1)
			ENDIF
		ENDIF
	ENDIF
		
	LABEL MCE_dummy_surface_exit
		
	IF (subGSTR$ $!= "DO NOT MAP SURFACE ID")
		IF (subNUM != 2)												# there is no pick-up solve
			SETMCOPERAND CurrentMConfRow, 0, j, 1						# Zemax OS MCE command
		ELSE 
			SETMCOPERAND CurrentMConfRow, 0, TargetSurfPosition, 1		# Zemax OS MCE command
		ENDIF
    ELSE
        # stores vignetting factors in array for post-calculations of MCE data
        IF ((subXTR$ $== "VUX") | (subXTR$ $== "VUY") | (subXTR$ $== "VLX") | (subXTR$ $== "VLY"))
            FOR i = 4, NumberConfigs + 3, 1
                val$ = $GETSTRING(curLine$, i)
                val = SVAL(val$)
                temp = INTE(subVal)
                IF subXTR$ $== "VUX"
                    VU_X_MCE(temp, i - 3, 1) = CurrentMConfRow
                    VU_X_MCE(temp, i - 3, 2) = val
                ENDIF
                IF subXTR$ $== "VUY"
                    VU_Y_MCE(temp, i - 3, 1) = CurrentMConfRow
                    VU_Y_MCE(temp, i - 3, 2) = val
                ENDIF
                IF subXTR$ $== "VLX"
                    VL_X_MCE(temp, i - 3, 1) = CurrentMConfRow
                    VL_X_MCE(temp, i - 3, 2) = val
                ENDIF
                IF subXTR$ $== "VLY"
                    VL_Y_MCE(temp, i - 3, 1) = CurrentMConfRow
                    VL_Y_MCE(temp, i - 3, 2) = val
                ENDIF
            NEXT
        ENDIF
        SETMCOPERAND CurrentMConfRow, 0, subVal, 1						# Zemax OS MCE command
	ENDIF
    
    ! add ZOO command to MCE (7993)
    subINT = 2
    StrXTD$ = $GETSTRING(curLine$, 3)
    StrXTD$ = $LEFTSTRING(StrXTD$, 1)
    GOSUB UpperStrXTD
    
    IF ((StrXTD$ $== "S") || (StrXTD$ $== "W")) THEN subINT = 3
    FOR  j, 1, NumberConfigs , 1
		subGSTR$ = $GETSTRING(curLine$, j + subINT)
		subVal = SVAL(subGSTR$)
		IF (abso(subVal) > epsilon) THEN subVal = subVal*MCE_SignValue
		SETMCOPERAND CurrentMConfRow, j, subVal, 0						# Zemax OS MCE command
	NEXT j
RETURN

SUB PopulateMCE_byString
	! Setting up string operand values for all configs in MCE

	FOR  j, 1, NumberConfigs , 1
		subGSTR$ = $GETSTRING(curLine$, j + 3)
		subXTR$ = $LEFTSTRING(subGSTR$, 1)
		IF (subXTR$ $== "'")
			subXTR$ = $RIGHTSTRING(subGSTR$ , SLEN(subGSTR$) -1)
			subGSTR$ = $LEFTSTRING(subXTR$ , SLEN(subXTR$) -1)
		ENDIF
		IF (subGSTR$ $== "REFL") THEN subGSTR$ = "MIRROR"
		IF (subGSTR$ $== "REFR") THEN subGSTR$ = "AIR"	
		IF (subGSTR$ $!= "AIR") THEN SETMCOPERAND ConfRow, j, subGSTR$, 0		# Zemax OS MCE command
	NEXT j
	
	remove1stChar$ = $GETSTRING(curLine$, 3)
	IF (SLEN(remove1stChar$) > 1)
		subGSTR$ = $LEFTSTRING(remove1stChar$,1)
		remove1stChar$ = $RIGHTSTRING(remove1stChar$ , SLEN(remove1stChar$) -1)
	ENDIF
	
	subINT = SVAL(remove1stChar$)
	IF (subGSTR$ $== "S")
		SETMCOPERAND ConfRow, 0, VEC2(subINT), 1						# Zemax OS MCE command
	ELSE
		SETMCOPERAND ConfRow, 0, subINT, 1								# Zemax OS MCE command
	ENDIF
RETURN


SUB CheckForThicknessTransfer
	! Post-processing check for thickness transfer conditions	
	! Special Condition flag:
	Num_Surf_Conds = 0
	
	! Move Z-Shift (ZDE) to a dedicated dummy surface
	NumSurf =  2 * NSUR()
	
	FOR i, 1, NumSurf, 1
        dummy = SPRO(i, 1)
		subSTR$ = $buffer()
        
		IF (subSTR$ $== "Z-shift") 
            ! Check if the surface is a Coordinate Break
			dummy = SPRO(i, 0)
			subSTR$ = $buffer()
			IF (subSTR$ $== "COORDBRK")
				! Check whether there are any other non-zero D/T parameters
				subVal = 0.0
				FOR j, 1, 5, 1
					subVal = subVal + abso(SPRX(i, 10, j))
				NEXT j
				
				IF (subVal > epsilon)
					FORMAT 3 INT
					PRINT "Inserting dummy surface at position: ", i
					INSERT i
					
					! Check MCE operands associated with former Z-shift surface 
					subSTR$ = "THIC"
					subINT = -1 * (i + 1)
					GOSUB CheckForMCE_Operand
					if (subINT != -1) 
						FORMAT 3 INT
						PRINT "Redirect THIC operand at MCE position ", k,
						PRINT " to surface ", i
						SETMCOPERAND k, 0, i, 1			# Zemax OS MCE command
					ENDIF
					
					PRINT "Transferring thickness value from surf ",i+1,
					PRINT " to surf ", i,
					PRINT "."
					
					! Move Z-shift to a dedicated dummy surface
					subVal = SPRO(i+1, 3)
					SURP i+1, THIC, 0.0
					SURP i, THIC, subVal
					
					! Move Z-shift comment tag:				
					SURP i , COMM, "Z-shift"							# Zemax OS Surface Property 
					SURP i+1, COMM, ""									# Zemax OS Surface Property 
					
					
					! Skip rays and do not draw this dummy surface:
					SURP i, 91, 1										# Zemax OS Surface Property 
					SURP i, 92, 1										# Zemax OS Surface Property 
					
					! Remap all the Code V surfaces beyond the insertion point
					FOR j, 1, NumberSurf - 1, 1
						IF (VEC2(j) >= i) THEN VEC2(j) = VEC2(j) + 1
					NEXT j
					
					! Increment the i and NumSurf
					i = i + 1
				ELSE
					! simply change surface type to STANDARD
					SURP i, TYPE, STANDARD   							# Zemax OS Surface Property
				ENDIF
                
                # temp value for Z-Shift inside a lens
                temp = 2
            ELSE
                temp = 1
			ENDIF
            
            # checks if ZDE Z-Shift is inside a lens
            IF (i - temp >= 0)
                curLine$ = $GLASS(i - temp)
                IF ((SLEN(curLine$) > 0) & (curLine$ $!= "MIRROR"))
                    SURP i - (temp - 1), GLAS, curLine$
                ENDIF
            ENDIF
		ENDIF
		IF (i > NSUR()) THEN i = NumSurf	
	NEXT i
				
	! Do not go to the IMA surf 
	! Start from the surface 3 (we are checking two previous surfaces)
	IF (NSUR() > 3)
		FOR i, 3, NSUR() -1 , 1
			Num_Surf_Conds = 0		# reset the number of spec. conds.
			
			! Coordinate Break Surface Condition Check:	
			! Check if the preceding surface is a CB (CB Surface Condition)
			dummy = SPRO(i -1, 0)
			subSTR$ = $buffer()
			IF (subSTR$ $== "COORDBRK") 
				Num_Surf_Conds = Num_Surf_Conds + 1
				! Zero CB Thickness Condition
				! Check for Zero Thickness Condition:
				subVal = SPRO(i -1, 3)
				IF (abso(subVal) < epsilon) 
					Num_Surf_Conds = Num_Surf_Conds + 1
				ENDIF
			ELSE
				GOTO SKIP_THK_TRANSFER
			ENDIF
		
			! Mirror Surface Condition Check
			! Check if the preceding previous surface is a MIRROR:
			dummy = SPRO(i -2, 4)
			subSTR$ = $buffer()
			IF (subSTR$ $== "MIRROR") 
				Num_Surf_Conds = Num_Surf_Conds + 1
				! Zero Mirror Thickness Condition
				! Check for Mirror Zero Thickness Condition:
				subVal = SPRO(i -2, 3)
				IF (abso(subVal) < epsilon) 
					Num_Surf_Conds = Num_Surf_Conds + 1
				ENDIF	
			ELSE
				GOTO SKIP_THK_TRANSFER
			ENDIF
		
			! Check if the current surface is:
			! 	a) not a CB surface
			!	b) not a "Z-shift", (ZDE Code V command)
			! 	c) not a mirror 
			! 	d) has non-zero thickness to transfer
			
			dummy = SPRO(i, 0)
			subSTR$ = $buffer()
			IF (subSTR$ $== "COORDBRK") THEN GOTO SKIP_THK_TRANSFER
				
			dummy = SPRO(i, 1)
			subSTR$ = $buffer()
			IF (subSTR$ $== "Z-shift") 
				! Skip rays and do not draw this dummy surface:
				SURP i, 91, 1												# Zemax OS Surface Property 
				SURP i, 92, 1												# Zemax OS Surface Property 
				GOTO SKIP_THK_TRANSFER
			ENDIF
			dummy = SPRO(i, 4)
			subSTR$ = $buffer()
			IF (subSTR$ $== "MIRROR") THEN GOTO SKIP_THK_TRANSFER

			subVal = SPRO(i, 3)
			IF (abso(subVal) > epsilon) 
				Num_Surf_Conds = Num_Surf_Conds + 1
			ENDIF
		
			! Proceed with transfer if number of conditions is 5
			IF (Num_Surf_Conds == 5)
				IF (i < 10) THEN FORMAT 1 INT
				IF ((i > 9) & (i < 100)) THEN FORMAT 2 INT
				IF (i > 100) THEN FORMAT 3 INT

				PRINT "Transferring thickness value from surf ",i,
				PRINT " to surf ", (i -1),
				PRINT "."
				SURP i, THIC, 0.0											# Zemax OS Surface Property
				SURP i -1, THIC, subVal										# Zemax OS Surface Property
			ENDIF
		
			LABEL SKIP_THK_TRANSFER
		NEXT i
	ENDIF
	PRINT
RETURN

SUB CheckForMCE_Operand
	! Check if MCE contains operand stored in subSTR$ at surface stored in subINT
	! if input subINT is negative than search by ZOS surface number
	! for positive input subINT seach by CodeV surface number
	! if no operands found subINT returns -1
	! k returns the MCE operand line 
	
	NumberMCE_Operands = MCON(0,0,0)
	
	FOR k, 1, NumberMCE_Operands, 1
		dummy = MCON(k, 0, 0)
		subD$ = $buffer()
		IF (subD$ $== subSTR$)
			dummy = MCON(k, 0, 1)
			if ((subINT > 0) & (dummy == VEC2(subINT)))
				GOTO END_CheckForMCE_Operand
			ENDIF
			if ((subINT < 0) & (dummy == abso(subINT)))
				GOTO END_CheckForMCE_Operand
			ENDIF
		ENDIF	
	NEXT k
	subINT = -1
	LABEL END_CheckForMCE_Operand
RETURN

SUB TrimString
	! Trimming of leading and trailing blanks
	! Use variable: subNumTrim
	!				subStrLength
	! input parameter: StringToTrim$
	subNumTrim = 0
	subStrLength = SLEN(StringToTrim$)
	IF (subStrLength > MAX_SLEN) THEN GOTO 301
	
	! remove trailing blanks and semicolons
	LABEL 302
	IF (subStrLength > 0) 
		subSTR$ = $RIGHTSTRING(StringToTrim$, 1)						
		IF (subSTR$ $== BLANK$) | (subSTR$ $== SEMICOLON$)
			subStrLength = subStrLength - 1				
			StringToTrim$ = $LEFTSTRING(StringToTrim$, subStrLength)	
			GOTO 302					
		ENDIF
	ENDIF

	! Remove leading blanks
	LABEL 303
	IF (subStrLength > 0) 
		subSTR$ = $LEFTSTRING(StringToTrim$, 1)	
		IF (subSTR$ $== BLANK$)	
			subStrLength = subStrLength - 1
			StringToTrim$ = $RIGHTSTRING(StringToTrim$, subStrLength)
			GOTO 303
		ENDIF	
	ENDIF
	LABEL 301
RETURN

SUB UpperStrXTD
! Replace StrXTD$ with its uppercase equivalent 
! Uses variables subGSTR$, subR$, subSTR$
	
	char_counter = 0
	subSTR$ = ""
	IF (SLEN(StrXTD$) < 1) THEN RETURN
	
	FOR  char_counter, 1, SLEN(StrXTD$) , 1
		subR$ = $LEFTSTRING(StrXTD$, char_counter)
		subGSTR$ = $RIGHTSTRING(subR$, 1)
		subR$ = subGSTR$ + "_UPPER"

		GOTO subR$
			GOTO END_UPPERCASE_CYCLE
		
		LABEL a_UPPER
			subGSTR$ = "A"
			GOTO END_UPPERCASE_CYCLE
		LABEL b_UPPER
			subGSTR$ = "B"
			GOTO END_UPPERCASE_CYCLE			
		LABEL c_UPPER
			subGSTR$ = "C"
			GOTO END_UPPERCASE_CYCLE			
		LABEL d_UPPER
			subGSTR$ = "D"
			GOTO END_UPPERCASE_CYCLE
		LABEL e_UPPER
			subGSTR$ = "E"
			GOTO END_UPPERCASE_CYCLE
		LABEL f_UPPER
			subGSTR$ = "F"
			GOTO END_UPPERCASE_CYCLE
		LABEL g_UPPER
			subGSTR$ = "G"
			GOTO END_UPPERCASE_CYCLE
		LABEL h_UPPER
			subGSTR$ = "H"
			GOTO END_UPPERCASE_CYCLE
		LABEL i_UPPER
			subGSTR$ = "I"
			GOTO END_UPPERCASE_CYCLE
		LABEL j_UPPER
			subGSTR$ = "J"
			GOTO END_UPPERCASE_CYCLE
		LABEL k_UPPER
			subGSTR$ = "K"
			GOTO END_UPPERCASE_CYCLE
		LABEL l_UPPER
			subGSTR$ = "L"
			GOTO END_UPPERCASE_CYCLE
		LABEL m_UPPER
			subGSTR$ = "M"
			GOTO END_UPPERCASE_CYCLE
		LABEL n_UPPER
			subGSTR$ = "N"
			GOTO END_UPPERCASE_CYCLE
		LABEL o_UPPER
			subGSTR$ = "O"
			GOTO END_UPPERCASE_CYCLE
		LABEL p_UPPER
			subGSTR$ = "P"
			GOTO END_UPPERCASE_CYCLE
		LABEL q_UPPER
			subGSTR$ = "Q"
			GOTO END_UPPERCASE_CYCLE
		LABEL r_UPPER
			subGSTR$ = "R"
			GOTO END_UPPERCASE_CYCLE
		LABEL s_UPPER
			subGSTR$ = "S"
			GOTO END_UPPERCASE_CYCLE
		LABEL t_UPPER
			subGSTR$ = "T"
			GOTO END_UPPERCASE_CYCLE
		LABEL u_UPPER
			subGSTR$ = "U"
			GOTO END_UPPERCASE_CYCLE
		LABEL v_UPPER
			subGSTR$ = "V"
			GOTO END_UPPERCASE_CYCLE
		LABEL w_UPPER
			subGSTR$ = "W"
			GOTO END_UPPERCASE_CYCLE
		LABEL x_UPPER
			subGSTR$ = "X"
			GOTO END_UPPERCASE_CYCLE
		LABEL y_UPPER
			subGSTR$ = "Y"
			GOTO END_UPPERCASE_CYCLE
		LABEL z_UPPER
			subGSTR$ = "Z"
						
		LABEL END_UPPERCASE_CYCLE
	
		subSTR$ = subSTR$ + subGSTR$
	NEXT char_counter
		
		StrXTD$ = subSTR$
RETURN

SUB VerifyGlass    
	! Post-processing of glass names for compatibility
	DECLARE DoNotCheck, INT, 1, NSUR() + 1 
	FORMAT 6 INT
	
	# ! Get the names of loaded catalogs: 
	# subD$ = $GLASSCATALOG(0)
    
	# FOR i, 1, NSUR(), 1
    FOR i, 0, NSUR(), 1         # changed from 1->0 to include Object Surface index
        
        
		DoNotCheck(i + 1) = 0
		curLine$ = $GLASS(i)
		subVal = GNUM(curLine$)
	
		IF (curLine$ $== "") THEN GOTO SKIP_GLASS_MODIFY
		
		!First check if the unknown name belongs to some other glass catalog
		IF (subVal <= 0) & (curLine$ $!= "") & (curLine$ $!= "MIRROR") 
			!Check if this is a table glass
			num_params = 0
			curLine$ = ZTG_STACK$
			GOSUB GetNumParamsInline
			
            curLine$ = $GLASS(i)
            VerifyGlassType = 0
            GOSUB VerifyGlass_ZTG
            IF boolVerifyGlass == 1 THEN GOTO SKIP_GLASS_MODIFY
            
			!Check if this is a PRV catalog glass
			num_params = 0
			curLine$ = PRV_STACK$
			GOSUB GetNumParamsInline
			
            curLine$ = $GLASS(i)
            VerifyGlassType = 0
            GOSUB VerifyGlass_PRV
            IF boolVerifyGlass == 1 THEN GOTO SKIP_GLASS_MODIFY

			curLine$ = $GLASS(i)
			subVal = GNUM(curLine$)     # is this needed?
		ENDIF
	
        curLine$ = $GLASS(i)
    
		! adding "N-" in case the glass is not recognized
		GOSUB VerifyGlass_N
        IF boolVerifyGlass == 1 THEN SURP i, GLAS, subR$
        
        ! adding "S-" in case the glass is not recognized
		GOSUB VerifyGlass_S
        IF boolVerifyGlass == -1 THEN GOTO SKIP_GLASS_MODIFY
        IF boolVerifyGlass == 1 THEN SURP i, GLAS, subR$				# Zemax OS Surface Property
				
		! adding "L-" in case the glass is not recognized
		GOSUB VerifyGlass_L
        IF boolVerifyGlass == 1 THEN SURP i, GLAS, subR$                # Zemax OS Surface Property
        
		
		! Issue a warning if the glass name was modified
		curR$ = $GLASS(i)
		IF (curLine$ $!= curR$) 
			PRINT
		    FORMAT 14 INT
			PRINT "Surface          : ", i
			PRINT "Glass            :      ", curR$
			PRINT "Warning! Glass name has been modified."
			curR$ = curLine$ + " glass name modified"
			SURP i, COMM, curR$											# Zemax OS Surface Property
		ENDIF
		
		LABEL SKIP_GLASS_MODIFY
	NEXT

	! Clean up Comment field for the PRV catalog glasses:
	num_params = 0
	curLine$ = PRV_STACK$
	GOSUB GetNumParamsInline
	IF (num_params > 0)
		FOR i, 1, NSUR(), 1
			curLine$ = $GLASS(i)
			subVal = GNUM(curLine$)
			IF (subVal > 0) 
				FOR j, 1, num_params, 1
					StrXTD$ = $GETSTRING(PRV_STACK$, j)
					GOSUB UpperStrXTD		# convert string to uppercase
					IF (StrXTD$ $== curLine$) 
						! Read Comment field:
						dummy = SPRO(i, 1)
						subD$ = $buffer()

						! Clean it up:
						subR$ = $GETSTRING(subD$, 2)
						subR$ = subR$ + " " + $GETSTRING(subD$, 3)
						subR$ = subR$ + " " + $GETSTRING(subD$, 4)
						IF (subR$ $== "glass name modified")
							SURP i, COMM, " "							# Zemax OS Surface Property
						ENDIF
					ENDIF
				NEXT j
			ENDIF
		NEXT i
	ENDIF 
	
	! Update surface properties
	UPDATE
	
	! Replacing unknown glass with a model
	FOR i, 0, NSUR(), 1
		IF (DoNotCheck(i + 1)) THEN GOTO SKIP_GLASS_VERIFY
		curLine$ = $GLASS(i)
		subVal = SVAL(curLine$)
		if ((subVal > 1000) | (curLine$ $== "")) THEN GOTO SKIP_GLASS_VERIFY
		
		subVal = GNUM(curLine$)
		IF (subVal <= 0) & (curLine$ $!= "") & (curLine$ $!= "MIRROR")	# Check if not AIR or Mirror	
			FORMAT 14 INT
			PRINT "Surface          : ", i
			PRINT "Glass            :     ", curLine$
			PRINT "Warning! Unknown glass has been replaced by Model glass."
			curR$ = curLine$ + " glass unknown: use Model"
			SURP i, COMM, curR$											# Zemax OS Surface Property
			SOLVETYPE i, GM, 1.5, 0.0, 0.0								# replacing unknown glass with Model glass
		ENDIF
		
		LABEL SKIP_GLASS_VERIFY
	NEXT i
	
    # checks MCE for "GLSS" operand; need to determine if ".ZTG" is required
    IF NCON() > 1
        FOR i, 1, MCON(0, 0, 0), 1  # row
            IF MCON(i, 0, 0) == 41
                # operand equals GLSS; loop through configs
                FOR j, 2, MCON(0, 0, 1), 1  # config
                    # gets glass name in MCE
                    
                    temp = MCON(i, j, 0)
                    StrXTD$ = $BUFFER()
                    
                    IF (StrXTD$ $== "") THEN GOTO SKIP_MCE_VERIFY_GLASS
                    
                    GOSUB UpperStrXTD
                    currentGlassName$ = StrXTD$
                    
                    # checks for ZTG table glass
                    num_params = 0
                        curLine$ = ZTG_STACK$
                        GOSUB GetNumParamsInline
                    
                    VerifyGlassType = 1
                    curLine$ = currentGlassName$
                    GOSUB VerifyGlass_ZTG
                    IF boolVerifyGlass == 1 THEN GOTO SKIP_MCE_VERIFY_GLASS
                    
                    
                    # checks if glass in PRV
                    num_params = 0
                        curLine$ = PRV_STACK$
                        GOSUB GetNumParamsInline
                    
                    VerifyGlassType = 1
                    curLine$ = currentGlassName$
                    GOSUB VerifyGlass_PRV  
                    IF boolVerifyGlass == 1 THEN GOTO SKIP_MCE_VERIFY_GLASS
                    
                    # common renaming
                    curLine$ = currentGlassName$
                    subVal = GNUM(curLine$)
                    
                    GOSUB VerifyGlass_N
                    IF boolVerifyGlass == 1 THEN SETMCOPERAND i, j, subR$, 0
                    GOSUB VerifyGlass_L
                    IF boolVerifyGlass == 1 THEN SETMCOPERAND i, j, subR$, 0
                    GOSUB VerifyGlass_S
                    IF boolVerifyGlass == 1 THEN SETMCOPERAND i, j, subR$, 0
                    
                    LABEL SKIP_MCE_VERIFY_GLASS
                NEXT j
            ENDIF
        NEXT i
    ENDIF
    
	RELEASE DoNotCheck    
    PRINT
RETURN

SUB VerifyGlass_ZTG
    boolVerifyGlass = 0
    
    num_params = num_params / 2
    IF (num_params > 0)
        
    
        FOR k, 1, num_params, 1
            ZTG_INDEX = 2 * k - 1
            StrXTD$ = $GETSTRING(ZTG_STACK$, ZTG_INDEX)
            
            GOSUB UpperStrXTD		# convert string to uppercase
            # remove invalid characters from ZTG name
            subStr$ = curLine$
            GOSUB RemoveInvalidCharacters
            curLine$ = str$
            
            
            IF (StrXTD$ $== curLine$) 
                
                
                StrXTD$ = StrXTD$ + ".ZTG"
                
                boolVerifyGlass = 1
                
                IF VerifyGlassType == 0
                    PRINT "Table glass match is found ", StrXTD$
                    SURP i, GLAS, StrXTD$
                    DoNotCheck(i + 1) = 1
                    GOTO END_VerifyGlass_ZTG                            # skip further glass checks for this surface
                ENDIF
                
                IF VerifyGlassType == 1
                    PRINT "Table glass match is found in MCE ", StrXTD$
                    SETMCOPERAND i, j, StrXTD$, 0
                    GOTO END_VerifyGlass_ZTG                            # skip further glass checks for this MCE cell
                ENDIF
                
                # UPDATE                # not needed (right here).  Think this is what prompts message box
                # curLine$ = $GLASS(i)  # not needed
                
            ENDIF 
        NEXT k
    ENDIF
    LABEL END_VerifyGlass_ZTG
RETURN 

SUB VerifyGlass_PRV
    ! Get the names of loaded catalogs: 
	subD$ = $GLASSCATALOG(0)

    boolVerifyGlass = 0
    IF (num_params > 0)
        FOR k, 1, num_params, 1
            StrXTD$ = $GETSTRING(PRV_STACK$, k)
            GOSUB UpperStrXTD		# convert string to uppercase
            
            IF (StrXTD$ $== curLine$) 
                PRINT
                PRINT "Glass match from ", PRV_GLASS_CATALOG$, " catalog is found."
                subD$ = subD$ + " " + PRV_GLASS_CATALOG$	
                SYSP 23, subD$
                LOADCATALOG
                
                boolVerifyGlass = 1
                IF VerifyGlassType == 0
                    DoNotCheck(i + 1) = 1		# skip further glass checks for this surface
                    #GOTO SKIP_GLASS_MODIFY
                    GOTO END_VerifyGlass_PRV
                ENDIF
                
                #IF VerifyGlassType == 1 THEN GOTO MCE_SKIP_VERIFY_GLASS
                IF VerifyGlassType == 1 THEN GOTO END_VerifyGlass_PRV
            ENDIF
        NEXT k
    ENDIF
    LABEL END_VerifyGlass_PRV
RETURN 

SUB VerifyGlass_N
    boolVerifyGlass = 0
    subR$ = $LEFTSTRING(curLine$, 1)
    IF (subR$ $== "N") & (subVal <= 0)
        subR$ = $LEFTSTRING(curLine$, 2)
        IF (subR$ $!= "N-")
            GOSUB GetNumParamsInline
                subR$ = $RIGHTSTRING(curLine$,  SLEN(curLine$) -1)
                subR$ = "N-" + subR$
                boolVerifyGlass = 1
        ENDIF
    ENDIF
RETURN 

SUB VerifyGlass_S
    boolVerifyGlass = 0
    subR$ = $LEFTSTRING(curLine$, 1)
    !Exceptions:
    IF (curLine$ $== "SILICA") 
        boolVerifyGlass = -1
        GOTO SKIP_MODIFY_S
    ENDIF
    
    IF (subR$ $== "S") & (subVal <= 0)
        subR$ = $LEFTSTRING(curLine$, 2)
        IF (subR$ $!= "S-")
            GOSUB GetNumParamsInline
                subR$ = $RIGHTSTRING(curLine$,  SLEN(curLine$) -1)
                subR$ = "S-" + subR$
                boolVerifyGlass = 1
        ENDIF
    ENDIF
    LABEL SKIP_MODIFY_S
RETURN 

SUB VerifyGlass_L
    boolVerifyGlass = 0
    subR$ = $LEFTSTRING(curLine$, 1)
    IF (subR$ $== "L") & (subVal <= 0)
        subR$ = $LEFTSTRING(curLine$, 2)
        IF (subR$ $!= "L-")
            GOSUB GetNumParamsInline
                subR$ = $RIGHTSTRING(curLine$,  SLEN(curLine$) -1)
                subR$ = "L-" + subR$
                boolVerifyGlass = 1
        ENDIF
    ENDIF
RETURN

SUB CheckSurfRange
	! Check is the surface-specific identifier contains a range of surfaces 
	! INPUT: curLine$
	! OUTPUT: SurfStart, SurfStop
	
	num_params = 0 
	GOSUB GetNumParamsInline
    IF (num_params > 1)
		subEXT$ = $GETSTRING(curLine$, 2)
	ELSE
		subEXT$ = curLine$
	ENDIF
	
	SurfStart = 0
	SurfStop = 0
	subLen = 0
	subR$ = $RIGHTSTRING(subEXT$, SLEN(subEXT$) -1)
	
	LABEL SurfRange_L1
	subLen = subLen + 1
	subD$ = $LEFTSTRING(subR$, subLen)
	subT$ = $RIGHTSTRING(subD$, 1)
	IF ((subT$ $!= ".") & (subLen <= SLEN(subR$))) THEN GOTO SurfRange_L1
	
	subD$ = $LEFTSTRING(subR$, subLen -1)
	SurfStart = SVAL(subD$)
	
	subD$ = $LEFTSTRING(subR$, subLen +1)
	subT$ = $RIGHTSTRING(subD$, 2)
	
	IF (subT$ $== "..")
		subLen = subLen + 2											
		subLen = SLEN(subEXT$) -subLen
		subR$ = $RIGHTSTRING(subEXT$, subLen)
		SurfStop = SVAL(subR$)
	ELSE
		subR$ = $RIGHTSTRING(subEXT$, SLEN(subEXT$) -1)
		SurfStart = SVAL(subR$)
		SurfStop = SurfStart
	ENDIF
RETURN

SUB FormatInteger
	! Calculates number of positions to format number to a string
	! Uses global variable IntToStr

	IF (VEC2(IntToStr) < 10) THEN FORMAT 1 INT
	IF ((VEC2(IntToStr) > 9) & (VEC2(IntToStr) < 100)) THEN FORMAT 2 INT
	IF (VEC2(IntToStr) > 100) THEN FORMAT 3 INT	
RETURN	

SUB WavelengthRange
		! Determine wavelength range from the .seq prescription
		num_params = 0 
		GOSUB GetNumParamsInline
		NumWaves = num_params - 1										# exclude the header operand 
		IF (NumWaves < 2) THEN NumWaves = 1
		
		FOR j, 1, 50, 1
			VEC3(j) = 0.0
		NEXT j
		
		FOR  j, 2, num_params , 1
			subSTR$ = $GETSTRING(curLine$, j)							# Get the j-th wavelength
			VEC3(j) = SVAL(subSTR$) / 1000.0							# make sure the units are um

			! Determine min and max wavelengths for the glass catalog
			IF (PRV_WL_MIN > VEC3(j)) THEN PRV_WL_MIN = VEC3(j)
			IF (PRV_WL_MAX < VEC3(j)) THEN PRV_WL_MAX = VEC3(j)
		NEXT j 
RETURN 

SUB AddGlassEntry
    ! Create a new glass catalog entry
	subSTR$ = $GETSTRING(curLine$, 2)
    
	subR$ = ""
	
	! Modify glass identifier
	subSTR$ = subSTR$ + "_GLASS"
    
	GOTO subSTR$ 
		! No dispersion formula
		subINT = 0														# Table Glass (Code V)
		GOTO SKIP_FORMULA_MAPPING
    LABEL HAR_GLASS
		subINT = -6														# TableGlass (HAR fitting)
		GOTO SKIP_FORMULA_MAPPING
	LABEL LAU_GLASS
	LABEL GML_GLASS
	LABEL GMS_GLASS
	LABEL SLM_GLASS 
	LABEL CAU_GLASS
		num_params = 25
	
	FOR  j, 25, 3, -1
		subD$ = $GETSTRING(curLine$, j)
		value = SVAL(subD$)
		IF (abso(value) > epsilon) THEN GOTO SKIP_PARAMS
	NEXT j 
	LABEL SKIP_PARAMS
	
	! Number of non-zero parameters
	num_params = j - 2													# first two parameters are operands 
	
	FOR j, 1, num_params, 1
		subD$ = $GETSTRING(curLine$, j + 2)
		VEC1(j) = SVAL(subD$)
	NEXT j	
		
	FORMAT 8.5

	! Dispersion Formula of Table Glass?
	! Default to Schott Formula
	subINT = 1
	
	IF (subStr$ $== "LAU_GLASS") 
		IF (num_params <= 6) THEN subINT = 1  							# Schott
		IF ((num_params > 6) & (num_params <= 8)) THEN subINT = 10  	# Extended
		IF (num_params == 9) THEN subINT = 13  							# Extended 3
		IF (num_params > 9) 
			subINT = -1													# TableGlass (LAU fitting)
			GOTO SKIP_FORMULA_MAPPING
		ENDIF
	ENDIF
	
	IF (subStr$ $== "GML_GLASS") 
		IF (num_params <= 6) 
			subINT = 1  												# Schott
		ELSE
			subINT = 12													# Extended 2
		ENDIF
	ENDIF

	IF ((subStr$ $== "GMS_GLASS") | (subStr$ $== "SLM_GLASS")) 
		IF (num_params <= 6) THEN subINT = 2  							# Sellmeier 1
		IF ((num_params == 7) | (num_params == 8)) THEN subINT = 6  	# Sellmeier 3
		IF (num_params == 10) THEN subINT = 11  						# Sellmeier 5
		IF (num_params > 10) 
			IF (subStr$ $== "GMS_GLASS")
				subINT = -3												# TableGlass (GMS fitting)
			ELSE
				subINT = -4												# TableGlass (SLM fitting)
			ENDIF
			GOTO SKIP_FORMULA_MAPPING
		ENDIF
	ENDIF
	
	IF (subStr$ $== "CAU_GLASS") 
		subINT = 1														# Schott
	ENDIF
	
	! Line 1: 
	IF (!PRV_ENTRIES)
		PRINT "CC Code V Private Glass Catalog Data"
	ENDIF
	
	! Line 2:  (subR$ is the glass name)
	subR$ = "NM "+ StrXTD$
	
	PRINT subR$,
	
	FOR j, 1, 50, 1
		VEC3(j) = 0.0
	NEXT j
	
	! Default values:
	VEC3(1)  = subINT					# Dispersion Formula Type
	VEC3(2)  = 1
	VEC3(3)  = 1
	VEC3(4)  = 0
	VEC3(5)  = 0
	VEC3(6)  = 0
	VEC3(7)  = -1	
	
	FOR j, 1, 7, 1
		IF ((j < 3) | (j > 4)) 
			FORMAT 1 INT
		ELSE
			FORMAT 8.6
		ENDIF
		PRINT " ", VEC3(j),
	NEXT j
	PRINT
	
	! Line 3:	
	PRINT "GC source: ZOS CodeV Converter"

	FORMAT 14.8 EXP
	! Line 4:	
	PRINT "ED",
	
	VEC3(1)  = 0
	VEC3(2)  = 0
	VEC3(3)  = 1
	VEC3(4)  = 0
	VEC3(5)  = 0
	VEC3(6)  = 0		

	FOR j, 1, 6, 1
		IF (j > 4) THEN FORMAT 1 INT
		PRINT " ", VEC3(j),
	NEXT j
	PRINT 
	FORMAT 14.8 EXP
	
	! Line 5:	
	PRINT "CD",

	IF (subStr$ $!= "CAU_GLASS")
		FOR j, 1, num_params, 1
			VEC3(j) = VEC1(j)			
		NEXT j
	ELSE
		! Square coefficients to go from  Cauchy to Schott formula :
		VEC3(1) = VEC1(1) * VEC1(1)
		VEC3(2) = 0														# ~ lam^2
		VEC3(3)	= 2* VEC1(1) * VEC1(2)									# ~ lam^-2
		VEC3(4) = 2* VEC1(1) * VEC1(3) + VEC1(2) * VEC1(2)				# ~ lam^-4
		VEC3(5) = 2* VEC1(2) * VEC1(3)									# ~ lam^-2
		VEC3(6) = VEC1(3) * VEC1(3)										# ~ lam^-8
		num_params = 6
 	ENDIF

	FOR j, 1, num_params, 1
		PRINT " ", VEC3(j),
	NEXT j
	
	PRINT 
	
	! Line 6:	
	PRINT "TD",
	
	VEC3(1)   = 0
	VEC3(2)   = 0
	VEC3(3)   = 0
	VEC3(4)   = 0
	VEC3(5)   = 0
	VEC3(6)   = 0	
	VEC3(7)   = 20.0	
	
	FOR j, 1, 7, 1
		PRINT " ", VEC3(j),
	NEXT j
	PRINT 
	
	! Line 7:	
	PRINT "OD",
	FORMAT 8.5
	
	VEC3(1)   = -1
	VEC3(2)   = -1
	VEC3(3)   = -1
	VEC3(4)   = -1
	VEC3(5)   = -1
	VEC3(6)   = -1

	FOR j, 1, 6, 1
		PRINT " ", VEC3(j),
	NEXT j
	PRINT 
	
	! Line 8:	
	PRINT "LD",
	FORMAT 14.8 EXP

	! Check limits
	IF (PRV_WL_MIN < PRV_MIN_LIMIT) 
        PRV_WL_MIN = PRV_MIN_LIMIT
        OUTPUT SCREEN
        PRINT "Default PRV minimum wavelength modified.  You may need to modify PRV_MIN_LIMIT variable."
        OUTPUT PRV_PATH$, APPEND
    ENDIF
	IF (PRV_WL_MIN > PRV_MAX_LIMIT) 
        PRV_WL_MIN = PRV_MIN_LIMIT
        OUTPUT SCREEN
        PRINT "Default PRV minimum wavelength modified.  You may need to modify PRV_MIN_LIMIT variable."
        OUTPUT PRV_PATH$, APPEND
    ENDIF
	IF (PRV_WL_MIN > PRV_WL_MAX) 
        PRV_WL_MIN = PRV_MIN_LIMIT
        OUTPUT SCREEN
        PRINT "Default PRV minimum wavelength modified.  You may need to modify PRV_MIN_LIMIT variable."
        OUTPUT PRV_PATH$, APPEND
    ENDIF
	IF (PRV_WL_MAX < PRV_WL_MIN ) 
        PRV_WL_MAX = PRV_MAX_LIMIT
        OUTPUT SCREEN
        PRINT "Default PRV maximum wavelength modified.  You may need to modify PRV_MAX_LIMIT variable."
        OUTPUT PRV_PATH$, APPEND
    ENDIF
	IF (PRV_WL_MAX > PRV_MAX_LIMIT) 
        PRV_WL_MAX = PRV_MAX_LIMIT
        OUTPUT SCREEN
        PRINT "Default PRV maximum wavelength modified.  You may need to modify PRV_MAX_LIMIT variable."
        OUTPUT PRV_PATH$, APPEND
    ENDIF
	IF (PRV_WL_MAX < PRV_MIN_LIMIT) 
        PRV_WL_MAX = PRV_MAX_LIMIT
        OUTPUT SCREEN
        PRINT "Default PRV maximum wavelength modified.  You may need to modify PRV_MAX_LIMIT variable."
        OUTPUT PRV_PATH$, APPEND
    ENDIF
	
	! Assign min/max values
	VEC3(1)   = PRV_WL_MIN
	VEC3(2)   = PRV_WL_MAX

	
	FOR j, 1, 2, 1
		PRINT " ", VEC3(j),
	NEXT j	
	PRINT

	! Wavelenghths for internal transmission
	VEC4(1)		=	0.3340
	VEC4(2)		=	0.3500
	VEC4(3)		=	0.3650
	VEC4(4)		=	0.3700
	VEC4(5)		=	0.3800
	VEC4(6)		=	0.3900
	VEC4(7)		=	0.4000
	VEC4(8)		=	0.4200
	VEC4(9)		=	0.4600
	VEC4(10)	=	0.5000
	VEC4(11)	=	0.6600
	VEC4(12)	=	1.0600
	VEC4(13)	=	1.5290
	VEC4(14)	=	2.3250
	
	! Line 9 - 22:
	FORMAT 12.5 EXP
	FOR k, 1, 14, 1
		PRINT "IT",
		VEC3(1)   = VEC4(k)
		VEC3(2)   = 1
		VEC3(3)   = 25

		FOR j, 1, 3, 1
			PRINT " ", VEC3(j),
		NEXT j	
		PRINT 		
	NEXT k
		
	LABEL SKIP_FORMULA_MAPPING
RETURN

SUB CreateTableGlass
	! Create table glass txt file
	! subINT contants information from AddGlassEntry 
	
	! Determine the table glass type:
	subXTR$ = ""
	IF (subINT == -1) THEN subXTR$ = "LAU_ZTG"
	IF (subINT == -3) THEN subXTR$ = "GMS_ZTG"
	IF (subINT == -4) THEN subXTR$ = "SLM_ZTG"
	IF (subINT == -6) THEN subXTR$ = "HAR_ZTG"
	IF (subXTR$ $== "") THEN subXTR$ = "TAB_ZTG"
	
	IF (subXTR$ $!= "TAB_ZTG")
		GOSUB TableGlassFromFormula
	ELSE
		GOSUB TableGlassOriginal
	ENDIF
RETURN

SUB TableGlassOriginal
	! Create ZOS table glass from wavelength and refractive indexes sets
    
	! Parse Existing Wavelength Range
	subSTR$ = curLine$												
	curLine$ = PRV_PWL_STRING$											

	num_params = 0 
	GOSUB GetNumParamsInline
	
	IF ((num_params == 0) | (num_params == 1))
		PRINT "Error: PRV wavelengths are not defined ", curLine$
	ELSE
		FOR j, 2, num_params, 1
			subD$ = $GETSTRING(curLine$, j)
			VEC3(j) = SVAL(subD$) / 1000.0
		NEXT j
	ENDIF
	
	! Store number of wavelength parameters 
	subVal = num_params													# wavelengths
	
	! Read Refractive Indexes
	curLine$ = subSTR$
	
	num_params = 0 
	GOSUB GetNumParamsInline
	
	IF ((num_params == 0) | (num_params == 1))
		PRINT "Error: PRV refractice indexes are not defined ", curLine$
	
	ELSE
		FOR j, 2, num_params, 1
			subD$ = $GETSTRING(curLine$, j)
			VEC4(j) = SVAL(subD$)
		NEXT j
	ENDIF
	
	FORMAT 0.16         # increase number of digits in ZTG file
	! only pairs of wavelength and refractive index
	if (subVal < num_params) THEN num_params = subVal
	
	PRINT "! Code V Private Glass Catalog Data"
	FOR  j, num_params, 2, -1
		PRINT VEC3(j)," ", VEC4(j)
	NEXT j
RETURN 

SUB TableGlassFromFormula
	! For CodeV glass dispersion formulas that have no ZOS equivalent
	! Create ZOS table glass

	! Calculation wavelength range:
	subNUM = (PRV_WL_MAX - PRV_WL_MIN)/ZTG_NUM_STEPS	     # step
	
	FOR  j, 1, ZTG_NUM_STEPS + 1, 1
		VEC3(j) = PRV_WL_MIN + (j - 1) * subNUM
	NEXT j
	
	num_params = 0 
	GOSUB GetNumParamsInline
	if (num_params < 3) THEN num_params = 3

	! Reset array for the formula coefficients
	FOR  k, 1, 30, 1
		VEC1(j) = 0.0
	NEXT k
	
	FOR  j, 1, num_params - 2, 1
		subD$ = $GETSTRING(curLine$, j + 2)
		VEC1(j) = SVAL(subD$)
	NEXT j
	
	GOTO subXTR$
		PRINT "Error: could not find dispersion formula"
		GOTO END_ZTG_FORMULA
		
	LABEL LAU_ZTG
		FOR  j, 1, ZTG_NUM_STEPS + 1, 1
			value = VEC3(j) * VEC3(j)
			VEC4(j) = VEC1(1) + VEC1(2) * value 
			
			value = 1
			FOR k, 3, 13, 1
				value = value / (VEC3(j) * VEC3(j)) 
				VEC4(j) = VEC4(j) + VEC1(k) * value 
			NEXT k
			VEC4(j) = SQRT(VEC4(j))
		NEXT j
		GOTO ZTG_END_N2_CASE
	
	LABEL SLM_ZTG
		FOR k, 1, 11, 2
			VEC1(k + 1) = VEC1(k + 1) * VEC1(k + 1)
		NEXT k
		
	LABEL GMS_ZTG
		FOR  j, 1, ZTG_NUM_STEPS + 1, 1
			value = VEC3(j) * VEC3(j)
			VEC4(j) = 1
			FOR k, 1, 11, 2
				VEC4(j) = VEC4(j) + (VEC1(k) * value) / (value - VEC1(k + 1))
			NEXT k
			VEC4(j) = SQRT(VEC4(j))
		NEXT j
		GOTO ZTG_END_N2_CASE
	
	LABEL HAR_ZTG	
		FOR  j, 1, ZTG_NUM_STEPS + 1, 1
			value = VEC1(3) - VEC3(j)
			VEC4(j)= VEC1(1) + VEC1(2)/POWR(value, 1.2)
		NEXT j
		
	LABEL ZTG_END_N2_CASE
	
	PRINT "! Code V Private Glass Catalog Data"
	PRINT "! ", curLine$
	FORMAT 12.8
	FOR  j, 1, ZTG_NUM_STEPS + 1, 1
		PRINT VEC3(j)," ", VEC4(j)
	NEXT j
	
	LABEL END_ZTG_FORMULA
RETURN 

SUB RemoveInvalidCharacters
    str$ = ""
    FOR ii, 1, SLEN(PRV_RESTRICTED_CHARACTERS$), 1
        temp$ = $LEFTSTRING(PRV_RESTRICTED_CHARACTERS$, ii)
        temp$ = $RIGHTSTRING(temp$, 1)
        FOR jj, 1, SLEN(subStr$), 1
            temp1$ = $LEFTSTRING(subStr$, jj)
            temp1$ = $RIGHTSTRING(temp1$, 1)
            IF temp1$ $!= temp$
                str$ = str$ + temp1$
            ENDIF
        NEXT
    NEXT
RETURN

SUB CleanupMCE_Vignet
    IF ((MCON(0, 0, 0) > 0) & (NumberConfigs > 0) & (NumFields > 0))
        FOR i = 1, NumFields, 1
            FOR j = 1, NumberConfigs, 1
                row = VU_X_MCE(i, j, 1)
                # compression factors (operand already exists)
                vux_val = VU_X_MCE(i, j, 2)
                vlx_val = VL_X_MCE(i, j, 2)
                IF (MCON(row, 0, 0) == 36) THEN SETMCOPERAND row, j, (vux_val + vlx_val) / 2, 0
                IF (MCON(row + 1, 0, 0) == 38) THEN SETMCOPERAND row + 1, j, (vux_val - vlx_val) / 2, 0
                
                row = VU_Y_MCE(i, j, 1)
                vuy_val = VU_Y_MCE(i, j, 2)
                vly_val = VL_Y_MCE(i, j, 2)
                IF (MCON(row, 0, 0) == 37) THEN SETMCOPERAND row, j, (vuy_val + vly_val) / 2, 0
                IF (MCON(row + 1, 0, 0) == 39) THEN SETMCOPERAND row + 1, j, (vuy_val - vly_val) / 2, 0
            NEXT
        NEXT
    ENDIF
RETURN

SUB EvenAsphereMapping
	DECLARE EvenAsphereCoeffs, DOUBLE, 1, 8
	FOR i, 1, 8, 1
		EvenAsphereCoeffs(i) = PARM(i, CurSurfNum)
	NEXT
RETURN 
