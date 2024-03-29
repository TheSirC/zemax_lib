!This macro computes the fourth and sixth-order
!Wave aberration coefficients. Fourth order pupil aberration coefficents are also provided.
!The macro provides a conversion to the fifth order Buchdahl-Rimmer coefficients.
!The wave coefficients are with the field vector at the object plane and aperture vector at the exit pupil plane
!The reference sphere is centered at the Gaussian image point
!In this revision of February 2013 provision is given for the undefined case that arises when the marginal ray
!refraction invariant is zero, A=0, or when the marginal ray height is zero, y=0.
!The wave coefficients account for aspheric surfaces as specified with a conic constant and fourth and sixth order coefficients.
!The theory used to write this macro is in "Introduction to aberrations in optical imaging systems," Cambridge University Press 2012.
!THIS MACRO SHOULD ONY BE USED IN SYSTEMS WITH STANDARD AND EVEN ASPHERE SURFACES
!Written by Jose Sasian, Febraury 2013.
!Code was added to reflect change of units; May 2013




	dimen$=$UNITS()

	if (dimen$ $== "MM") then factor=1
	if (dimen$ $== "CM") then factor=10
	if (dimen$ $== "IN") then factor=25.4
	if (dimen$ $== "M") then factor=1000



	N=nsur()
	pwnum=pwav()
	onda=wavl(pwnum)/1000/factor


	w040b=0
	w131b=0
	w222b=0
	w220b=0
	w311b=0
	w400b=0

	w040t=0
	w131t=0
	w222t=0
	w220t=0
	w311t=0
	w400t=0

	w040pt=0
	w131pt=0
	w222pt=0
	w220pt=0
	w311pt=0
	w400pt=0

	w060It=0
	w333It=0
	w151It=0
	w242It=0

	w240It=0
	w331It=0
	w422It=0
	w420It=0
	w511It=0

	!*************
	!Prints header 


	FORMAT 10.4

	PRINT
	PRINT " WAVE ABERRATION COEFFICIENTS"
	Print " Wavelength =",onda*1000*factor, " micrometers"
	PRINT
	PRINT " Surface #"
	PRINT "		W040  			W131		  	W222	  		W220 	 		W311	"
	PRINT "		W040P		 	W131P		 	W222P	 		W220P	 		W311P	"
	PRINT
	PRINT "		W240I			W331I			W422I			W420I			W511I	"
	PRINT "		W060I			W151I			W242I			W333I"
	PRINT
	PRINT "		W240E			W331E			W422E			W420E			W511E	"
	PRINT "		W060E			W151E			W242E			W333E"
	PRINT


	!*****************************************************
	!Starts the surface by surface calculation with a loop 


	PARAXIAL ON
	
	j=1

	FOR j=1, N-1, 1

	
	!*********************************************************************
	!Gets the aspheric coefficients and checks for the radius of curvature
	

	rad=RADI(j)
	if (rad==0)
	rad=10e40
	endif


	kc=coni(j)
	aa = parm (2,j)+kc/rad/rad/rad/8
	bb= parm (3,j)+(kc*kc+2*kc)/rad/rad/rad/rad/rad/16

	!Accounts for mirrors

	njb=INDX(j-1)
	nj=INDX(j)
   	index = INDX(j-1)
   	indexp = INDX(j)
  	If isms(j-1) then njb = -njb
   	If isms(j) then nj = -nj

	!Trace marginal ray

	RAYTRACE 0,0,0,1	
	ym=rayy(j)
	um=(RAYM(j)/RAYN(j))
	umb=(RAYM(j-1)/RAYN(j-1))
	im=umb+ym/rad
	A=njb*im
	alfa=ym/rad
	del=um/nj-umb/njb


	!Trace chief ray

	RAYTRACE 0,1,0,0	
	yc=rayy(j)
	uc=(RAYM(j)/RAYN(j))
	ucb=(RAYM(j-1)/RAYN(j-1))
	ic=ucb+yc/rad
	B=njb*ic
	alfap=yc/rad
	delp=uc/nj-ucb/njb

	!Calculates the Lagrange invariant and checks its value
	
	If (j==1)
	LG=nj*(uc*ym-yc*um)
	If (LG==0)
	print "The lagrange invariant is zero: provide field and aperture "
	end
	endif
	endif

	!Aspheric fourth-order image coefficients

	W040as=(nj-njb)*aa*ym*ym*ym*ym
	W131as=4*(nj-njb)*aa*ym*ym*ym*yc
	W222as=4*(nj-njb)*aa*ym*ym*yc*yc
	W220as=2*(nj-njb)*aa*ym*ym*yc*yc
	w311as=4*(nj-njb)*aa*ym*yc*yc*yc
	w400as=yc*yc*yc*yc*(nj-njb)*aa


	!Aspheric fourth-order pupil coefficients


	W040asp=(nj-njb)*aa*yc*yc*yc*yc
	W131asp=4*(nj-njb)*aa*yc*yc*yc*ym
	W222asp=4*(nj-njb)*aa*yc*yc*ym*ym
	W220asp=2*(nj-njb)*aa*yc*yc*ym*ym
	W311asp=4*(nj-njb)*aa*yc*ym*ym*ym
	W400asp=(nj-njb)*aa*ym*ym*ym*ym


	!Spherical fourth-order wave image aberrations

	w040=(-1/8)*A*A*del*ym
	w131=(-1/2)*A*B*del*ym
	w222=(-1/2)*B*B*del*ym
	w220pz=(-1/4)*LG*LG*(1/nj-1/njb)/rad
	w220=w220pz+w222/2
	w311=-(1/2)*(B*B*B*ym*(1/(nj*nj)-1/(njb*njb))-B*(LG+B*ym)*yc*(1/nj-1/njb)/rad)
	w400=(-1/8)*B*B*delp*yc
 

	!Spherical fourth-order wave pupil aberrations

	w040p=(-1/8)*B*B*delp*yc
	w131p=(-1/2)*A*B*delp*yc
	w222p=(-1/2)*A*A*delp*yc
	w220p=w220pz+w222p/2
	w311p=-(1/2)*(A*A*A*yc*(1/(nj*nj)-1/(njb*njb))-A*(-LG+A*yc)*ym*(1/nj-1/njb)/rad)
	w400p=(-1/8)*A*A*del*ym



	!****************************

	!Resolves singularity when A~0


	IF (abso(A) > 1e-12) 



	!**********************

	!Factors for stop shift

	ba1=b/a
	ba2=ba1*ba1
	ba3=ba2*ba1
	ba4=ba3*ba1
	ba5=ba4*ba1
	ba6=ba5*ba1
	ba7=ba6*ba1
	ba8=ba7*ba1



	!*********************************************
	!Coefficients with stop at center of curvature


	w040Dym=(-1/8)*A*A*del


	w060i=w040*(ym*ym/rad/rad/2-a*(um/nj+umb/njb)/2+2*ym*um/rad)-8*w040*w040Dym*yc/lg

	w240cc=lg*lg*A*(um/nj/nj-umb/njb/njb)/16/rad+lg*lg*(um*um/nj-umb*umb/njb)/rad/8
	w240cc=w240cc+ym*ym*w220pz/rad/rad/4+w220pz*um*ym/rad-umb*lg*lg*(um/nj-umb/njb)/rad/4

	w240cc=+w240cc-8*ba1*w220pz*w040/lg


	w420cc=(3/16)*LG*LG*(ba2*ym*ym+yc*yc-2*ba1*ym*yc)*((1/nj)-(1/njb))/rad/rad/rad
	w420cc=w420CC-2*ba1*w220pz*w220pz/lg

	uco=uc-ba1*um
	
	w422cc=-w220pz*uco*uco
	w331cc=-2*um*uco*w220pz
	w151cc=-4*um*uco*w040
	w242cc=-2*uco*uco*w040
	


	!*****************************************************************
	!Intrinsic Coefficients upon stop shifting for a spherical surface

	w060i=W060i

	w151i=6*ba1*W060i+w151cc

 	w242i=12*ba2*w060i+4*ba1*w151cc+w242cc

	w333i=8*ba3*w060i+4*ba2*w151cc+2*ba1*w242cc

	w240i=3*ba2*w060i+ba1*w151cc+w240cc

	w331i=12*ba3*w060i+6*ba2*w151cc+4*ba1*w240cc+2*w242cc*ba1+w331cc

	w422i=12*ba4*w060i+8*ba3*w151cc+4*ba2*w240cc+5*ba2*w242cc+2*ba1*w331cc+w422cc

	w420i=3*ba4*w060i+2*ba2*w240cc+ba1*w331cc+2*ba3*w151cc+ba2*w242cc+w420cc

	w511i=6*ba5*w060i+5*ba4*w151cc+4*ba3*w240cc+3*ba2*w331cc+2*ba1*w420cc+2*ba1*w422cc+4*ba3*w242cc
	


	!******************************************************************************
	!Intrisic coefficients for case of A < 1e-12 via image-pupil connections


	ELSE


	Print "			Concentric surface "


	w240ccP=lg*lg*B*(uc/nj/nj-ucb/njb/njb)/16/rad+lg*lg*(uc*uc/nj-ucb*ucb/njb)/rad/8

	w240ccP=w240ccp+yc*yc*w220pz/rad/rad/4+w220pz*uc*yc/rad-ucb*lg*lg*(uc/nj-ucb/njb)/rad/4

	w420ccp=(3/16)*LG*LG*ym*ym*((1/nj)-(1/njb))/rad/rad/rad
	
		
	w422ccp=-w220pz*um*um
	w331ccp=-2*uc*um*w220pz
	w151ccp=-4*uc*um*w040p
	w242ccp=-2*um*um*w040p


	w060i=0
	w151ip=w151ccp
 	w242ip=w242ccp
	w333ip=0
	w240ip=w240ccp
	w331ip=w331ccp
	w422ip=w422ccp
	w420ip=w420ccp
	w511ip=0



	!***********************************************


	!Connections between image and pupil aberrations



	dw060=ym*(nj*um*um*um*um*um-njb*umb*umb*umb*umb*umb)/16
	dw420=3*lg*(uc*uc*uc*um-ucb*ucb*ucb*umb)/16
	dw240=3*lg*(um*um*um*uc-umb*umb*umb*ucb)/16
	dw331=12*lg*(um*um*uc*uc-umb*umb*ucb*ucb)/16
	dw151=3*lg*(um*um*Um*um-umb*umb*umb*umb)/8
	dw422=3*lg*(um*uc*Uc*uc-umb*ucb*ucb*ucb)/4
	dw242=3*lg*(um*um*um*uc-umb*umb*umb*ucb)/4
	dw333=1*lg*(um*um*uc*uc-umb*umb*ucb*ucb)/2
	dw511=3*lg*(uc*uc*Uc*uc-ucb*ucb*ucb*ucb)/8


	w240ppp=(2*w220*w311p+2*w131*w220p+4*w040*w131p)/lg
	W151ppp=(3*W131*w311p+8*w040*W220p+8*W040*W222p)/lg
	W242ppp=(2*W222*W311p+4*W131*W220p+6*W131*W222p+8*W040*W131p)/lg	
	w333ppp=(4*W131*W131p+4*w222*w222p)/LG	
	W331ppp=(5*w131*W131p+4*W220*W220p+4*W220*W222p+4*W222*W220p+W311*W311p+16*W040*W040p)/lg
	w420ppp=(2*W220p*W311+2*W131p*W220+4*W131*W040p)/lg
	W511ppp=(3*W131p*w311+8*w040p*W220+8*W040p*W222)/lg  
	W422ppp=(2*W222p*W311+4*W131p*W220+6*W131p*W222+8*W040p*W131)/lg


	w242i=w422ip+dw242-w242ppp
	w240i=w420ip+dw240-w240ppp
	w151i=w511ip+dw151-w151ppp
	w333i=w333ip+dw333-w333ppp
	w331i=w331ip+dw331-w331ppp
	w420i=w240ip+dw420-w420ppp
	w511i=w151ip+dw511-w511ppp
	w422i=w242ip+dw422-w422ppp


	ENDIF


	!**********************************

	!Aspheric sixth-order contributions



	!Resolves singularity when y~0

	IF (abso(ym) > 1e-9)



	ycym1=(yc/ym)



	ycym2=ycym1*ycym1
	ycym3=ycym2*ycym1
	ycym4=ycym3*ycym1
	ycym5=ycym4*ycym1
	ycym6=ycym5*ycym1



	W060bas=(nj-njb)*bb*ym*ym*ym*ym*ym*ym



	W060as=-aa*(nj*um*um-njb*umb*umb)*ym*ym*ym*ym/2+2*W040as*ym*umb/rad
	W060asp=-aa*(nj*uc*uc-njb*ucb*ucb)*yc*yc*yc*yc/2+2*W040asp*yc*ucb/rad
	w240as=w040as*lg*lg/2/ym/ym/nj/njb
	w151as=w040as*umb*lg/ym/nj+3*w040as*lg/rad/nj+8*w040as*w220pz/lg


	w060ias=w060as-8*ycym1*w040as*w040as/lg
	w151ias=w151as+6*ycym1*w060ias
	w242ias=4*ycym1*w151as+12*ycym2*w060ias
	w333ias=4*ycym2*w151as+8*ycym3*w060ias

	w240ias=w240as+ycym1*w151as+3*ycym2*w060ias
	w331ias=4*ycym1*w240as+6*ycym2*w151as+12*ycym3*w060ias+w131as*w311as/lg-4*ycym1*w040as*w311as/lg
	w422ias=4*ycym2*w240as+8*ycym3*w151as+12*ycym4*w060ias+2*w222as*w311as/lg-8*ycym2*w040as*w311as/lg
	w420ias=2*ycym2*w240as+2*ycym3*w151as+3*ycym4*w060ias+2*w220as*w311as/lg-4*ycym2*w040as*w311as/lg

	w511ias=4*ycym3*w240as+5*ycym4*w151as+6*ycym5*w060ias+3*w311as*w311as/lg-12*ycym3*w040as*w311as/lg



	W333bAS=8*ycym3*w060bas
	W151bAS=6*ycym1*w060bas
	w242bAS=12*ycym2*w060bas

	W240bAS=3*ycym2*w060bas
	W331bAS=12*ycym3*w060bas
	W422bAS=12*ycym4*w060bas
	W420bAS=3*ycym4*w060bas
	W511bAS=6*ycym5*w060bas



	ELSE


	!**************************************************************

	!Aspheric sixth-order contributions via image-pupil connections

	Print "			Image at surface"

	w151iasp=-w040asp*ucb*lg/yc/nj-3*w040asp*lg/rad/nj-8*w040asp*w220pz/lg

	w240iasp=lg*lg*w040asp/yc/yc/nj/njb/2


	w060ias=0
	w242ias=0
	w240ias=0
	w151ias=0
	w333ias=0
	w331ias=0
	w420ias=w240iasp
	w511ias=w151iasp
	w422ias=0



	W060bas=0
	W333bAS=0
	W151bAS=0
	w242bAS=0
	W240bAS=0
	W331bAS=0
	W422bAS=0
	W420bAS=0
	W511bAS=0


	ENDIF


	!**********************************************************************

	!Extrinsic aberrations between aspheric cap and base spherical surface


	w060iE=-4*w040as*w311p/lg
	W151iE=-(3*W131as*w311p+8*w040as*W220p+8*W040as*W222p)/lg
	W242iE=-(2*W222as*W311p+4*W131as*W220p+6*W131as*W222p+8*W040as*W131p)/lg
	w333iE=-(4*W131as*W131p+4*w222as*w222p)/LG
	w240iE=-(2*w220as*w311p+2*w131as*w220p+4*w040as*w131p)/lg
	W331iE=-(5*w131as*W131p+4*W220as*W220p+4*W220as*W222p+4*W222as*W220p+W311as*W311p+16*W040as*W040p)/lg
	W422iE=-(2*W311as*W222p+4*W220as*W131p+6*W222as*W131p+8*W131as*W040p)/lg
	w420iE=-(2*W220as*W131p+2*W311as*W220p+4*W131as*W040p)/lg
	W511iE=-(3*W311as*W131p+8*W220as*W040p+8*W222as*W040p)/lg


	!******************

	!Add aspheric terms

	W060IAS=w060ias+w060bas+w060ie
	W333IAS=w333ias+W333bAS+w333ie
	W151IAS=w151ias+W151bAS+w151ie
	w242IAS=w242ias+w242bAS+w242ie

	W240IAS=w240ias+W240bAS+w240ie
	W331IAS=w331ias+W331bAS+w331ie
	W422IAS=w422ias+W422bAS+w422ie
	W420IAS=w420ias+W420bAS+w420ie
	W511IAS=w511ias+W511bAS+w511ie



	!*********************

	!Extrinsic aberrations


	w040p=w040p+w040asp
	w131p=w131p+w131asp
	w222p=w222p+w222asp
	w220p=w220p+w220asp
	w311p=w311p+w311asp



	w060E=-4*w040b*w311p/lg
	W151E=-(3*W131b*w311p+8*w040b*W220p+8*W040b*W222p)/lg
	W242E=-(2*W222b*W311p+4*W131b*W220p+6*W131b*W222p+8*W040b*W131p)/lg
	w333E=-(4*W131b*W131p+4*w222b*w222p)/LG
	w240E=-(2*w220b*w311p+2*w131b*w220p+4*w040b*w131p)/lg
	W331E=-(5*w131b*W131p+4*W220b*W220p+4*W220b*W222p+4*W222b*W220p+W311b*W311p+16*W040b*W040p)/lg	
	W422E=-(2*W311b*W222p+4*W220b*W131p+6*W222b*W131p+8*W131b*W040p)/lg	
	w420E=-(2*W220b*W131p+2*W311b*W220p+4*W131b*W040p)/lg
	W511E=-(3*W311b*W131p+8*W220b*W040p+8*W222b*W040p)/lg


	w040b=w040b+w040+w040as
	w131b=w131b+w131+w131as
	w222b=w222b+w222+w222as
	w220b=w220b+w220+w220as
	w311b=w311b+w311+w311as



	!********************************
	!Stores the surface contributions

	w040pt=w040pt+w040p
	w131pt=w131pt+w131p
	w222pt=w222pt+w222p
	w220pt=w220pt+w220p
	w311pt=w311pt+w311p


	w040=w040+w040as
	w131=w131+w131as
	w222=w222+w222as
	w220=w220+w220as
	w311=w311+w311as


	w040t=w040t+w040
	w131t=w131t+w131
	w222t=w222t+w222
	w220t=w220t+w220
	w311t=w311t+w311


	w060ti=w060I+W060IAS
	w333ti=w333I+W333IAS
	w151ti=w151I+W151IAS
	w242ti=w242I+w242IAS

	w240ti=w240I+W240IAS
	w331ti=w331I+W331IAS
	w422ti=w422I+W422IAS
	w420ti=w420I+W420IAS
	w511ti=w511I+W511IAS


	w060st=w060I+W060E+W060IAS
	w333st=w333I+w333E+W333IAS
	w151st=w151I+W151E+W151IAS
	w242st=w242I+W242E+w242IAS

	w240st=w240I+W240E+W240IAS
	w331st=w331I+W331E+W331IAS
	w422st=w422I+W422E+W422IAS
	w420st=w420I+w420E+W420IAS
	w511st=w511I+W511E+W511IAS


	w060It=w060It+w060st
	w333It=w333It+w333st
	w151It=w151It+w151st
	w242It=w242It+w242st

	w240It=w240It+w240st
	w331It=w331It+w331st
	w422It=w422It+w422st
	w420It=w420It+w420st
	w511It=w511It+w511st


	
	!******************
	!Print coefficients

	FORMAT 10.4

	PRINT j
	PRINT w040/onda ,"		",w131/onda, "		",w222/onda, "		",w220/onda, "		",w311/onda
	PRINT w040p/onda ,"		",w131p/onda, "		",w222p/onda, "		",w220p/onda, "		",w311p/onda
	print
	PRINT w240ti/onda ,"		",w331ti/onda, "		",w422ti/onda, "		",w420ti/onda, "		",w511ti/onda
	PRINT w060ti/onda ,"		",w151ti/onda, "		",w242ti/onda, "		",w333ti/onda 
	PRINT
	PRINT w240e/onda ,"		",w331e/onda, "		",w422e/onda, "		",w420e/onda, "		",w511e/onda
	PRINT w060e/onda ,"		",w151e/onda, "		",w242e/onda, "		",w333e/onda 
	PRINT


	next
	paraxial off

	PRINT " TOTALS"
	PRINT w040t/onda ,"		",w131t/onda, "		",w222t/onda, "		",w220t/onda, "		",w311t/onda
	PRINT w040pt/onda ,"		",w131pt/onda, "		",w222pt/onda, "		",w220pt/onda, "		",w311pt/onda
	PRINT
	PRINT w240it/onda ,"		",w331it/onda, "		",w422it/onda, "		",w420it/onda, "		",w511it/onda
	PRINT w060it/onda ,"		",w151it/onda, "		",w242it/onda, "		",w333it/onda 


	!Code to return values of aberrations to be called by optimization routine

	optreturn 1, w040t/onda 
	optreturn 2, w131t/onda
	optreturn 3, w222t/onda
	optreturn 4, w220t/onda
	optreturn 5, w311t/onda
	optreturn 6, w060it/onda 
	optreturn 7, w151it/onda
	optreturn 8, w242it/onda
	optreturn 9, w333it/onda
	optreturn 10, w240it/onda
	optreturn 11, w331it/onda
	optreturn 12, w422it/onda
	optreturn 13, w420it/onda
	optreturn 14, w511it/onda


!*****************

!Prints Buchdahl-Rimmer fifth-order coefficients
Print
Print
Print "			Buchdahl-Rimmer fifth-order coefficients"

Format 14.4 EXP
print


yc=-lg/nj/um

H151x=-(8*uc*um*w040t+4*um*um*w131t)/lg
H151x=H151x-4*w151it/lg-(24*w040t*w222pt+4*w131t*w311pt+16*w040t*w220pt)/lg/lg

H151y=-(1/2*um*um*w131t+4*w040t*um*uc)/lg
H151y=H151y-w151it/lg-(8*w040t*w220pt+2*w131t*w311pt)/lg/lg

F1=h151y*yc+h151x*yc/2
F2=H151x*yc/2 

print "F1  ", F1
print "F2  ", F2 


!*****************

H333y=-(4*um*uc*w222t+2*uc*uc*w131t)/lg
H333y=H333y-3*w333it/lg-(8*w131t*w131pt+4*w222t*w222pt)/lg/lg

!*****************

H331y=-(3/2*uc*uc*w131t+3/2*um*um*w311t+2*um*uc*w220t-um*um*w311t)/lg
H331y=H331y-w331it/lg-(16*w040t*w040pt+2*w131t*w131pt+4*w222t*w220pt+4*w220t*w220pt)/lg/lg


H331x=-(uc*uc*w131t+4*um*uc*w220t+2*w222t*uc*um+um*um*w311t)/lg
H331x=H331x-2*w331it/lg-(32*w040t*w040pt+8*w131t*w131pt+4*w220t*w222pt)/lg/lg

N1=H331y*yc
N2=H331x*yc+H333y*yc
N3=H331x*yc/2 

print "N1  ", N1
print "N2  ", N2
print "N3  ", N3



!*****************

H240x=-(3*um*um*w220t+2*uc*uc*w040t+uc*um*w131t)/lg
H240x = H240x-4*w240it/lg-(12*w040t*w131pt+4*w131t*w220pt+2*w220t*w311pt)/lg/lg


!*****************
H242y =-(4*um*uc*w131t+um*um*w222t+4*uc*uc*w040t)/lg
H242y = H242y-2*w242it/lg-(8*w040t*w131pt+4*w131t*w222pt+8*w131t*w220pt+2*w222t*w311pt)/lg/lg


H242x=-(4*um*uc*w131t+2*um*um*w222t)/lg
H242x=H242x-2*w242it/lg-(16*w040t*w131pt+8*w131t*w222pt)/lg/lg

M1=H242y*yc
M2=H240x*yc
M3=H242x*yc

print "M1  ", M1
print "M2  ", M2
print "M3  ", M3


!*****************
H422y=-(3*uc*uc*w222t+2*um*uc*w311t+2*uc*uc*w220t)/lg
H422y=H422y-2*w422it/lg-(16*w131t*w040pt+4*w220t*w131pt+6*w222t*w131pt)/lg/lg


H420x=-(uc*uc*w220t+um*uc*w311t)/lg
H420x=H420x-2*w420it/lg-(8*w131t*w040pt+2*w220t*w131pt)/lg/lg

c5=H422y*yc/4
Pi5=(H420x-H422y/4)*yc

print "C5  ", C5
print "Pi5 ", Pi5


H060=-(6*um*um*w040t+6*w060it+12*w040t*w311pt/lg)/lg

B=H060*yc

print "B5  ", B

H511=-(3/2*uc*uc*w311t+w511it+(8*w222t*w040pt+8*w220t*w040pt)/lg)/lg

E5=H511*yc

print "E5  ",E5


