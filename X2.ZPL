FOR J, 0, 5, 1
FOR Angle , -25+60*J, 10+60*J, 7
SETNSCPOSITION 2, 2, 4, Angle

UPDATE EDITORS

FOR L, -1, 1, 1/16
RAYTRACE L, 0, 0, 0

M=ACOS(RAYL(3))*52.295
IF M>90 
    P=M-90
ELSE 
    P=M
ENDIF


O=ACOS(RAYM(3))*52.295
IF O>90 
    Q=Q-90
ELSE 
    Q=O
ENDIF

makefolder "D:\TEST"
OUTPUT "D:\TEST\angle_12THREE.csv" ,APPEND
PRINT Q

NEXT
NEXT
NEXT




