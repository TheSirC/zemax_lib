
FOR J, 0, 5, 1
FOR Angle , 45+60*J, 80+60*J, 7

SETNSCPOSITION 2, 2, 4, Angle

UPDATE
FOR L, -1, 1, 2/31
RAYTRACE L, 0, 0, 0 


!L1=RAGL(3)
!M=RAGM(3)

L1 = RAYL(3)
M = RAYN(3)

D=ASIN(L1)*57.295
F=ASIN(M)*57.295



OUTPUT "D:\TEST\angle_110.csv" ,APPEND
PRINT F
!PRINT O
!PRINT F 


NEXT
NEXT
NEXT


