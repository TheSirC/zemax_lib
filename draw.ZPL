FOR J, 0, 5, 1
FOR angle , 20+60*J, 55+60*J, 7
SETNSCPOSITION 14, 1, 5, angle


FOR L, -1, 1, 2/31
RAYTRACE 0, l, 0, 0
UPDATE all
a = RAGL(15)
b = RAGM(15)
c = ragn(15)
!x_angle = asin(a)*57.29578
!y_angle = asin(b)*57.29578
!z=asin(c)*57.29578
OUTPUT "D:\TEST\vec_r1.csv" ,APPEND
!PRINT x_angle
!PRINT y_angle
!PRINT x_angle,",",y_angle,",",z

print a,",",b,",",c


!print x_angle,y_angle
NEXT
NEXT
NEXT