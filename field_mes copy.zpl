FOR J, 0, 5, 1
FOR Angle , -140-60*J, -175-60*J, -0.25
SETNSCPOSITION 14, 1, 5, Angle

UPDATE 
FOR L, -1, 1, 2/31
RAYTRACE 0, L, 0, 0

a = RAGL(15)
b = RAGM(15)

x_angle = ASIN(a)*57.29578
y_angle = ASIN(b)*57.29578

OUTPUT "D:\TEST\angle13000.csv" ,APPEND
!PRINT x_angle
!PRINT y_angle
!PRINT x_angle,":",y_angle
print Angle
NEXT
NEXT
NEXT