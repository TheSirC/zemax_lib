! by wys

thi = THIC(1)
PRINT "default z thick is: ", thi

## construct 4 vectors for 4 fields
vec1 0, 3
vec1 1, 1
vec1 2, 1 ## field
vec1 3, 1 ## 1 for ignore source
vec1 4, 0
vec1 5, 0.159 ## receiver NAx
vec1 6, 0
vec1 7, 0
vec1 8, 0
vec1 9, 0
vec1 10, 0
vec1 11, 0
vec1 12, 0
vec1 13, 1
vec1 14, 1 ## align receiver to the chief ray
vec1 15, 0
vec1 16, 0
vec1 17, 0 ## receiver NAy
vec1 18, 1 ## Huygens

!for i, 0, 18, 1
!    vec2 i = vec1(i)
!    vec3 i = vec1(i)
!    vec4 i = vec1(i)
!next
!vec2 2, 2
!vec3 2, 3
!vec4 2, 4

n = 500
n_total = 2*n+1
declare ficl_arr, double, 1, n_total
declare z_arr, double, 1, n_total
for k = 1, n_total, 1
    z_arr(k) = k/10
next

!OUTPUT "C:/wys/z_energy_eva.csv"
!PRINT ""


FOR i = 1, n_total, 1
!    OUTPUT SCREEN
    z = i*100
    PRINT i, " @ z thick set to: ", z
    THIC(1) = z
    UPDATE
    e1 = FICL(1)
!    e2 = FICL(2)
!    e3 = FICL(3)
!    e4 = FICL(4)
    ficl_arr(i) = e1
!    OUTPUT "C:/wys/z_energy_eva.csv", APPEND
!    PRINT i, ",", e1*225, ",", e2*225, ",", e3*225, ",", e4*225
    PRINT i, ",", e1
NEXT

PLOT DATA, z_arr, ficl_arr, n_total, 0,0,0
PLOT TITLEX, "object distance(m)"
PLOT TITLEY, "coupling efficiency by ficl"
PLOT TITLE, "coupling efficiency vs object distance"
PLOT FORMATX, "%4.0f"
!PLOT RANGEY, 0, 0.4
PLOT TICK, n/50
PLOT GO

THIC(1) = thi