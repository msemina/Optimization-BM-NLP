FUNCTION F(X,FNLP)
! ПPИMEP ПOДГOTOBKИ ФУHKЦИИ ДЛЯ ЗAДAЧИ БEЗУCЛOBHOЙ MИHИMИЗAЦИИ
!                ( ФУНКЦИЯ РОЗЕНБРОКА )
    COMMON /C/ NF
    REAL(8), DIMENSION(2)::X
    REAL(8)::F
    INTEGER::NF
! CЧETЧИK BЫЧИCЛEHИЙ ФУHKЦИИ
    NF=NF+1
! PACЧET MИHИMИЗИPУEMOЙ ФУHKЦИИ
    F=100 * (X(2) - X(1)**2 )**2 + (1 - X(1))**2
    RETURN
END FUNCTION F
