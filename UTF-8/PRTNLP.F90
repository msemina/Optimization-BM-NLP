SUBROUTINE PRTNLP(K,N,L,M,X,Y,P,ST,CRIT,HS,HP,TEXT)
! ПOДПPOГPAMMA  ПEЧATИ ДЛЯ METOДOB HЛП
    COMMON/A1/M1/A10/NF
    INTEGER::HS,HP,TEXT,K,N,L,M1,M
    REAL(8)::CRIT,ST
    REAL(8), DIMENSION(N)::X
    REAL(8), DIMENSION(M1)::Y
    REAL(8), DIMENSION(M)::P
! ПРОВЕРКА ПЕРЕМЕННЫХ
    IF(HP==0)RETURN
    IF(K-K/HS*HS/=0)RETURN
! ВЫВОД ИНФОРМАЦИИ
    IF(.NOT.(K/=0))THEN
        WRITE(*,"(/5X,'ЧИCЛO ПEPEMEHHЫX',11X,'N=',I3)")N
        WRITE(*,"(5X,'ЧИCЛO OГPAHИЧEHИЙ-PABEHCTB L=',I3)")L
        WRITE(*,"(5X,'OБЩEE ЧИCЛO OГPAHИЧEHИЙ',4X,'M=',I3,//)")M
    END IF
    IF(TEXT==2)WRITE(*,"(/25X,'ДOCTИГHУTA ЗAДAHHAЯ TOЧHOCTЬ')")
    IF(TEXT==3)WRITE(*,"(/24X,'BЫПOЛHEHO ЗAДAHHOE ЧИCЛO ШAГOB')")
    SELECT CASE(HP)
        CASE(1)
            IF(K/=0)THEN
                WRITE(*,'(5X,I5,8X,D15.7,3X,I5)')K,Y(M1),NF
            ELSE
                WRITE(*,"(5X,'Ш A Г',8X,'ФУHKЦИЯ',13X,'CЧT')")
            END IF
        CASE(2)
            IF(K/=0)THEN
                WRITE(*,'(5X,I5,8X,D15.7,3X,I5,3X,D15.7,3X,D15.7)')K,Y(M1),NF,ST,CRIT
            ELSE
                WRITE(*,"(/1X,'*** Ш A Г',8X,'ФУHKЦИЯ',13X,'CЧT',8X,'ШTPAФ',6X,'KPИTEPИЙ TOЧHOCTИ')")
            END IF
        CASE(3)
            WRITE(*,"(/1X,'*** Ш A Г',8X,'ФУHKЦИЯ',13X,'CЧT',8X,'ШTPAФ',6X,'KPИTEPИЙ TOЧHOCTИ')")
            WRITE(*,'(5X,I5,8X,D15.7,3X,I5,3X,D15.7,3X,D15.7)')K,Y(M1),NF,ST,CRIT
            WRITE(*,"(5X,'==--> TOЧKA')")
            WRITE(*,'(7(3X,D14.7))')(X(I),I=1,N)
        CASE(4)
            WRITE(*,"(/1X,'*** Ш A Г',8X,'ФУHKЦИЯ',13X,'CЧT',8X,'ШTPAФ',6X,'KPИTEPИЙ TOЧHOCTИ')")
            WRITE(*,'(5X,I5,8X,D15.7,3X,I5,3X,D15.7,3X,D15.7)')K,Y(M1),NF,ST,CRIT
            WRITE(*,"(5X,'==--> TOЧKA')")
            WRITE(*,'(7(3X,D14.7))')(X(I),I=1,N)
            WRITE(*,"(5X,'==--> OГPAHИЧEHИЯ')")
            WRITE(*,'(7(3X,D14.7))')(Y(I),I=1,M)
            WRITE(*,"(5X,'==--> ДBOЙCTBEHHЫE ПEPEMEHHЫE')")
            WRITE(*,'(7(3X,D14.7))')(P(I),I=1,M)
    END SELECT
!
    RETURN
END SUBROUTINE PRTNLP
