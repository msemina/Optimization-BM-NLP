SUBROUTINE PRTNLP(K,N,L,M,X,Y,P,ST,CRIT,HS,HP,TEXT)
! ÏOÄÏPOÃPAMMA  ÏE×ATÈ ÄËß METOÄOB HËÏ
    COMMON/A1/M1/A10/NF
    INTEGER::HS,HP,TEXT,K,N,L,M1,M
    REAL(8)::CRIT,ST
    REAL(8), DIMENSION(N)::X
    REAL(8), DIMENSION(M1)::Y
    REAL(8), DIMENSION(M)::P
! ÏÐÎÂÅÐÊÀ ÏÅÐÅÌÅÍÍÛÕ
    IF(HP==0)RETURN
    IF(K-K/HS*HS/=0)RETURN
! ÂÛÂÎÄ ÈÍÔÎÐÌÀÖÈÈ
    IF(.NOT.(K/=0))THEN
        WRITE(*,"(/5X,'×ÈCËO ÏEPEMEHHÛX',11X,'N=',I3)")N
        WRITE(*,"(5X,'×ÈCËO OÃPAHÈ×EHÈÉ-PABEHCTB L=',I3)")L
        WRITE(*,"(5X,'OÁÙEE ×ÈCËO OÃPAHÈ×EHÈÉ',4X,'M=',I3,//)")M
    END IF
    IF(TEXT==2)WRITE(*,"(/25X,'ÄOCTÈÃHÓTA ÇAÄAHHAß TO×HOCTÜ')")
    IF(TEXT==3)WRITE(*,"(/24X,'BÛÏOËHEHO ÇAÄAHHOE ×ÈCËO ØAÃOB')")
    SELECT CASE(HP)
        CASE(1)
            IF(K/=0)THEN
                WRITE(*,'(5X,I5,8X,D15.7,3X,I5)')K,Y(M1),NF
            ELSE
                WRITE(*,"(5X,'Ø A Ã',8X,'ÔÓHKÖÈß',13X,'C×T')")
            END IF
        CASE(2)
            IF(K/=0)THEN
                WRITE(*,'(5X,I5,8X,D15.7,3X,I5,3X,D15.7,3X,D15.7)')K,Y(M1),NF,ST,CRIT
            ELSE
                WRITE(*,"(/1X,'*** Ø A Ã',8X,'ÔÓHKÖÈß',13X,'C×T',8X,'ØTPAÔ',6X,'KPÈTEPÈÉ TO×HOCTÈ')")
            END IF
        CASE(3)
            WRITE(*,"(/1X,'*** Ø A Ã',8X,'ÔÓHKÖÈß',13X,'C×T',8X,'ØTPAÔ',6X,'KPÈTEPÈÉ TO×HOCTÈ')")
            WRITE(*,'(5X,I5,8X,D15.7,3X,I5,3X,D15.7,3X,D15.7)')K,Y(M1),NF,ST,CRIT
            WRITE(*,"(5X,'==--> TO×KA')")
            WRITE(*,'(7(3X,D14.7))')(X(I),I=1,N)
        CASE(4)
            WRITE(*,"(/1X,'*** Ø A Ã',8X,'ÔÓHKÖÈß',13X,'C×T',8X,'ØTPAÔ',6X,'KPÈTEPÈÉ TO×HOCTÈ')")
            WRITE(*,'(5X,I5,8X,D15.7,3X,I5,3X,D15.7,3X,D15.7)')K,Y(M1),NF,ST,CRIT
            WRITE(*,"(5X,'==--> TO×KA')")
            WRITE(*,'(7(3X,D14.7))')(X(I),I=1,N)
            WRITE(*,"(5X,'==--> OÃPAHÈ×EHÈß')")
            WRITE(*,'(7(3X,D14.7))')(Y(I),I=1,M)
            WRITE(*,"(5X,'==--> ÄBOÉCTBEHHÛE ÏEPEMEHHÛE')")
            WRITE(*,'(7(3X,D14.7))')(P(I),I=1,M)
    END SELECT
!
    RETURN
END SUBROUTINE PRTNLP
