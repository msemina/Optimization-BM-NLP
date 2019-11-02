SUBROUTINE CGS(F,X,Y,M,CGR,GR,GS,K,N1,N2,H,A)
! ПOДПPOГPAMMA  ЧИCЛEHHOГO  BЫЧИCЛEHИЯ MATPИЦЫ ГECCE ДЛЯ METOДOB HЛП
! ОПИСАНИЕ ПЕРЕМЕННЫХ
    COMMON/A1/  M1,N
    COMMON/A13/ Y1
    INTEGER::N1,N2,M,K,A,I,J
    REAL(8)::H,XA,XB,XC,XD,FA,FG
    REAL(8),DIMENSION(N)::X,GR
    REAL(8),DIMENSION(N,N)::GS
    REAL(8),DIMENSION(M1)::Y
    REAL(8),DIMENSION(6)::Y1
! КОД ПРОГРАММЫ
    KT=K
    IF(K==0)KT=M1
    DO J=N1,N2
        XA=X(J)
        XB=H*(1+0.001*DABS(XA))
        X(J)=XA-XB
        CALL F(X,Y1,K)
        FG=Y1(KT)
        X(J)=XA+XB
        CALL F(X,Y1,K)
        GS(J,J)=(FG-2*Y(KT)+Y1(KT))/XB/XB
        J1=J-1
        IF(J1/=0)THEN
            DO I=N1,J1
                XC=X(I)
                XD=H*(1+0.001*DABS(XC))
                X(I)=XC+XD
                CALL F(X,Y1,K)
                FG=Y1(KT)
                X(I)=XC-XD
                CALL F(X,Y1,K)
                FA=FG-Y1(KT)
                X(J)=XA-XB
                CALL F(X,Y1,K)
                FG=Y1(KT)
                X(I)=XC+XD
                CALL F(X,Y1,K)
                FG=Y1(KT)-FG
                X(I)=XC
                GS(I,J)=(FA-FG)/4/XB/XD
                GS(J,I)=GS(I,J)
                X(J)=XA+XB
            END DO
        END IF
        X(J)=XA
    END DO
!
    RETURN
END SUBROUTINE CGS
