! МЕТОД ВНЕШНИХ ШРАФНЫХ ФУНКЦИЙ
SUBROUTINE CP4(N,L,M,X,A,B,P,F,CGR,CGS,Y,PAR,Q,UNCONS)
! ОПИСАНИЕ ПЕРЕМЕННЫХ
    COMMON /A1/   M1, N__UNUSED, L__UNUSED
    COMMON /A10/  NF
    COMMON /A11/  G
    COMMON /AGR1/ R1,K1,K2,K3,T,YA,VAR
    INTEGER::N,L,M,D,I,K,KK,KKK,J,VAR,SHAGP,PODRP,TEXT,Q
    REAL(8)::E,TV,YA,FS,ES,YZ,T,EBM,Z,BB,R1,ST,SB,EW,YB,K1,K2,K3,E1,YS,CRIT,FHCP4
    REAL(8), DIMENSION(3)::G
    REAL(8), DIMENSION(40)::PAR
    REAL(8), DIMENSION(N)::X,A,B
    REAL(8), DIMENSION(M)::P
    REAL(8), DIMENSION(M1)::Y
    EXTERNAL F,AGRCP4,AGECP4,FHCP4
!
    E=PAR(1)
    KK=PAR(3)
    D=PAR(2)+KK
    VAR=PAR(4)
    SB=PAR(5)
    T=PAR(6)
    Z=PAR(7)
    ES=PAR(8)
    SHAGP=PAR(9)
    PODRP=PAR(10)
    TEXT=0
    CRIT=-1.0
    EBM=PAR(1+Q)
    E1=PAR(1+Q)
    YA=E1/100.
    IF(YA>1.D-5) YA=1.D-5
    EW=1.D-10
    R1=1.D-4
    K1=1./(3*R1)
    K2=-R1
    K3=R1**2/3.
    BB=E1*(1.+DLOG(1.+T)*0.1/ES)
    NF=0
    CALL F(X,Y,-1)
    FS=Y(M1)
    ST=0.
    DO I=1,M
        IF(I<=L)THEN
            ST=ST+DABS(Y(I))
        ELSE
            IF(Y(I)>0)THEN
                ST=ST+Y(I)
            END IF
        END IF
    END DO
    IF(PODRP/=0)THEN
        WRITE(*,"(/A/)")    '     METOД BHEШHИX ШTPAФHЫX ФУHKЦИЙ: CP4'
        WRITE(*,"(A,D12.5)")'     TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ HЛП     E=  ',E
        WRITE(*,"(A,I5)")   '     ЧИCЛO ИTEPAЦИЙ                  D=  ',D
        WRITE(*,"(A,I5)")   '     BEPCИЯ METOДA                   VAR=',VAR
        WRITE(*,"(A,D12.5)")'     BEC OГPAHИЧEHИЙ                 SB= ',SB
        WRITE(*,"(A,D12.5)")'     HAЧAЛЬHЫЙ KOЭФФИЦИEHT ШTPAФA    T=  ',T
        WRITE(*,"(A,D12.5)")'     KOЭФФИЦИEHT УBEЛИЧEHИЯ ШTPAФA   Z=  ',Z
        WRITE(*,"(A,D12.5)")'     ПAPAMETP BCПOMOГ. ЗAДAЧИ        ES= ',ES
    END IF
    CALL PRTNLP(0,N,L,M,X,Y,P,ST,CRIT,1,4,TEXT)
    DO I=1,N
        IF(X(I)<A(I)-EW.OR.X(I)>B(I)+EW)THEN
            WRITE(*,"(/10X,'HAЧAЛЬHAЯ TOЧKA BHE ПAPAЛЛEЛEПИПEДA')")
            PAR(1+Q)=EBM
            IF(PODRP/=0)WRITE(*,"(/30X,'OПTИMAЛЬHAЯ TOЧKA')")
            CALL PRTNLP(KK,N,L,M,X,Y,P,ST,CRIT,1,4,TEXT)
            RETURN
        END IF
    END DO
! HAЧAЛO OCHOBHOГO ЦИKЛA
    DO
        K=KK+1
        IF(.NOT.(K<D+1.AND.DABS(CRIT)>E))EXIT
        YB=Y(M1)
        CALL UNCONS(N,X,A,B,FHCP4,AGRCP4,AGECP4,YS,G,Q,PAR,F)
        CALL F(X,Y,-1)
        KKK=PAR(3+Q)
        IF(KKK/=0)THEN
            YS=FHCP4(X,F)
            FS=YB
            TV=T
            ST=0.
            DO I=1,M
                IF(I<=L)THEN
                    ST=ST+DABS(Y(I))
                ELSE
                    IF(Y(I)>0)THEN
                        ST=ST+Y(I)
                    END IF
                END IF
            END DO
        END IF
        IF(ST*SB>E)THEN
            T=T*Z
        ELSE
            BB=BB*ES
        END IF
        PAR(1+Q)=BB/(1+DLOG(1.+T)*0.1/ES)
        PAR(3+Q)=0.
        KK=K
        CRIT=DABS(FS-Y(M1))/(1+DABS(FS))+ST*SB
        CALL PRTNLP(KK,N,L,M,X,Y,P,ST,CRIT,SHAGP,PODRP,TEXT)
    END DO
! KOHEЦ OCHOBHOГO ЦИKЛA
    IF(VAR==1)THEN
        IF(L>=1)THEN
            DO I=1,L
                P(I)=2*TV*Y(I)
            END DO
        END IF
        L1=L+1
        IF(L1<=M)THEN
            DO I=L1,M
                YZ=Y(I)
                IF(YZ<0)THEN
                    P(I)=0.
                ELSE
                    IF(YZ<R1)THEN
                        P(I)=TV*3*K1*YZ*YZ
                    ELSE
                        P(I)=TV*(2*YZ+K2)
                    END IF
                END IF
            END DO
        END IF
    END IF
    IF(PODRP>0.AND.CRIT<=E)TEXT=2
    IF(PODRP>0.AND.KK==D.AND.TEXT/=2)TEXT=3
    PAR(1+Q)=EBM
    IF(PODRP/=0)WRITE(*,"(/32X,'OПTИMAЛЬHAЯ TOЧKA')")
    CALL PRTNLP(KK,N,L,M,X,Y,P,ST,CRIT,1,4,TEXT)
    RETURN
END SUBROUTINE CP4
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE AGRCP4(N,FHCP4,X1,YS,GR,N1,N2,HH,AC,F)
! ОПИСАНИЕ ПЕРЕМЕННЫХ
    COMMON /A1/   M1,N11,L
    COMMON /AGR1/ R1,K1,K2,K3,T,YA,VAR
    COMMON /AGR2/ Y
    COMMON /AGR3/ GB
    INTEGER::N1,N2,AC,I,J,L,M,VAR,N
    REAL(8)::YS,HH,YZ,YH,R1,K1,T,K2,YA,FHCP4,K3,F
    REAL(8), DIMENSION(3)::GB
    REAL(8), DIMENSION(6)::Y
    REAL(8), DIMENSION(N)::X1,GR
    EXTERNAL F
!
    M=M1-1
    CALL CGR(F,X1,Y,M,GR,0,N1,N2,HH,AC)
    DO I=1,M
        YZ=Y(I)
        IF(VAR==1)THEN
            IF(I<=L)THEN
                YH=YZ*2
            ELSE IF(YZ<=0)THEN
                YH=0.
            ELSE IF(YZ<=R1)THEN
                YH=3*K1*YZ*YZ
            ELSE
                YH=2*YZ+K2
            END IF
        ELSE IF(VAR==2)THEN
            IF(I<=L)THEN
                IF(YZ>0)THEN
                    YH=1.
                ELSE
                    YH=-1.
                END IF
            ELSE 
                IF(YZ<=0)THEN
                    YH=0.
                ELSE
                    YH=1.
                END IF
            END IF
        END IF
        YH=T*YH
        IF(DABS(YH)>YA)THEN
            CALL CGR(F,X1,Y,M,GB,I,N1,N2,HH,AC)
            DO J=N1,N2
                GR(J)=GR(J)+GB(J)*YH
            END DO
        END IF
    END DO
    RETURN
END SUBROUTINE AGRCP4
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE AGECP4(N,FHCP4,X1,YS,FGR,GR,GS,N1,N2,HH,AC,F)
! ОПИСАНИЕ ПЕРЕМЕННЫХ
    COMMON /A1/    M1,N11,L
    COMMON /AGR1/  R1,K1,K2,K3,T,YA,VAR
    COMMON /AGR2/  Y
    COMMON /AGES1/ G1
    COMMON /AGES2/ GB
    INTEGER::N1,N2,AC,I,J,S,L,M,N,VAR
    REAL(8)::HH,YS,YZ,YH,R1,K1,T,YA,K2,FHCP4,K3,F
    REAL(8), DIMENSION(3)::G1,GB
    REAL(8), DIMENSION(6)::Y
    REAL(8), DIMENSION(N)::X1,GR
    REAL(8), DIMENSION(N,N)::GS
    EXTERNAL FGR,F
!
    M=M1-1
    IF(AC==3)THEN
        CALL CGR(F,X1,Y,M,G1,0,N1,N2,HH,AC)
    END IF 
    CALL CGS(F,X1,Y,M,FGR,G1,GS,0,N1,N2,HH,AC)
    DO I=1,M
        YZ=Y(I)
        IF(I<=L)THEN
            YH=2.
        ELSE IF(YZ<=0)THEN
            YH=0.
        ELSE IF(YZ<=R1)THEN
            YH=6*K1*YZ
        ELSE
            YH=2.
        END IF
        YH=YH*T
        IF(DABS(YH)>YA.OR.AC==3)THEN
            CALL CGR(F,X1,Y,M,G1,I,N1,N2,HH,AC)
            DO J=N1,N2
                DO S=N1,J
                    GS(J,S)=GS(J,S)+G1(J)*G1(S)*YH
                    GS(S,J)=GS(J,S)
                END DO
            END DO
        END IF
        IF(I<=L)THEN
            YH=2*YZ
        ELSE IF(YZ<=0)THEN
            YH=0.
        ELSE IF(YZ<=R1)THEN
            YH=3*K1*YZ*YZ
        ELSE
            YH=2*YZ+K2
        END IF
        YH=T*YH
        IF(DABS(YH)>YA)THEN
            CALL CGS(F,X1,Y,M,FGR,G1,GB,I,N1,N2,HH,AC)
            DO J=N1,N2
                DO S=N1,J
                    GS(J,S)=GS(J,S)+GB((S-1)*N+J)*YH
                    GS(S,J)=GS(J,S)
                END DO
            END DO
        END IF
    END DO
    RETURN
END SUBROUTINE AGECP4
!----------------------------------------------------------------------------------------------------------------------------------
FUNCTION FHCP4(X,F)
! ОПИСАНИЕ ПЕРЕМЕННЫХ
    COMMON /C/    NF
    COMMON /A1/   M1,N,L
    COMMON /AGR1/ R1,K1,K2,K3,T,YA,VAR
    COMMON /AGR2/ Y
    INTEGER::I,M,VAR,L,N
    REAL(8)::YZ,YH,T,R1,K1,K2,K3,YA,FHCP4
    REAL(8), DIMENSION(6)::Y
    REAL(8), DIMENSION(N)::X
!
    M=M1-1
    ! PRINT *, (X(I), I=1,2 )
    CALL F(X,Y,-1)
    YH=Y(M1)
    DO I=1,M
        YZ=Y(I)
        IF(VAR==1)THEN
            IF(I<=L)THEN
                YH=YH+T*YZ**2
            ELSE
                IF(YZ>0)THEN
                    IF(YZ<=R1)THEN
                        YH=YH+T*K1*YZ*YZ*YZ
                    ELSE
                        YH=YH+T*(YZ**2+K2*YZ+K3)
                    END IF
                END IF
            END IF
        ELSE IF(VAR==2)THEN
            IF(I<=L)THEN
                YH=YH+T*DABS(YZ)
            ELSE
                IF(YZ>0)YH=YH+T*YZ
            END IF
        END IF
    END DO
    FHCP4=YH
    NF=NF+1
    RETURN
END FUNCTION FHCP4
!----------------------------------------------------------------------------------------------------------------------------------

