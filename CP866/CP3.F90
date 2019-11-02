! åÖíéÑ èÄêÄåÖíêàáÄñàà ñÖãÖÇéâ îìçäñàà
SUBROUTINE CP3(N,L,M,X,A,B,P,F,CGR,CGS,Y,PAR,Q,UNCONS)
! éèàëÄçàÖ èÖêÖåÖççõï
    COMMON /A1/   M1, N__UNUSED, L__UNUSED
    COMMON /A10/  NF
    COMMON /A11/  G
    COMMON /AGR1/ R,K1,K2,K3,R1,YA,PZ
    INTEGER::N,L,M,KKK,D,I,K,KK,SHAGP,PODRP,VAR,TEXT,L1,Q,NF
    REAL(8)::ES,ST,E,H,FS,R,YS,PZ,K1,K2,K3,RR,R1,YZ,T,TA,YA,BB,EW,CRIT,FHCP3,BB1
    REAL(8), DIMENSION(3)::G
    REAL(8), DIMENSION(40)::PAR
    REAL(8), DIMENSION(N)::X,A,B
    REAL(8), DIMENSION(M)::P
    REAL(8), DIMENSION(M1)::Y
    EXTERNAL FHCP3,AGRCP3,AGECP3,F
! 
    E=PAR(1)
    KK=PAR(3)
    D=PAR(2)+KK
    VAR=PAR(4)
    R=PAR(5)
    ES=PAR(6)
    SHAGP=PAR(7)
    PODRP=PAR(8)
    TEXT=0.
    EW=1.D-9
    CRIT=-1.0
    BB=PAR(1+Q)
    BB1=PAR(1+Q)
    YA=BB/100.
    IF(YA>1.D-5)YA=1.D-5
    BB=BB*(1+0.1*KK/ES)
    R1=1.D-4
    K1=1/(3*R1)
    K2=-R1
    K3=R1**2/3
    RR=R
    NF=0
    CALL F(X,Y,-1)
    FS=1+E*(1+DABS(R))
    ST=0.
    DO I=1,M
        IF(I<=L)THEN
            ST=ST+DABS(Y(I))
        ELSE
            IF(Y(I)>0)ST=ST+Y(I)
        END IF
    END DO
    IF(PODRP/=0)THEN
        WRITE(*,"(/A/)")    '     METOÑ èAPAMETPàáAñàà ñEãEBOâ îìHKñàà: CP3'
        WRITE(*,"(A,D12.5)")'     TOóHOCTú PEòEHàü áAÑAóà Hãè     E=  ',E
        WRITE(*,"(A,I5)")   '     óàCãO àTEPAñàâ                  D=  ',D
        WRITE(*,"(A,I5)")   '     BEPCàü METOÑA                   VAR=',VAR
        WRITE(*,"(A,D12.5)")'     HàÜHüü OñEHKA                   R=  ',R
        WRITE(*,"(A,D12.5)")'     èAPAMETP BCèOMOÉ. áAÑAóà        ES= ',ES
    END IF
    CALL PRTNLP(0,N,L,M,X,Y,P,ST,CRIT,1,4,TEXT)
    DO I=1,N
        IF(X(I)<(A(I)-EW).OR.X(I)>(B(I)+EW))THEN
            WRITE(*,"(/20X,'HAó TOóKA BHE èAPAããEãEèàèEÑA')")
            RETURN
        END IF
    END DO
! HAóAãO OCHOBHOÉO ñàKãA
    DO
        K=KK+1
        CALL UNCONS(N,X,A,B,FHCP3,AGRCP3,AGECP3,YS,G,Q,PAR,F)
        CALL F(X,Y,-1)
        KKK=PAR(3+Q)
        IF(KKK==0)THEN
            BB=BB*ES
        ELSE
            RR=R
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
            SELECT CASE(VAR)
                CASE(1)
                    FS=DSQRT(YS)
                CASE(2)
                    FS=Y(M1)-R
                CASE(3)
                    FS=YS/(Y(M1)-R)
            END SELECT
            R=R+FS
            IF(ST<=E)FS=0
        END IF
        PAR(1+Q)=BB/(1+0.1*K/ES)
        PAR(3+Q)=0.
        KK=K
        CRIT=FS/(1+DABS(R))
        CALL PRTNLP(KK,N,L,M,X,Y,P,ST,CRIT,SHAGP,PODRP,TEXT)
        IF(K>=D.OR.(DABS(CRIT))<=E)EXIT
    END DO
! KOHEñ ñàKãA èO K
    PZ=Y(M1)-RR
    TA=E*(1+DABS(R))*10
    IF(PZ>TA)THEN
        T=0.5/PZ
        IF(L>=1)THEN
            DO I=1,L
               P(I)=2*T*Y(I)
            END DO
        END IF
        L1=L+1
        IF(L1<=M)THEN
            DO I=L1,M
                YZ=Y(I)
                IF(YZ<0)THEN
                    P(I)=0
                ELSE
                    IF(YZ<R1)THEN
                        P(I)=T*3*K1*YZ*YZ
                    ELSE
                        P(I)=T*(2*YZ+K2)
                    END IF
                END IF
            END DO
        END IF
    END IF
    IF(PODRP>0.AND.CRIT<=E)TEXT=2
    IF(PODRP>0.AND.KK==D.AND.TEXT/=2)TEXT=3
    IF(PODRP/=0)WRITE(*,"(/32X,'OèTàMAãúHAü TOóKA')")
    CALL PRTNLP(KK,N,L,M,X,Y,P,ST,CRIT,1,4,TEXT)
    PAR(1+Q)=BB1
    RETURN
END SUBROUTINE CP3
!----------------------------------------------------------------------------------------------------------------------------------
FUNCTION FHCP3(X,F)
! éèàëÄçàÖ èÖêÖåÖççõï
    COMMON /C/    NF
    COMMON /A1/   M1,N,L
    COMMON /AGR1/ R,K1,K2,K3,R1,YA,PZ
    COMMON /AGR2/ Y
    INTEGER::I,M,NF
    REAL(8)::YZ,YH,PZ,R,K1,K2,K3,R1,YA, FHCP3
    REAL(8), DIMENSION(6)::Y
    REAL(8), DIMENSION(N)::X
!
    M=M1-1
    CALL F(X,Y,-1)
    PZ=Y(M1)-R
    YH=PZ**2
    DO I=1,M
        YZ=Y(I)
        IF(I<=L)THEN
            YH=YH+YZ**2
        ELSE
            IF(YZ>0)THEN
                IF(YZ<=R1)THEN
                    YH=YH+K1*YZ*YZ*YZ
                ELSE
                    YH=YH+YZ**2+K2*YZ+K3
                END IF
            END IF
        END IF
    END DO
    FHCP3 =YH
    NF=NF+1
    RETURN
END FUNCTION FHCP3
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE AGRCP3(N,FH,X1,YS,GR,N1,N2,HH,AC,F)
! éèàëÄçàÖ èÖêÖåÖççõï
    COMMON /A1/   M1,N11,L
    COMMON /AGR1/ R,K1,K2,K3,R1,YA, PZ__UNUSED
    COMMON /AGR2/ Y
    COMMON /AGR3/ GB
    INTEGER::N1,N2,AC,I,J,M
    REAL(8)::YS,HH,YZ,YH,R,R1,K1,K2,K3,YA,FH
    REAL(8), DIMENSION(6)::Y
    REAL(8), DIMENSION(3)::GB
    REAL(8), DIMENSION(N)::X1,GR
    EXTERNAL F
!
    M=M1-1
    CALL CGR(F,X1,Y,M,GR,0,N1,N2,HH,AC)
    DO I=N1,N2
        GR(I)=GR(I)*(Y(M1)-R)*2
    END DO
    DO I=1,M
        YZ=Y(I)
        IF(I<=L)THEN
            YH=YZ*2
        ELSE
            IF(YZ<=0)THEN
                YH=0
            ELSE
                IF(YZ<=R1)THEN
                    YH=3*K1*YZ*YZ
                ELSE
                    YH=2*YZ+K2
                END IF
            END IF
        END IF
        IF(DABS(YH)>YA)THEN
            CALL CGR(F,X1,Y,M,GB,I,N1,N2,HH,AC)
            DO J=N1,N2
                GR(J)=GR(J)+GB(J)*YH
            END DO
        END IF
    END DO
    RETURN
END SUBROUTINE AGRCP3
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE AGECP3(N,FH,X1,YS,FGR,GR,GS,N1,N2,HH,AC,F)
! éèàëÄçàÖ èÖêÖåÖççõï
    COMMON /A1/    M1,N11,L
    COMMON /AGR1/  R,K1,K2,K3,R1,YA, PZ__UNUSED
    COMMON /AGR2/  Y
    COMMON /AGES1/ G1
    COMMON /AGES2/ GB
    INTEGER::N1,N2,AC,I,J,S,L,M,N
    REAL(8)::HH,YZ,YH,YS,R,K1,YA,K2,R1,FH,K3
    REAL(8), DIMENSION(6)::Y
    REAL(8), DIMENSION(3)::GB,G1
    REAL(8), DIMENSION(N)::X1,GR
    REAL(8), DIMENSION(N,N)::GS
    EXTERNAL FGR,F
!
    M=M1-1
    CALL CGR(F,X1,Y,M,G1,0,N1,N2,HH,AC)
    CALL CGS(F,X1,Y,M,FGR,G1,GS,0,N1,N2,HH,AC)
    DO J=N1,N2
        DO S=N1,J
            GS(J,S)=GS(J,S)*2*(Y(M1)-R)+2*G1(J)*G1(S)
            GS(S,J)=GS(J,S)
        END DO
    END DO
    DO I=1,M
        YZ=Y(I)
        IF(I<=L)THEN
            YH=2
        ELSE
            IF(YZ<=0)THEN
                YH=0
            ELSE
                IF(YZ<=R1)THEN
                    YH=6*K1*YZ
                ELSE
                    YH=2
                END IF
            END IF
        END IF
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
        ELSE
            IF(YZ<=0)THEN
                YH=0
            ELSE
                IF(YZ<=R1)THEN
                    YH=3*K1*YZ*YZ
                ELSE
                    YH=2*YZ+K2
                END IF
            END IF
        END IF
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
END SUBROUTINE AGECP3
!----------------------------------------------------------------------------------------------------------------------------------
