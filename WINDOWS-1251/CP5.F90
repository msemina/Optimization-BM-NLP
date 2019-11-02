! ƒ¬Œ…—“¬≈ÕÕ€… Ã≈“Œƒ ÃŒƒ»‘»÷»–Œ¬¿ÕÕŒ… ‘”Õ ÷»» À¿√–¿Õ∆¿
SUBROUTINE CP5(N,L,M,X,A,B,P,F,CGR,CGS,Y,PAR,Q,UNCONS)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /A1/    M1
    COMMON /A10/   NF
    COMMON /A11/   G
    COMMON /AGR10/ R,YA,R1,E2
    COMMON /AGR20/ Y1
    COMMON /AGR30/ P1
    INTEGER::N,L,M,Q,NF,L1,M1,KKK,KK,D,I,K,SHAGP,PODRP,VAR,TEXT
    REAL(8)::E,FS,R,R1,YA,YH,YZ,YP,ST,SB,ES,E1,YS,BB,E2,EW,CRIT,EBM,FH1,FH2,FH3
    REAL(8), DIMENSION(3)::G
    REAL(8), DIMENSION(6)::Y1,P1
    REAL(8), DIMENSION(40)::PAR
    REAL(8), DIMENSION(N)::X,A,B
    REAL(8), DIMENSION(M)::P
    REAL(8), DIMENSION(M1)::Y
    EXTERNAL F,FH1,FH2,FH3,AGR1,AGR2,AGR3,AGES1,AGES2,AGES3
! 
    EBM=PAR(1+Q)
    E=PAR(1)
    KK=PAR(3)
    D=PAR(2)+KK
    VAR=PAR(4)
    SB=PAR(5)
    R=PAR(6)
    ES=PAR(7)
    R1=PAR(8)
    E2=R1
    SHAGP=PAR(9)
    PODRP=PAR(10)
    TEXT=0
    CRIT=-1.0
    EW=1.D-10
    BB=PAR(1+Q)
    YA=BB/100
    IF(YA>1.D-5) YA=1.D-5
    BB=BB*(1+0.1*KK/ES)
    NF=0
    CALL F(X,Y,-1)
    FS=Y(M1)
    ST=0
    DO I=1,M
        IF(I<=L)THEN
            ST=ST+DABS(Y(I))
        ELSE IF(Y(I)>0)THEN
            ST=ST+Y(I)
        END IF
    END DO
    IF(PODRP/=0)THEN
        WRITE(*,"(/A/)")    '     ƒBO…CTBEHH€… METOƒ MOƒ»‘»÷»POBAHHO… ‘”HK÷»» ÀA√PAH∆A: CP5'
        WRITE(*,"(A,D12.5)")'     TO◊HOCT‹ PEÿEH»ﬂ «AƒA◊» HÀœ     E=  ',E
        WRITE(*,"(A,I5)")   '     ◊»CÀO »TEPA÷»…                  D=  ',D
        WRITE(*,"(A,I5)")   '     BEPC»ﬂ METOƒA                   VAR=',VAR
        WRITE(*,"(A,D12.5)")'     BEC O√PAH»◊EH»…                 SB= ',SB
        WRITE(*,"(A,D12.5)")'     œAPAMETP PE√”ÀﬂP»«A÷»»          R=  ',R
        WRITE(*,"(A,D12.5)")'     œAPAMETP BCœOMO√. «AƒA◊»        ES= ',ES
        WRITE(*,"(A,D12.5)")'     BECOBO… KO›‘‘»÷»EHT             R1= ',R1
    END IF
    CALL PRTNLP(0,N,L,M,X,Y,P,ST,CRIT,1,4,TEXT)
    DO I=1,N
        IF(X(I)<A(I)-EW.OR.X(I)>B(I)+EW)THEN
            WRITE(*,"(/10X,'HA◊AÀ‹HAﬂ TO◊KA BHE œAPAÀÀEÀEœ»œEƒA')")
            IF(PODRP/=0)WRITE(*,"(/32X,'OœT»MAÀ‹HAﬂ TO◊KA')")
            PAR(1+Q)=EBM
            CALL PRTNLP(KK,N,L,M,X,Y,P,ST,CRIT,1,4,TEXT)
            RETURN
        END IF
    END DO
    L1=L+1
    IF(L1<=M)THEN
        DO I=L1,M
            IF(P(I)<-EW)THEN
                WRITE(*,"(/10X,'ƒBO…CTBEH€… BEKTOP HEƒOœ”CT»M')")
                IF(PODRP/=0)WRITE(*,"(/32X,'OœT»MAÀ‹HAﬂ TO◊KA')")
                PAR(1+Q)=EBM
                CALL PRTNLP(KK,N,L,M,X,Y,P,ST,CRIT,1,4,TEXT)
                RETURN
            END IF
        END DO
    END IF
! HA◊AÀO OCHOBHO√O ÷»KÀA
    DO
        K=KK+1
        IF(.NOT.(K<D+1.AND.DABS(CRIT)>E))THEN
            EXIT; CYCLE
        END IF
        E1=Y(M1)
        DO I=1,M
            P1(I)=P(I)
            Y1(I)=Y(I)
        END DO
        Y1(M1)=Y(M1)
        IF(VAR==1)CALL UNCONS(N,X,A,B,FH1,AGR1,AGES1,YS,G,Q,PAR,F)
        IF(VAR==2)CALL UNCONS(N,X,A,B,FH2,AGR2,AGES2,YS,G,Q,PAR,F)
        IF(VAR==3)CALL UNCONS(N,X,A,B,FH3,AGR3,AGES3,YS,G,Q,PAR,F)
        CALL F(X,Y,-1)
        KKK=PAR(3+Q)
        IF(KKK==0)THEN
            BB=BB*ES
        ELSE
            IF(VAR==1)YS=FH1(X,F)
            IF(VAR==2)YS=FH2(X,F)
            IF(VAR==3)YS=FH3(X,F)
            FS=E1
            ST=0
            IF(L>=1)THEN
                DO I=1,L
                    ST=ST+DABS(Y(I))
                END DO
            END IF
            L1=L+1
            IF(L1<=M)THEN
                DO I=L1,M
                    IF(Y(I)>=0)ST=ST+Y(I)
                END DO
            END IF
        END IF
        SELECT CASE(VAR)
            CASE(1)
                DO I=1,M
                    YZ=Y(I)*R
                    IF(I<=L)THEN
                        YH=P(I)+YZ
                    ELSE IF(P(I)+YZ>0)THEN
                        YH=P(I)+YZ
                    ELSE
                        YH=0.
                    END IF
                    P(I)=YH
                END DO
            CASE(2)
                DO I=1,M
                    YZ=Y(I)*R
                    IF(I<=L)THEN
                        YH=P(I)+YZ
                    ELSE IF(YZ<=0)THEN
                        YH=P(I)/(1-YZ)**2
                    ELSE
                        YH=P(I)*(1+2*YZ*(1+3*YZ/2))+YZ**3*4*R1
                    END IF
                    P(I)=YH
                END DO
            CASE(3)
                DO I=1,M
                    YZ=Y(I)*R
                    YP=YZ+P(I)
                    IF(I<=L)THEN
                        YH=P(I)+YZ
                    ELSE
                        IF(YP>0)THEN
                            YH=-E2*P(I)**3+YP**3*E2
                        ELSE
                            YH=-E2*P(I)**3
                        END IF
                        IF(YZ<0)THEN
                            YH=YH+P(I)/(1+YZ**2)
                        ELSE
                            YH=YH+P(I)
                        END IF
                    END IF 
                    P(I)=YH
                END DO
        END SELECT
        PAR(1+Q)=BB/(1+0.1*K/ES)
        PAR(3+Q)=0
        KK=K
        CRIT=DABS(FS-Y(M1))/(1+DABS(FS))+ST*SB
        CALL PRTNLP(KK,N,L,M,X,Y,P,ST,CRIT,SHAGP,PODRP,TEXT)
    END DO
! KOHE÷ OCHOBHO√O ÷»KÀA
    IF(PODRP>0.AND.CRIT<=E) TEXT=2
    IF(PODRP>0.AND.KK==D.AND.TEXT/=2) TEXT=3
    IF(PODRP/=0)WRITE(*,"(/32X,'OœT»MAÀ‹HAﬂ TO◊KA')")
    PAR(1+Q)=EBM
    CALL PRTNLP(KK,N,L,M,X,Y,P,ST,CRIT,1,4,TEXT)
    RETURN
END SUBROUTINE CP5
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE AGR1(N,FH,X1,YS,GR,N1,N2,HH,AC,F)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /A1/    M1,N11,L
    COMMON /AGR10/ R,YA
    COMMON /AGR20/ Y
    COMMON /AGR30/ P
    COMMON /AGR40/ GB
    INTEGER::N1,N2,N,AC,I,J,N11,M1,L,M
    REAL(8)::YS,HH,FH,YZ,YH,R,YA
    REAL(8), DIMENSION(3)::GB
    REAL(8), DIMENSION(5)::P
    REAL(8), DIMENSION(6)::Y
    REAL(8), DIMENSION(N)::X1,GR
    EXTERNAL F
! 
    M=M1-1
    CALL CGR(F,X1,Y,M,GR,0,N1,N2,HH,AC)
    DO I=1,M
        YZ=Y(I)*R
        IF(I<=L)THEN
            YH=P(I)+YZ
        ELSE IF(P(I)+YZ>0)THEN
            YH=P(I)+YZ
        ELSE
            YH=0.
        END IF
        IF(DABS(YH)>YA)THEN
            CALL CGR(F,X1,Y,M,GB,I,N1,N2,HH,AC)
            DO J=N1,N2
                GR(J)=GR(J)+GB(J)*YH
            END DO
        END IF
    END DO
    RETURN
END SUBROUTINE AGR1
!----------------------------------------------------------------------------------------------------------------------------------
FUNCTION FH1(X,F)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /A1/    M1,N,L
    COMMON /C/     NF
    COMMON /AGR10/ R
    COMMON /AGR20/ Y
    COMMON /AGR30/ P
    INTEGER::I,M1,N,L,M
    REAL(8)::YH,YZ,R, FH1
    REAL(8), DIMENSION(6)::Y
    REAL(8), DIMENSION(5)::P
    REAL(8), DIMENSION(N)::X
! 
    M=M1-1
    CALL F(X,Y,-1)
    YH=0.
    DO I=1,M
        YZ=Y(I)*R
        IF(I<=L)THEN
            YH=YH+YZ*P(I)+YZ**2/2
        ELSE IF(P(I)+YZ>0)THEN
            YH=YH+(P(I)+YZ)**2/2
        END IF
    END DO
    FH1=Y(M1)+YH/R
    NF=NF+1
    RETURN
END FUNCTION FH1
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE AGR2(N,FH,X1,YS,GR,N1,N2,HH,AC,F)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /A1/    M1,N11,L
    COMMON /AGR10/ R,YA,R1
    COMMON /AGR20/ Y
    COMMON /AGR30/ P
    COMMON /AGR40/ GB
    INTEGER::N,N1,N2,AC,I,J,M1,N11,L,M
    REAL(8)::HH,YS,FH,YZ,YH,R,YA,R1
    REAL(8), DIMENSION(6)::Y
    REAL(8), DIMENSION(5)::P
    REAL(8), DIMENSION(3)::GB
    REAL(8), DIMENSION(N)::X1,GR
    EXTERNAL F
! 
    M=M1-1
    CALL CGR(F,X1,Y,M,GR,0,N1,N2,HH,AC)
    DO I=1,M
        YZ=Y(I)*R
        IF(I<=L)THEN
            YH=P(I)+YZ
        ELSE IF(YZ<=0)THEN
            YH=P(I)/(1-YZ)**2
        ELSE
            YH=P(I)*(1+2*YZ*(1+3*YZ/2))+YZ**3*4*R1
        END IF
        IF(DABS(YH)>YA)THEN
            CALL CGR(F,X1,Y,M,GB,I,N1,N2,HH,AC)
            DO J=N1,N2
               GR(J)=GR(J)+GB(J)*YH
            END DO
        END IF
    END DO
    RETURN
END SUBROUTINE AGR2
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE AGES2(N,FH,X1,YS,FGR,GR,GS,N1,N2,HH,AC,F)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /A1/     M1,N11,L
    COMMON /AGR10/  R,YA,R1
    COMMON /AGR20/  Y
    COMMON /AGR30/  P
    COMMON /AGES10/ G1
    COMMON /AGES20/ GB
    INTEGER::N1,N2,AC,I,J,S,M1,N11,L,M
    REAL(8)::HH,YS,FH,YZ,YH,R,YA,R1
    REAL(8), DIMENSION(3)::G1,GB
    REAL(8), DIMENSION(6)::Y
    REAL(8), DIMENSION(5)::P
    REAL(8), DIMENSION(N)::X1,GR
    REAL(8), DIMENSION(N,N)::GS
    EXTERNAL F
! 
    M=M1-1
    IF(AC==3)CALL CGR(F,X1,Y,M,G1,0,N1,N2,HH,AC)
    CALL CGS(F,X1,Y,M,FGR,G1,GS,0,N1,N2,HH,AC)
    DO I=1,M
        YZ=Y(I)*R
        IF(I<=L)THEN
            YH=1
        ELSE IF(YZ<=0)THEN
            YH=2*P(I)/(1-YZ)**3
        ELSE
            YH=2*P(I)*(1+3*YZ)+12*YZ**2*R1
        END IF
        YH=YH*R
        IF(DABS(YH)>YA.OR.AC==3)THEN
            CALL CGR(F,X1,Y,M,G1,I,N1,N2,HH,AC)
            DO J=N1,N2
                DO S=N1,J
                    GS(S,J)=GS(J,S)+G1(J)*G1(S)*YH
                    GS(J,S)=GS(S,J)
                END DO
            END DO
        END IF
        IF(I<=L)THEN
            YH=P(I)+YZ
        ELSE IF(YZ<=0)THEN
            YH=P(I)/(1-YZ)**2
        ELSE
            YH=P(I)*(1+2*YZ*(1+3*YZ/2))+YZ**3*4*R1
        END IF
        IF(DABS(YH)>YA)THEN
            CALL CGS(F,X1,Y,M,FGR,G1,GB,I,N1,N2,HH,AC)
            DO J=N1,N2
                DO S=N1,J
                    GS(S,J)=GS(J,S)+GB((S-1)*N+J)*YH
                    GS(J,S)=GS(S,J)
                END DO
            END DO
        END IF
    END DO
    RETURN
END SUBROUTINE AGES2
!----------------------------------------------------------------------------------------------------------------------------------
FUNCTION FH2(X,F)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /C/     NF
    COMMON /A1/    M1,N,L
    COMMON /AGR10/ R,YA,R1
    COMMON /AGR20/ Y
    COMMON /AGR30/ P
    INTEGER::I,M1,N,L,M
    REAL(8)::YH,YZ,R,YA,R1, FH2
    REAL(8), DIMENSION(6)::Y
    REAL(8), DIMENSION(5)::P
    REAL(8), DIMENSION(N)::X
! 
    M=M1-1
    CALL F(X,Y,-1)
    YH=0
    DO I=1,M
        YZ=Y(I)*R
        IF(I<=L)THEN
            YH=YH+YZ*P(I)+YZ**2/2
        ELSE IF(YZ<=0)THEN
            YH=YH+P(I)/(1-YZ)
        ELSE
            YH=YH+P(I)*(1+YZ*(1+YZ*(1+YZ)))+YZ**4*R1
        END IF
    END DO
    FH2=Y(M1)+YH/R
    NF=NF+1
    RETURN
END FUNCTION FH2
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE AGR3(N,FH,X1,YS,GR,N1,N2,HH,AC,F)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /A1/    M1,N11,L
    COMMON /AGR10/ R,YA,R1,E2
    COMMON /AGR20/ Y
    COMMON /AGR30/ P
    COMMON /AGR40/ GB
    INTEGER::N,N1,N2,AC,I,J,M1,N11,L,M
    REAL(8)::FH,YS,HH,R,YA,R1,E2,YH,YZ,YP
    REAL(8), DIMENSION(3)::GB
    REAL(8), DIMENSION(6)::Y
    REAL(8), DIMENSION(5)::P
    REAL(8), DIMENSION(N)::X1,GR
    EXTERNAL F
! 
    M=M1-1
    CALL CGR(F,X1,Y,M,GR,0,N1,N2,HH,AC)
    DO I=1,M
        YZ=Y(I)*R
        YP=YZ+P(I)
        IF(I<=L)THEN
            YH=P(I)+YZ
        ELSE
            IF(YP>0)THEN
                YH=-E2*P(I)**3+YP**3*E2
            ELSE
                YH=-E2*P(I)**3
            END IF
            IF(YZ<0)THEN
                YH=YH+P(I)/(1+YZ**2)
            ELSE
                YH=YH+P(I)
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
END SUBROUTINE AGR3
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE AGES3(N,FH,X1,YS,FGR,GR,GS,N1,N2,HH,AC,F)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /A1/     M1,N11,L
    COMMON /AGR10/  R,YA,R1,E2
    COMMON /AGR20/  Y
    COMMON /AGR30/  P
    COMMON /AGES10/ G1
    COMMON /AGES20/ GB
    INTEGER N,N1,N2,AC,I,J,S,M1,N11,L,M
    REAL(8)::FH,YS,HH,YZ,YP,YH,R,YA,R1,E2
    REAL(8), DIMENSION(3)::G1,GB
    REAL(8), DIMENSION(6)::Y
    REAL(8), DIMENSION(5)::P
    REAL(8), DIMENSION(N)::X1,GR
    REAL(8), DIMENSION(N,N)::GS
    EXTERNAL F
! 
    M=M1-1
    IF(AC==3)CALL CGR(F,X1,Y,M,G1,0,N1,N2,HH,AC)
    CALL CGS(F,X1,Y,M,FGR,G1,GS,0,N1,N2,HH,AC)
    DO I=1,M
        YZ=Y(I)*R
        YP=YZ+P(I)
        IF(I<=L)THEN
            YH=1
        ELSE
            IF(YP>0)THEN
                YH=3*E2*YP**2
            ELSE
                YH=0
            END IF
            IF(YZ<0)THEN
                YH=YH+P(I)*(-2*YZ/(1+YZ**2)**2)
            END IF
        END IF
        YH=YH*R
        IF(DABS(YH)>YA.OR.AC==3)THEN
            CALL CGR(F,X1,Y,M,G1,I,N1,N2,HH,AC)
            DO J=N1,N2
                DO S=N1,J
                    GS(S,J)=GS(J,S)+G1(J)*G1(S)*YH
                    GS(J,S)=GS(S,J)
                END DO
            END DO
        END IF
        IF(I<=L)THEN
            YH=P(I)+YZ
        ELSE
            IF(YP>0)THEN
                YH=-E2*P(I)**3+YP**3*E2
            ELSE
                YH=-E2*P(I)**3
            END IF
            IF(YZ<0)THEN
                YH=YH+P(I)/(1+YZ**2)
            ELSE
                YH=YH+P(I)
            END IF
        END IF
        YH=YH*R
        IF(DABS(YH)>YA)THEN
            CALL CGS(F,X1,Y,M,FGR,G1,GB,I,N1,N2,HH,AC)
            DO J=N1,N2
                DO S=N1,J
                    GS(S,J)=GS(J,S)+GB((S-1)*N+J)*YH
                    GS(J,S)=GS(S,J)
                END DO
            END DO
        END IF
    END DO
    RETURN
END SUBROUTINE AGES3
!----------------------------------------------------------------------------------------------------------------------------------
FUNCTION FH3(X,F)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /A1/    M1,N,L
    COMMON /AGR10/ R,YA,R1,E2
    COMMON /AGR20/ Y
    COMMON /AGR30/ P
    COMMON /C/     NF
    INTEGER::I,M1,N,L,M
    REAL(8)::YH,YZ,YP,R,YA,R1,E2, FH3
    REAL(8), DIMENSION(6)::Y
    REAL(8), DIMENSION(5)::P
    REAL(8), DIMENSION(N)::X
! 
    M=M1-1
    CALL F(X,Y,-1)
    YH=0
    DO I=1,M
        YZ=Y(I)*R
        YP=YZ+P(I)
        IF(I<=L)THEN
            YH=YH+YZ*P(I)+YZ**2/2
        ELSE
            YH=YH-E2*P(I)**3*YZ
            IF(YP>0)THEN
                YH=YH+YP**4*E2/4
            ELSE IF(YZ<0)THEN
                YH=YH+P(I)*DATAN(YZ)
            ELSE
                YH=YH+P(I)*YZ
            END IF
        END IF
    END DO
    FH3=Y(M1)+YH/R
    NF=NF+1
    RETURN
END FUNCTION FH3
!----------------------------------------------------------------------------------------------------------------------------------

