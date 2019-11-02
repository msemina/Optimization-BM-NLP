SUBROUTINE CP6(N,L,M,X,A,B,P,F,CGR,CGS,YY,PAR,Q,UNCONS)
! éèàëÄçàÖ èÖêÖåÖççõï
    COMMON /A1/    M1
    COMMON /A10/   NF
    COMMON /A11/   G
    COMMON /CP60/  YYT
    COMMON /CP61/  LX
    COMMON /CP62/  HX
    COMMON /CP63/  GR
    COMMON /CP64/  FG
    COMMON /CP65/  U
    COMMON /CP66/  GU
    COMMON /CP67/  A1
    COMMON /CP68/  B1
    COMMON /CP69/  XX
    COMMON /CPFI1/ AL,PI,VAR
    COMMON /CPFI2/ AA
    COMMON /CPFI3/ BB
    INTEGER::N,L,M,I,J,K,KK,D,SHAGP,PODRP,ACC,VAR,TEXT,Q,L1,M1,NF
    REAL(8)::FS,H,YU,SG,C,E,E1,ST,SB,BHT,AL,PI,EW,CRIT,FHCP6,EBM,DFI
    REAL(8), DIMENSION(3)::AA,BB,XX,GR,HX,G,LX
    REAL(8), DIMENSION(5)::A1,B1,U,GU
    REAL(8), DIMENSION(6)::YYT
    REAL(8), DIMENSION(18)::FG
    REAL(8), DIMENSION(40)::PAR
    REAL(8), DIMENSION(N)::X,A,B
    REAL(8), DIMENSION(M)::P
    REAL(8), DIMENSION(M1)::YY
    EXTERNAL F,FHCP6,AGRCP6,AGES
! 
    E=PAR(1)
    KK=PAR(3)
    D=PAR(2)+KK
    VAR=PAR(4)
    SG=PAR(5)
    SB=PAR(6)
    H=PAR(7)
    ACC=PAR(8)
    SHAGP=PAR(9)
    PODRP=PAR(10)
    EBM=PAR(1+Q)
    EW=1.D-10
    TEXT=0
    NF=0
    CRIT=-1.0
    E1=PAR(1+Q)
    E1=E1*2*(1+KK*KK)
    IF(KK>1) E1=E1/2
    PI=3.1415926535
    AL=SG
    C=SG
    CALL F(X,YY,-1)
    ST=0
    DO I=1,M
        IF(I<=L)THEN
            ST=ST+DABS(YY(I))
        ELSE
            IF(YY(I)>=0)ST=ST+YY(I)
        END IF
    END DO
    IF(PODRP/=0)THEN
        WRITE(*,"(/A/)")    '     èPüMOâ METOÑ MOÑàîàñàPOBAHHOâ îìHKñàà ãAÉPAHÜA: CP6'
        WRITE(*,"(A,D12.5)")'     TOóHOCTú PEòEHàü áAÑAóà Hãè     E=  ',E
        WRITE(*,"(A,I5)")   '     óàCãO àTEPAñàâ                  D=  ',D
        WRITE(*,"(A,I5)")   '     BEPCàü METOÑA                   VAR=',VAR
        WRITE(*,"(A,D12.5)")'     èAPAMETP PEÉìãüPàáAñàà          SG= ',SG
        WRITE(*,"(A,D12.5)")'     BEC OÉPAHàóEHàâ                 SB= ',SB
        WRITE(*,"(A,D12.5)")'     òAÉ ÑàîîEPEHñàPOBAHàü           H=  ',H
        WRITE(*,"(A,I5)")   '     èOPüÑOK ÑàîîEPEHñàPOBAHàü       ACC=',ACC
    END IF
    DO I=1,M
        A1(I)=-1.D+15
        B1(I)=1.D+15
        IF(KK==0.AND.I<L+1) P(I)=1.
        IF(KK==0.AND.I>=L+1) P(I)=0.5
    END DO
    CALL PRTNLP(KK,N,L,M,X,YY,P,ST,CRIT,1,4,TEXT)
    DO I=1,N
        IF(X(I)<A(I)-EW.OR.X(I)>B(I)+EW)THEN
            WRITE(*,"(20X,'HAóAãúHAü TOóKA BHE èAPAããEãEèàèEÑA')")
            RETURN
        END IF
    END DO
    L1=L+1
    IF(L1<=M)THEN
        DO I=L1,M
            IF(P(I)<-EW)THEN
                WRITE(*,"(20X,'ÑBOâCTBEHHõâ BEKTOP HEÑOèìCTàM')")
                RETURN
            END IF
        END DO
    END IF
! èOÑÉOTOBKA PAÅOóàX MACCàBOB Ñãü è/è  FI , DFI
    DO I=1,N
        AA(I)=A(I)
        BB(I)=B(I)
    END DO
! 
    IF(L>=1)THEN
        DO I=1,L
            U(I)=P(I)
        END DO
    END IF
    IF(L1<=M)THEN
        DO I=L1,M
            U(I)=DSQRT(2*P(I))
            IF(DABS(U(I))<0.01)U(I)=0.1
        END DO
    END IF
! HAóAãO OCHOBHOÉO ñàKãA
    DO
        K=KK+1
        IF(.NOT.(K<(D+1).AND.DABS(CRIT)>E))EXIT
        FS=YY(M1)
        DO J=1,M1
            CALL CGR(F,X,YY,M,GR,J-1,1,N,H,ACC)
            DO I=1,N
                FG((J-1)*N+I)=GR(I)
            END DO
        END DO
        BHT=E1/(1+K*K)
        PAR(1+Q)=BHT
        PAR(3+Q)=0
        IF(L1<=M)THEN
            DO I=L1,M
                IF(DABS(U(I))<0.01) U(I)=0.1
            END DO
        END IF
        ! èOÑÉOTOBKA PAÅOóàX MACCàBOB Ñãü è/è FHCP6, AGRCP6
        DO I=1,N
            XX(I)=X(I)
        END DO
        DO I=1,M1
            YYT(I)=YY(I)
        END DO
        !
        CALL UNCONS(M,U,A1,B1,FHCP6,AGRCP6,AGES,YU,G,Q,PAR,F)
        CALL GRL(U,LX)
        DO I=1,N
            HX(I)=DFI(LX(I),X(I),I)
        END DO
        DO I=1,N
            X(I)=X(I)-C*HX(I)
        END DO
        CALL F(X,YY,-1)
        ST=0
        DO I=1,M
            IF(I<=L)THEN
                ST=ST+DABS(YY(I))
            ELSE
                IF(YY(I)>=0)ST=ST+YY(I)
            END IF
        END DO
        IF(L>=1)THEN
            DO I=1,L
                P(I)=U(I)
            END DO
        END IF
        IF(L1<=M)THEN
            DO I=L1,M
                P(I)=U(I)*U(I)/2.
            END DO
        END IF
        KK=K
        CRIT=DABS(FS-YY(M1))/(1+DABS(FS))+ST*SB
        CALL PRTNLP(KK,N,L,M,X,YY,P,ST,CRIT,SHAGP,PODRP,TEXT)
    END DO
! KOHEñ OCHOBHOÉO ñàKãA
	IF(PODRP>0.AND.CRIT<=E)TEXT=2
    IF(PODRP>0.AND.KK==D.AND.TEXT/=2)TEXT=3
	IF(PODRP/=0)WRITE(*,"(/32X,'OèTàMAãúHAü TOóKA')")
    CALL PRTNLP(KK,N,L,M,X,YY,P,ST,CRIT,1,4,TEXT)
    PAR(1+Q)=EBM
    RETURN
END SUBROUTINE CP6
!----------------------------------------------------------------------------------------------------------------------------------
FUNCTION FI(T,S,J)
! éèàëÄçàÖ èÖêÖåÖççõï
    COMMON /CPFI1/ AL,PI,VAR
    COMMON /CPFI2/ A
    COMMON /CPFI3/ B
    INTEGER::J,VAR
    REAL(8)::T,S,T1,T2,T3,T4,AL,PI, FI
    REAL(8), DIMENSION(2)::A,B
! 
    T3=(S-A(J))/AL
    T4=(S-B(J))/AL
    SELECT CASE(VAR)
        CASE(1)
            IF(T>=T4.AND.T<=T3)FI=T*T/2
            IF(T>T3)FI=T3*(T-0.5*T3)
            IF(T<T4)FI=T4*(T-0.5*T4)
        CASE(2)
            T1=1.D+15
            IF(T3<PI/2)T1=DSIN(T3)/DCOS(T3)
            T2=-1.D+15
            IF(T4>-PI/2)T2=DSIN(T4)/DCOS(T4)
            IF(T>=T2.AND.T<=T1) FI=T*DATAN(T)-0.5*DLOG(1+T*T)
            IF(T>T1)FI=T1*DATAN(T1)-0.5*DLOG(1+T1*T1)+T3*T-T3*T1
            IF(T<T2)FI=T2*DATAN(T2)-0.5*DLOG(1+T2*T2)+T4*T-T4*T2
        CASE(3)
            T1=1.D+18
            IF(T3<1.D+8)T1=(3*T3+DSQRT(8+9*T3*T3))**(1/3.)-(-3*T3+DSQRT(8+9*T3*T3))**(1/3.)
            T2=-1.D+18
            IF(T4>-1.D+8)T2=(3*T4+DSQRT(8+9*T4*T4))**(1/3.)-(-3*T4+DSQRT(8+9*T4*T4))**(1/3.)
            IF(T>=T2.AND.T<=T1) FI=1+T*T/2+T*T*T*T/24.
            IF(T>T1)FI=T3*T-T3*T1+1+T1*T1/2+T1*T1*T1*T1/24.
            IF(T<T2)FI=T4*T-T4*T2+1+T2*T2/2+T2*T2*T2*T2/24.
    END SELECT
    RETURN
END FUNCTION FI
!----------------------------------------------------------------------------------------------------------------------------------
FUNCTION DFI(T,S,J)
! éèàëÄçàÖ èÖêÖåÖççõï
    COMMON /CPFI1/ AL,PI,VAR
    COMMON /CPFI2/ A
    COMMON /CPFI3/ B
    INTEGER::J,VAR
    REAL(8)::T,S,T1,T2,T3,T4,AL,PI, DFI
    REAL(8), DIMENSION(2)::A,B
! 
    T3=(S-A(J))/AL
    T4=(S-B(J))/AL
    SELECT CASE(VAR)
        CASE(1)
            IF(T<=T3.AND.T>=T4)DFI=T
            IF(T>T3)DFI=T3
            IF(T<T4)DFI=T4
        CASE(2)
            T1=1.D+15
            IF(T3<PI/2)T1=DSIN(T3)/DCOS(T3)
            T2=-1.D+15
            IF(T4>-PI/2)T2=DSIN(T4)/DCOS(T4)
            IF(T>=T2.AND.T<=T1)DFI=DATAN(T)
            IF(T>T1)DFI=T3
            IF(T<T2)DFI=T4
        CASE(3)
            T1=1.D+18
            IF(T3<1.D+8)T1=(3*T3+DSQRT(8+9*T3*T3))**(1/3.)-(-3*T3+DSQRT(8+9*T3*T3))**(1/3.)
            T2=-1.D+18
            IF(T4>-1.D+8)T2=(3*T4+DSQRT(8+9*T4*T4))**(1/3.)-(-3*T4+DSQRT(8+9*T4*T4))**(1/3.)
            IF(T>=T2.AND.T<=T1)DFI=T+T*T*T/6.
            IF(T>T1)DFI=T3
            IF(T<T2)DFI=T4
    END SELECT
    RETURN
END FUNCTION DFI
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE GRL(U,LX)
! éèàëÄçàÖ èÖêÖåÖççõï
    COMMON /A1/   M1,N,L
    COMMON /CP64/ FG
    REAL(8)::Q
    INTEGER::I,J,N,L,M1,M
    REAL(8), DIMENSION(M1-1)::U
    REAL(8), DIMENSION(N)::LX
    REAL(8), DIMENSION(18)::FG
! 
    M=M1-1
    DO I=1,N
        Q=FG(I)
        DO J=1,M
            IF(J<L+1)THEN
                Q=Q+FG(J*N+I)*U(J)
            ELSE
                Q=Q+FG(J*N+I)*U(J)*(U(J)/2.)
            END IF
        END DO
        LX(I)=Q
    END DO
   RETURN
END SUBROUTINE GRL
!----------------------------------------------------------------------------------------------------------------------------------
FUNCTION FHCP6(U1,F)
! éèàëÄçàÖ èÖêÖåÖççõï
    COMMON /C/     NF
    COMMON /A1/    M1,N,L
    COMMON /CP60/  YY
    COMMON /CP61/  LX
    COMMON /CP69/  X
    COMMON /CPFI1/ AL
    INTEGER::I,M1,N,L,M
    REAL(8)::F,AL,YH,FI, FHCP6
    REAL(8),DIMENSION(M1-1)::U1
    REAL(8),DIMENSION(2)::LX,X
    REAL(8),DIMENSION(6)::YY
! 
    M=M1-1
    CALL GRL(U1,LX)
    YH=0
    DO I=1,N
        YH=YH+FI(LX(I),X(I),I)
    END DO
    YH=YH*AL-YY(M1)
    DO I=1,M
        IF(I<L+1)THEN
            YH=YH-U1(I)*YY(I)
        ELSE
            YH=YH-U1(I)*YY(I)*(U1(I)/2.)
        END IF
    END DO
    FHCP6=YH
    NF=NF+1
    RETURN
END FUNCTION FHCP6
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE AGRCP6(N,FHCP6,U1,YS,GU1,M1,M2,HH,ACC,F)
! éèàëÄçàÖ èÖêÖåÖççõï
    COMMON /A1/    M11,N1,L
    COMMON /CPFI1/ AL
    COMMON /CP64/  FG
    COMMON /CP69/  X
    COMMON /CP60/  YY
    COMMON /AGR1/  LX1
    INTEGER N,M1,M2,ACC,I,J,M11,N1,L,M
    REAL(8)::FHCP6,YS,HH,F,AL,YZ,DFI
    REAL(8), DIMENSION(2)::X,LX1
    REAL(8), DIMENSION(18)::FG
    REAL(8), DIMENSION(6)::YY
    REAL(8), DIMENSION(N)::U1,GU1
! 
    M=M11-1
    CALL GRL(U1,LX1)
    DO I=1,M
        YZ=0
        DO J=1,N1
            YZ=YZ+FG(I*N1+J)*DFI(LX1(J),X(J),J)
        END DO
        YZ=YZ*AL-YY(I)
        IF(I<=L)THEN
            GU1(I)=YZ
        ELSE
            GU1(I)=YZ*U1(I)
        END IF
    END DO
    RETURN
END SUBROUTINE AGRCP6
!----------------------------------------------------------------------------------------------------------------------------------
