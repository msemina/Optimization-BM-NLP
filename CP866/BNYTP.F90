SUBROUTINE BNYTP(N,L,M,X,A,B,P,F,CGR,CGS,Y,PAR,Q,UNCONS)
! Žˆ‘€ˆ… ……Œ…›•
    COMMON /A1/   M1,N11__UNUSED,L__UNUSED
    COMMON /A10/  NF
    COMMON /A11/  G
    COMMON /AGR1/ BB,R1,K1,K2,K3,YA,T
    COMMON /AGR2/ YFH
    INTEGER::D,I,K,SHAGP,PODRP,KK,KKK,TEXT,Q
    REAL(8)::E,YA,T,BB,R1,ST,EBM,K1,K2,K3,E1,YS,EW,CRIT,FH
    REAL(8),DIMENSION(3)::G
    REAL(8),DIMENSION(6)::YFH
    REAL(8),DIMENSION(N)::A,B,X
    REAL(8),DIMENSION(M)::P
    REAL(8),DIMENSION(M1)::Y
    REAL(8),DIMENSION(40)::PAR
    EXTERNAL FH,AGR,AGES,F
! ŠŽ„ Žƒ€ŒŒ›
    E=PAR(1)
    KK=PAR(3)
    D=PAR(2)+KK
    SHAGP=PAR(4)
    PODRP=PAR(5)
    EBM=PAR(1+Q)
    TEXT=0
    CRIT=-1.0
    NF=0
    EW=1.D-10
    E1=PAR(1+Q)
    YA=E1/100.
    IF(YA>1.D-5)YA=1.D-5
    IF(KK>0) E1=E1*KK
    R1=0.0001; K1=1./(3*R1); K2=-R1; K3=R1**2/3
    CALL F(X,Y,-1)
    ST=0
    DO I=1,M
        IF(I>L)THEN
            IF(Y(I)>0) ST=ST+Y(I)
        ELSE
            ST=ST+DABS(Y(I))
        END IF
    END DO
    CRIT=ST
    IF(PODRP/=0)THEN
        WRITE(*,"(/26X,'METO„ OT›CKAHˆŸ „O“CTˆM›X TO—EK: BHYTP')")  
        WRITE(*,"(/5X,'TO—HOCTœ PE˜EHˆŸ ‡A„A—ˆ H‹',5X,'E=',D12.5,/5X,'—ˆC‹O ˆTEPA–ˆ‰',18X,'D=',I5)")E,D
    END IF
    CALL PRTNLP(KK,N,L,M,X,Y,P,ST,CRIT,1,4,TEXT)
    DO I=1,N
        IF(X(I)<A(I)-EW.OR.X(I)>B(I)+EW)THEN
            WRITE(*,"(10X,'HA—A‹œHAŸ TO—KA BHE APA‹‹E‹EˆE„A')")
            RETURN
        END IF
    END DO
    IF(CRIT<E)THEN
        TEXT=2
        RETURN
    END IF
! HA—A‹O OCHOBHOƒO –ˆK‹A
    DO
        K=KK+1
        IF(.NOT.(K<D+1.AND.DABS(CRIT)>E))EXIT
        E1=E1/K; PAR(1+Q)=E1; BB=E*0.2/(1.+M)/K**2
        CALL UNCONS(N,X,A,B,FH,AGR,AGES,YS,G,Q,PAR,F) ! …‡‘Œ›‘‹…›‰ ‚›‡Ž‚ "“‘’Ž‰" Ž„Žƒ€ŒŒ›
        KKK=PAR(3+Q)
        IF(KKK==0)THEN
            E1=E1*0.2
            PAR(1+Q)=E1
        ELSE
            YS=FH(X,F)
            DO I=1,M1
                Y(I)=YFH(I)
            END DO
            ST=0
            IF(.NOT.(L<1))THEN
                DO I=1,L
                     ST=ST+DABS(Y(I))
                END DO
            END IF
            L1=L+1
            IF(.NOT.(L1>M))THEN
                DO I=L1,M
                    IF(Y(I)>0) ST=ST+Y(I)
                END DO
            END IF
        END IF
        KK=K
        CRIT=ST
        CALL PRTNLP (KK,N,L,M,X,Y,P,ST,CRIT,SHAGP,PODRP,TEXT)
    END DO
! KOHE– OCHOBHOƒO –ˆK‹A
    IF(PODRP>0.AND.CRIT<=E)TEXT=2
    IF(PODRP>0.AND.KK==D.AND.TEXT/=2)TEXT=3
    IF(PODRP/=0) WRITE(*,"(/25X,'OTˆMA‹œHAŸ TO—KA')")
    CALL PRTNLP (KK,N,L,M,X,Y,P,ST,CRIT,1,4,TEXT)
    PAR(1+Q)=EBM
    RETURN
END SUBROUTINE BNYTP
!------------------------------------------------------------------------------------------------------
SUBROUTINE AGR (N,FH,X1,YS,GR,N1,N2,HH,AC,F)
! Žˆ‘€ˆ… ……Œ…›•
    COMMON /A1/   M1,N11,L
    COMMON /AGR1/ BB,R1,K1,K2,K3,YA,T
    COMMON /AGR2/ Y
    COMMON /AGR3/ GB
    INTEGER::N1,N2,AC,M1,N,L,M
    REAL(8)::FH,YS,HH,YZ,YH,BB,R1,K1,K2,K3,YA,T
    REAL(8),DIMENSION(6)::Y
    REAL(8),DIMENSION(3)::GB
    REAL(8),DIMENSION(N)::X1,GR
    EXTERNAL F
! ŠŽ„ Žƒ€ŒŒ›
    M=M1-1
    DO I=N1,N2
        GR(I)=0
    END DO
    DO I=1,M
        YZ=Y(I)
        IF(I>L)YZ=YZ+BB
        IF(I<=L)THEN
            YH=YZ*2.
        ELSE IF(YZ<=0)THEN
            YH=0.
        ELSE IF(YZ<=R1)THEN
            YH=3.*K1*YZ*YZ
        ELSE
            YH=2.*YZ+K2
        END IF
        IF(DABS(YH)>YA)THEN
            CALL CGR (F,X1,Y,M,GB,I,N1,N2,HH,AC)
            DO J=N1,N2
                GR(J)=GR(J)+GB(J)*YH
            END DO
        END IF
    END DO
    RETURN
END SUBROUTINE AGR
!------------------------------------------------------------------------------------------------------
SUBROUTINE AGES (N,FH,X1,YS,FGR,GR,GS,N1,N2,HH,AC,F)
! Žˆ‘€ˆ… ……Œ…›•
    COMMON /A1/    M1,N11,L
    COMMON /AGR1/  BB,R1,K1,K2,K3,YA,T
    COMMON /AGR2/  Y
    COMMON /AGES1/ G1
    COMMON /AGES2/ GB
    REAL(8), DIMENSION(6)::Y
    REAL(8), DIMENSION(3)::G1,GB
    REAL(8), DIMENSION(N)::X1,GR
    REAL(8), DIMENSION(N,N)::GS
    INTEGER::N1,N2,AC,M1,N,L,M,S
    REAL(8)::BB,R1,K1,K2,K3,YA,T,YZ,YH,FH,YS,HH
    EXTERNAL F,FGR
! ŠŽ„ Žƒ€ŒŒ›
    M=M1-1
    DO I=N1,N2
        DO J=N1,N2
            GS(I,J)=0
        END DO
    END DO
    DO I=1,M
        YZ=Y(I)
        IF(I>L)YZ=YZ+BB
        IF(I<=L)THEN
            YH=2.
        ELSE IF(YZ<=0)THEN
            YH=0.
        ELSE IF(YZ<=R1)THEN
            YH=6.*K1*YZ
        ELSE
            YH=2.
        END IF
        YH=YH*T
        IF(DABS(YH)>YA)THEN
            CALL CGR (F,X1,Y,M,G1,I,N1,N2,HH,AC)
            DO J=N1,N2
                DO S=N1,J
                    GS(J,S)=GS(J,S)+G1(J)*G1(S)*YH
                    GS(S,J)=GS(J,S)
                END DO
            END DO
        END IF
        IF(I<=L)THEN
            YH=2.*YZ
        ELSE IF(YZ<=0)THEN
            YH=0.
        ELSE IF(YZ<=R1)THEN
            YH=3.*K1*YZ*YZ
        ELSE
            YH=2.*YZ+K2
        END IF
        IF(DABS(YH)>YA)THEN
            CALL CGS(F,X1,Y,M,FGR,GR,GB,I,N1,N2,HH,AC)
            DO J=N1,N2
                DO S=N1,J
                    GS(J,S)=GS(J,S)+GB((S-1)*N+J)*YH
                    GS(S,J)=GS(J,S)
                END DO
            END DO
        END IF
    END DO
    RETURN
END SUBROUTINE AGES
!------------------------------------------------------------------------------------------------------
FUNCTION FH(X,F)
! Žˆ‘€ˆ… ……Œ…›•
    COMMON /C/    NF
    COMMON /A1/   M1,N,L
    COMMON /AGR1/ BB,R1,K1,K2,K3,YA,T
    COMMON /AGR2/ Y
    REAL(8), DIMENSION(6)::Y
    REAL(8), DIMENSION(N)::X
    INTEGER::M1,M,N,L
    REAL(8)::BB,R1,K1,K2,K3,YA,T,YH,YZ, FH
! ŠŽ„ Ž„Žƒ€ŒŒ›
    M=M1-1
    CALL F(X,Y,-1)
    YH=0
    DO I=1,M
        YZ=Y(I)
        IF(I>L)YZ=YZ+BB
        IF(I<=L)THEN
            YH=YH+YZ**2
        ELSE IF(YZ<=0)THEN
            CYCLE
        ELSE IF(YZ<=R1)THEN
            YH=YH+K1*YZ*YZ*YZ
        ELSE
            YH=YH+YZ**2+K2*YZ+K3
        END IF
    END DO
    FH=YH
    NF=NF+1
    RETURN
END FUNCTION FH
