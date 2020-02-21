SUBROUTINE A9(N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
! Žˆ‘€ˆ… ……Œ…›•
    COMMON /C/   NF
    COMMON /A91/ BG
    COMMON /A92/ G
    COMMON /A93/ G2
    COMMON /A94/ GN
    COMMON /A95/ XN
    COMMON /A96/ X1
    COMMON /SU3/ CS,EL,EG,EM,EC
    REAL(8)::CS,EL,EG,EM,EC,B1,B2,B3,U,V,C1,EX,NORMA,NG,C,H,E,S,F,Y
    REAL(8),DIMENSION(1)::BG,G,G2,GN,XN,X1
    REAL(8),DIMENSION(40)::PAR
    REAL(8),DIMENSION(N)::X,A,B,G1
    INTEGER::D,KK,SHAGP,PODRP,NS,AC,Q
    LOGICAL::GO
    EXTERNAL F,FNLP
! 
    E=PAR(1+Q)
    D=PAR(2+Q)
    KK=PAR(3+Q)
    C=PAR(4+Q)
    EC=PAR(5+Q)
    S=PAR(6+Q)
    NS=PAR(7+Q)
    AC=PAR(8+Q)
    H=PAR(9+Q)
    SHAGP=PAR(10+Q)
    PODRP=PAR(11+Q)
    NN=N
    EM=1.D-11
    EL=1.D-16
    EG=1.D15
    K1=0
    KD=2*(N+1)
    N1=1
    N2=N
    DO I=1,N
        XN(I)=X(I)
        X1(I)=X(I)
    END DO
    NF=0
    Y=F(XN,FNLP)
    CALL GRAD(N,F,X,Y,G1,N1,N2,H,AC,FNLP)
    DO I=1,N
        GN(I)=G1(I)
    END DO
    NG=NORMA(GN,N)
! E—ATœ ˆCXO„H›X „AHH›X
    IF(PODRP/=0)THEN
        WRITE(*,"(/5X,'MˆHˆMˆ‡A–ˆŸ METO„OM ‹‹ˆCOˆ„OB. BEPCˆŸ ',I1)")NS
        WRITE(*,"(/5X,'TO—HOCTœ PE˜EHˆŸ ‡A„A—ˆ',17X,'E=',D11.4)")E
        WRITE(*,"(/5X,'—ˆC‹O ˆTEPA–ˆ‰',26X,'D=',I4)")D
        WRITE(*,"(/5X,'PA„ˆ“C HA—A‹œHOƒO ˜APA',18X,'R=',D11.4)")C
        WRITE(*,"(/5X,'TO—HOCTœ O„HOMEPHO‰ MˆHˆMˆ‡A–ˆˆ',9X,'EC=',D11.4)")EC
        WRITE(*,"(/5X,'MˆHˆMA‹œH›‰ O''EM ‹‹ˆCOˆ„A',12X,'S=',D11.4)")S
        WRITE(*,"(/5X,'OPŸ„OK „ˆ””EPEH–ˆPOBAHˆŸ',15X,'P=',I1)")AC
        WRITE(*,"(/5X,'˜Aƒ „ˆ””EPEH–ˆPOBAHˆŸ',19X,'H=',D11.4)")H
    END IF
    IF(D/=0)THEN
        CALL PRTUCM(0,NF,N,X,Y,NG,SHAGP,PODRP)
        B1=DSQRT(DFLOAT(N-1))/DSQRT(DFLOAT(N+1))-1
        B2=N/DSQRT(DFLOAT(N*N-1))
        GO=.FALSE.
        B3=C
        V=1
        DO I=1,N
            DO J=1,N
                BG((J-1)*N+I)=0.D0
                IF (I==J) BG((J-1)*N+I)=1.D0
            END DO
        END DO
        K=KK+1
        ! HA—A‹O OCHOBHOƒO –ˆK‹A
        DO
            K1=K1+1
            IF (GO) THEN
                IF (NG>EM) C1=C/NG
                IF (NG<=EM) C1=C/EM
                CALL S3A9 (C1,F,N,Y,G1,X,X1,FNLP)
                DO I=1,N
                    X(I)=X(I)-CS*G1(I)
                    XN(I)=X(I)
                END DO
                CALL GRAD(N,F,X,Y,G1,N1,N2,H,AC,FNLP)
                DO I=1,N
                    GN(I)=G1(I)
                END DO
                NG=NORMA(GN,N)
                IF (NG<E) EXIT
                IF (K==D) EXIT
                DO I=1,N
                    DO J=1,N
                        BG((J-1)*N+I)=0.D0
                        IF (I==J) BG((J-1)*N+I)=1.D0
                    END DO
                END DO
                CALL PRTUCM(K,NF,N,X,Y,NG,SHAGP,PODRP)
                GO=.FALSE.
                K=K+1
                KK=K
                K1=0
            END DO
            ! B›—ˆC‹EHˆE BEKTOPA PACTŸ†EHˆŸ
            DO I=1,N
                G2(I)=0.D0
                DO J=1,N
                    G2(I)=G2(I)+BG((I-1)*N+J)*GN(J)
                END DO
            END DO
            U=NORMA (G2,N)
            IF (U<EL) U=EM
            DO I=1,N
                G(I)=-G2(I)/U
            END DO
            ! OPE„E‹EHˆE HOBOƒO –EHTPA
            DO I=1,N
                G2(I)=0.D0
                DO J=1,N
                    G2(I)=G2(I)+BG((J-1)*N+I)*G(J)
                END DO
                XN(I)=XN(I)+B3*G2(I)/(N+1)
            END DO
            ! ‡A„AHˆE HOBO‰ MATPˆ–›
            DO I=1,N
                U=0.D0
                DO J=1,N
                    U=U+BG((J-1)*N+I)*G(J)
                END DO
                U=U*B1
                DO J=1,N
                    BG((J-1)*N+I)=BG((J-1)*N+I)+G(J)*U
                END DO
            END DO
    ! POBEPKA B›O‹HEHˆŸ KPˆTEPˆEB ˆ E—ATœ
            U=F(XN,FNLP)
            CALL GRAD(N,F,XN,U,GN,1,N,H,AC,FNLP)
            IF (U<Y) THEN
                DO I=1,N
                    G1(I)=GN(I)
                    X(I)=XN(I)
                END DO
                NG=NORMA(GN,N)
                IF (NG<E) EXIT
                Y=U
                K1=0
            END IF
            B3=B3*B2
            V=(B1+1.D0)*V
            IF (V<S) GO=.TRUE.
            IF (GO) THEN
                IF (NS==1) EXIT
                IF (NS/=1) CYCLE
            END IF
            IF (K1==KD) K1=0
            IF (K1==0) THEN
                IF (K==D) EXIT
                CALL PRTUCM(K,NF,N,X,Y,NG,SHAGP,PODRP)
                K=K+1
                KK=K
            END IF
        END DO
    END IF
! KOHE– OCHOBHOƒO –ˆK‹A
    IF (V<S) WRITE(*,"(15X,'OœEM ‹‹ˆCOˆ„A MA‹')")
    IF(PODRP/=0)THEN
        IF (PODRP>0.AND.NG<E) WRITE(*,"(15X,'„OCTˆƒH“TA ‡A„AHHAŸ TO—HOCTœ')")
        IF (PODRP>0.AND.KK==D) WRITE(*,"(15X,'B›O‹HEHO ‡A„AHHOE —ˆC‹O ˜AƒOB')")
    END IF
    CALL PRTUCM(KK,NF,N,X,Y,NG,SHAGP,3)
    PAR(3+Q)=KK
    RETURN
END SUBROUTINE A9
!-----------------------------------------------------------------------------------------------------------------------
REAL(8) FUNCTION NORMA(G,NN)
    REAL(8),DIMENSION(NN)::G
    REAL(8)::P
    P=0.D0
    DO I=1,NN
        P=P+G(I)*G(I)
    END DO
    NORMA=DSQRT(P)
    RETURN
END FUNCTION NORMA
!-----------------------------------------------------------------------------------------------------------------------
REAL(8) FUNCTION F1A9(X2,X,X1,NN,F,G1,FNLP)
    REAL(8)::X2,F
    REAL(8),DIMENSION(NN)::X,X1,G1
    EXTERNAL FNLP,F
    DO I=1,NN
        X1(I)=X(I)-G1(I)*X2
    END DO
    F1A9=F(X1,FNLP)
    RETURN
END FUNCTION F1A9
!-----------------------------------------------------------------------------------------------------------------------
SUBROUTINE S3A9(C1,F,N,Y,G1,X,X1,FNLP)
! Žˆ‘€ˆ… ……Œ…›•
    COMMON /SU3/ CS,EL,EG,EM,EC
    REAL(8)::C1,F,Y,C,D,E,M,P,Q,R,TOL,T2,U,V,W,FU,FV,FW,FX,A1,B1,XS,CS,EL,EG,EM,EC,F1A9
    REAL(8),DIMENSION(N)::G1,X,X1
    EXTERNAL F,FNLP
! ŠŽ„ Ž„Žƒ€ŒŒ›
    CS=0.D0
    C=C1
    I1=0
    I2=0
    U=0
    FX=Y
    DO
        V=U+C
        FV=F1A9(V,X,X1,N,F,G1,FNLP)
        IF (FX>FV) THEN
            I1=1
            A1=U
            C=2*C
            U=V
            FX=FV
        ELSE
            I2=1
            C=C/2
            B1=V
        END IF
        IF (C<EL.OR.C>EG) THEN
            XS=V
            IF (Y>FX) THEN
                CS=XS
                Y=FX
            END IF
        END IF
        IF (.NOT.(I1+I2<1.5)) EXIT
    END DO
    C=(3-DSQRT(5.D0))*0.5D0
    V=A1+C*(B1-A1)
    W=V
    XS=V
    E=0
    FV=F1A9(XS,X,X1,N,F,G1,FNLP)
    FW=FV
    FX=FV
    DO
        M=0.5D0*(A1+B1)
        TOL=EM*DABS(XS)+EC
        T2=2.D0*TOL
        IF (DABS(XS-M)>T2-0.5D0*(B1-A1)) THEN
            P=0
            Q=0
            R=0
            IF (DABS(E)>TOL) THEN
                R=(XS-W)*(FX-FV)
                Q=(XS-V)*(FX-FW)
                P=(XS-V)*Q-(XS-W)*R
                Q=2*(Q-R)
                IF (Q>0) P=-P
                IF (Q<=0) Q=-Q
                R=E
                E=D
            END IF
            IF (DABS(P)<DABS(0.5D0*Q*R).AND.P>Q*(A1-XS).AND.P>Q*(A1-XS).AND.P<Q*(B1-XS)) THEN
                D=P/Q
                U=XS+D
                IF (U-A1<T2.OR.B1-U<T2) THEN
                    D=TOL
                    IF (XS>M) D=-TOL
                END IF
            ELSE
                E=A1-XS
                IF (XS<M) E=B1-XS
                D=C*E
            END IF
            IF (DABS(D)>=TOL) THEN
                U=XS+D
            ELSE
                IF (D>0) U=XS+TOL
                IF (D<=0) U=XS-TOL
            END IF
            FU=F1A9(U,X,X1,N,F,G1,FNLP)
            IF (FU<FX) THEN
                IF (U>=XS) A1=XS
                IF (U<XS) B1=XS
                V=W
                FV=FW
                W=XS
                FW=FX
                XS=U
                FX=FU
            ELSE
                IF (U>=XS) B1=U
                IF (U<XS) A1=U
                IF (FU<=FW.OR.W==XS) THEN
                    V=W
                    FV=FW
                    W=U
                    FW=FU
                ELSE
                    IF (FU<=FV.OR.V==XS.OR.V==W) THEN
                        V=U
                        FV=FU
                    END IF
                END IF
            END IF
        ELSE
            EXIT
        END IF
    END DO
    IF (Y>FX) THEN
        CS=XS
        Y=FX
    END IF
    RETURN
END SUBROUTINE S3A9
