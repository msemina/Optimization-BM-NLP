      SUBROUTINE A9(N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
      COMMON/A91/ BG
     &      /A92/ G
     &      /A93/ G2
     &      /A94/ GN
     &      /A95/ XN
     &      /A96/ X1
     &      /C/ NF
     &      /SU3/ CS,EL,EG,EM,EC
      REAL*8 X(N),A(N),B(N),F,Y,G1(N),PAR(40),
     &       BG(1),G(1),G2(1),GN(1),XN(1),X1(1),
     &       CS,EL,EG,EM,EC,B1,B2,B3,U,V,C1,EX,
     &       NORMA,NG,C,H,E,S
      INTEGER D,KK,SHAGP,PODRP,NS,AC,Q
      LOGICAL GO
      EXTERNAL F,FNLP
C
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
      DO 1 I=1,N
         XN(I)=X(I)
         X1(I)=X(I)
    1 CONTINUE
      NF=0
C-------------
      Y=F(XN,FNLP)
C-------------
      CALL GRAD(N,F,X,Y,G1,N1,N2,H,AC,FNLP)
C-------------
      DO 2 I=1,N
         GN(I)=G1(I)
    2 CONTINUE
      NG=NORMA(GN,N)
      IF(PODRP.EQ.0) GO TO 502
      PRINT 500,NS
  500 FORMAT(/5X,'MИHИMИЗAЦИЯ METOДOM ЭЛЛИПCOИДOB. BEPCИЯ ',I1)
      PRINT 501,E,D,C,EC,S,AC,H
  501    FORMAT(/5X,'TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ',17X,'E=',D11.4,
     &       /5X,'ЧИCЛO ИTEPAЦИЙ',26X,'D=',I4,
     &       /5X,'PAДИУC HAЧAЛЬHOГO ШAPA',18X,'R=',D11.4,
     &       /5X,'TOЧHOCTЬ OДHOMEPHOЙ MИHИMИЗAЦИИ',9X,'EC=',D11.4,
     &       /5X,'MИHИMAЛЬHЫЙ OБ''EM ЭЛЛИПCOИДA',12X,'S=',D11.4,
     &       /5X,'ПOPЯДOK ДИФФEPEHЦИPOBAHИЯ',15X,'P=',I1,
     &       /5X,'ШAГ ДИФФEPEHЦИPOBAHИЯ',19X,'H=',D11.4)
  502 IF (D.EQ.0) GO TO 10002
      CALL PRTUCM(0,NF,N,X,Y,NG,SHAGP,PODRP)
      B1=DSQRT(DFLOAT(N-1))/DSQRT(DFLOAT(N+1))-1
      B2=N/DSQRT(DFLOAT(N*N-1))
      GO=.FALSE.
      B3=C
      V=1
      DO 3 I=1,N
         DO 4 J=1,N
            BG((J-1)*N+I)=0.D0
            IF (I.EQ.J) BG((J-1)*N+I)=1.D0
    4    CONTINUE
    3 CONTINUE
      K=KK+1
C
C  H A Ч A Л O   O C H O B H O Г O   Ц И K Л A
C
10001 K1=K1+1
      IF (.NOT.GO) GO TO 5
          IF (NG.GT.EM) C1=C/NG
          IF (NG.LE.EM) C1=C/EM
C-------------
         CALL S3A9 (C1,F,N,Y,G1,X,X1,FNLP)
C-------------
         DO 6 I=1,N
            X(I)=X(I)-CS*G1(I)
            XN(I)=X(I)
    6    CONTINUE
C-------------
         CALL GRAD(N,F,X,Y,G1,N1,N2,H,AC,FNLP)
C-------------
         DO 7 I=1,N
            GN(I)=G1(I)
    7    CONTINUE
         NG=NORMA(GN,N)
         IF (NG.LT.E) GO TO 10002
         IF (K.EQ.D) GO TO 10002
         DO 8 I=1,N
            DO 9 J=1,N
               BG((J-1)*N+I)=0.D0
               IF (I.EQ.J) BG((J-1)*N+I)=1.D0
    9 CONTINUE
    8    CONTINUE
         CALL PRTUCM(K,NF,N,X,Y,NG,SHAGP,PODRP)
         GO=.FALSE.
         K=K+1
         KK=K
         K1=0
    5 CONTINUE
C
C         BЫЧИCЛEHИE BEKTOPA PACTЯЖEHИЯ
C
      DO 10 I=1,N
         G2(I)=0.D0
        DO 11 J=1,N
            G2(I)=G2(I)+BG((I-1)*N+J)*GN(J)
   11    CONTINUE
   10 CONTINUE
      U=NORMA (G2,N)
      IF (U.LT.EL) U=EM
      DO 12 I=1,N
         G(I)=-G2(I)/U
   12 CONTINUE
C
C         OПPEДEЛEHИE HOBOГO ЦEHTPA
C
      DO 13 I=1,N
         G2(I)=0.D0
         DO 14 J=1,N
            G2(I)=G2(I)+BG((J-1)*N+I)*G(J)
   14    CONTINUE
         XN(I)=XN(I)+B3*G2(I)/(N+1)
   13 CONTINUE
C
C         ЗAДAHИE HOBOЙ MATPИЦЫ
C
      DO 15 I=1,N
         U=0.D0
         DO 16 J=1,N
            U=U+BG((J-1)*N+I)*G(J)
   16    CONTINUE
         U=U*B1
         DO 17 J=1,N
            BG((J-1)*N+I)=BG((J-1)*N+I)+G(J)*U
   17    CONTINUE
   15 CONTINUE
C
C         ПPOBEPKA BЫПOЛHEHИЯ KPИTEPИEB И ПEЧATЬ
C
C-------------
      U=F(XN,FNLP)
C-------------
      CALL GRAD(N,F,XN,U,GN,1,N,H,AC,FNLP)
C-------------
      IF (.NOT.(U.LT.Y)) GO TO 18
         DO 19 I=1,N
            G1(I)=GN(I)
            X(I)=XN(I)
   19    CONTINUE
         NG=NORMA(GN,N)
         IF (NG.LT.E) GO TO 10002
         Y=U
         K1=0
   18 CONTINUE
      B3=B3*B2
      V=(B1+1.D0)*V
      IF (V.LT.S) GO=.TRUE.
      IF (.NOT.GO) GO TO 20
         IF (NS.EQ.1) GO TO 10002
         IF (NS.NE.1) GO TO 10001
   20 CONTINUE
      IF (K1.EQ.KD) K1=0
      IF (.NOT.(K1.EQ.0)) GO TO 21
         IF (K.EQ.D) GO TO 10002
         CALL PRTUCM(K,NF,N,X,Y,NG,SHAGP,PODRP)
         K=K+1
         KK=K
   21 CONTINUE
      GO TO 10001
C
C  K O H E Ц    O C H O B H O Г O   Ц И K Л A
C
10002 IF (V.LT.S) PRINT 200
  200 FORMAT(15X,'OБЬEM ЭЛЛИПCOИДA MAЛ')
      IF(PODRP.EQ.0) GO TO 10003
      IF (PODRP.GT.0.AND.NG.LT.E) PRINT 201
  201 FORMAT(15X,'ДOCTИГHУTA ЗAДAHHAЯ TOЧHOCTЬ')
      IF (PODRP.GT.0.AND.KK.EQ.D) PRINT 202
  202 FORMAT(15X,'BЫПOЛHEHO ЗAДAHHOE ЧИCЛO ШAГOB')
10003 CALL PRTUCM(KK,NF,N,X,Y,NG,SHAGP,3)
      PAR(3+Q)=KK
      RETURN
      END
C
C
C
      REAL*8 FUNCTION NORMA(G,NN)
      REAL*8 G(NN),P
      P=0.D0
      DO 1 I=1,NN
        P=P+G(I)*G(I)
    1 CONTINUE
      NORMA=DSQRT(P)
      RETURN
      END
C
C
      REAL*8 FUNCTION F1A9(X2,X,X1,NN,F,G1,FNLP)
      REAL*8 X2,X(NN),X1(NN),F,G1(NN)
      EXTERNAL FNLP,F
      DO 1 I=1,NN
        X1(I)=X(I)-G1(I)*X2
    1 CONTINUE
C-------------
      F1A9=F(X1,FNLP)
C-------------
      RETURN
      END
C
C
      SUBROUTINE S3A9 (C1,F,N,Y,G1,X,X1,FNLP)
      COMMON/SU3/CS,EL,EG,EM,EC
      REAL*8 C1,F,Y,G1(N),X(N),X1(N),
     &       C,D,E,M,P,Q,R,TOL,T2,U,V,W,FU,FV,FW,FX,A1,B1,
     &       XS,CS,EL,EG,EM,EC,F1A9
      EXTERNAL F,FNLP
      CS=0.D0
      C=C1
      I1=0
      I2=0
      U=0
      FX=Y
10031 V=U+C
C-------------
      FV=F1A9(V,X,X1,N,F,G1,FNLP)
C-------------
      IF (.NOT.(FX.GT.FV)) GO TO 1
        I1=1
        A1=U
        C=2*C
        U=V
        FX=FV
        GO TO 2
    1 CONTINUE
        I2=1
        C=C/2
        B1=V
    2 CONTINUE
      IF (.NOT.(C.LT.EL.OR.C.GT.EG)) GO TO 3
        XS=V
        GO TO 10032
    3 CONTINUE
      IF (I1+I2.LT.1.5) GO TO 10031
      C=(3-DSQRT(5.D0))*0.5D0
      V=A1+C*(B1-A1)
      W=V
      XS=V
      E=0
C-------------
      FV=F1A9(XS,X,X1,N,F,G1,FNLP)
C-------------
      FW=FV
      FX=FV
11111 M=0.5D0*(A1+B1)
      TOL=EM*DABS(XS)+EC
      T2=2.D0*TOL
      IF (.NOT.(DABS(XS-M).GT.T2-0.5D0*(B1-A1))) GO TO 4
        P=0
        Q=0
        R=0
        IF (.NOT.(DABS(E).GT.TOL)) GO TO 5
          R=(XS-W)*(FX-FV)
          Q=(XS-V)*(FX-FW)
          P=(XS-V)*Q-(XS-W)*R
          Q=2*(Q-R)
        IF (Q.GT.0) P=-P
        IF (Q.LE.0) Q=-Q
        R=E
        E=D
    5   CONTINUE
        IF (.NOT.(DABS(P).LT.DABS(0.5D0*Q*R).AND.P.GT.Q*(A1-XS).AND.
     &  P.GT.Q*(A1-XS).AND.P.LT.Q*(B1-XS))) GO TO 6
          D=P/Q
          U=XS+D
          IF (.NOT.(U-A1.LT.T2.OR.B1-U.LT.T2)) GO TO 7
            D=TOL
            IF (XS.GT.M) D=-TOL
    7     CONTINUE
          GO TO 8
    6   CONTINUE
          E=A1-XS
          IF (XS.LT.M) E=B1-XS
          D=C*E
    8   CONTINUE
        IF (.NOT.(DABS(D).GE.TOL)) GO TO 9
          U=XS+D
          GO TO 10
    9   CONTINUE
          IF (D.GT.0) U=XS+TOL
          IF (D.LE.0) U=XS-TOL
   10   CONTINUE
C-------------
         FU=F1A9(U,X,X1,N,F,G1,FNLP)
C-------------
        IF (.NOT.(FU.LT.FX)) GO TO 11
          IF (U.GE.XS) A1=XS
          IF (U.LT.XS) B1=XS
          V=W
          FV=FW
          W=XS
          FW=FX
          XS=U
          FX=FU
          GO TO 12
   11   CONTINUE
          IF (U.GE.XS) B1=U
          IF (U.LT.XS) A1=U
          IF (.NOT.(FU.LE.FW.OR.W.EQ.XS)) GO TO 13
            V=W
            FV=FW
            W=U
            FW=FU
            GO TO 14
   13     CONTINUE
          IF (.NOT.(FU.LE.FV.OR.V.EQ.XS.OR.V.EQ.W)) GO TO 15
            V=U
            FV=FU
   15     CONTINUE
   14   CONTINUE
   12   CONTINUE
        GO TO 11111
    4 CONTINUE
10032 IF (.NOT.(Y.GT.FX)) GO TO 16
        CS=XS
        Y=FX
   16 CONTINUE
      RETURN
      END
