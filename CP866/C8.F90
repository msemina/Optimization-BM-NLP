C
C          M  E  T  O  Ñ     H  ú  û  T  O  H  A
C
      SUBROUTINE C8(N,L,M,X,A,B,P,F,CGR,CGS,ZNFUNK,PAR,Q,UNCONS)
      COMMON/A1/M1 /A2/N1,N2,N3,N4,MACTIV /A3/SHAGD,AA
     &      /A4/ST /A5/FUNK /A10/NF
     &      /C81/ACTIV /C82/XPR /C83/GR /C84/HES /C85/LZ 
     &      /C86/LZZ /C87/NAPR /C88/DVOY /C89/DVPR 
     &      /C80/ZNFPR /C801/LRAB /C802/MRAB
      INTEGER N,L,M,N1,N2,N3,N4,MACTIV,K,KK,I,I1,I2,I3,J1,
     &        D,AA,AC,SHAGP,PODRP,TEXT,ACTIV(1),Q,L1,M1,N11,N21
      REAL*8 XPR(1),GR(1),HES(1),LZ(1),LZZ(1),NAPR(1),DVOY(1),
     &       DVPR(1),ZNFPR(1),LRAB(1),MRAB(1),FUNK(1),
     &       A(N),B(N),X(N),P(M),ZNFUNK(M1),PAR(40),E,MAJGOS,
     &       SLOY,NACHDV,MINDV,SHAGD,SHAGG,CONST1,CONST2,
     &       STEP,NORLZ,NORLZ1,ST,DET,CRIT
      LOGICAL POVTOR
      EXTERNAL F,CGR
C
C
C èPàCBOEHàE áHAóEHàâ KOHCTAHTAM,èAPAMETPAM METOÑA
C                à àX AHAãàá.
C
      CONST1=1.D-60
      CONST2=1.D-15
      E=PAR(1)
      KK=PAR(3)
      D=PAR(2)+KK
      MAJGOS=PAR(4)
      SLOY=PAR(5)
      NACHDV=PAR(6)
      MINDV=PAR(7)
      SHAGD=PAR(8)
      AA=PAR(9)
      SHAGG=PAR(10)
      AC=PAR(11)
      SHAGP=PAR(12)
      PODRP=PAR(13)
      NF=0
      N1=1
      N2=N
      N3=N2-N1+1
C
C  BõÑEãEHàE  AKTàBHõX  OÉPAHàóEHàâ
C
      POVTOR=.FALSE.
11111 CALL F(X,ZNFUNK,-1)
      MACTIV=L
      L1=L+1
      IF(L.EQ.0) GO TO 1111
      DO 1 I=1,L
         ACTIV(I)=I
 1    CONTINUE
 1111 IF(L1.GT.M) GO TO 3
      DO 2 I=L1,M
         IF(.NOT.(ZNFUNK(I).GT.-SLOY.OR.P(I).GT.MINDV)) GO TO 2
         MACTIV=MACTIV+1
         ACTIV(MACTIV)=I
         IF(P(I).LT.CONST1) P(I)=NACHDV
 2    CONTINUE
 3    N4=N3+MACTIV
C
C   OCHOBHõE ÑEâCTBàü METOÑA HúûTOHA
C   áAÑAHàE áHAóEHàâ  XPR,DVOY
C
      N11=N1-1
      IF(N11.EQ.0) GO TO 41
      DO 4 I=1,N11
         XPR(I)=X(I)
 4    CONTINUE
 41   N21=N2+1
      IF(N21.GT.N) GO TO 51
         DO 5 I=N21,N
            XPR(I)=X(I)
 5       CONTINUE
 51   IF(L.EQ.0) GO TO 61
      DO 6 I=1,L
         DVOY(I)=P(I)
 6    CONTINUE
 61   IF(L1.GT.MACTIV) GO TO 71
         DO 7 I=L1,MACTIV
            DVOY(I)=DSQRT(P(ACTIV(I)))
 7       CONTINUE
 71   CALL GRADC8(X,DVOY,ZNFUNK,NORLZ,F)
      DO 77 I=1,M1
         FUNK(I)=ZNFUNK(I)
 77   CONTINUE
      CALL SHTRAF
      CRIT=NORLZ
      TEXT=0
      IF(PODRP.EQ.0) GO TO 333
      PRINT 222
  222 FORMAT(/46X,'METOÑ HúûTOHA : C8')
      PRINT 555,E,D,MAJGOS,SLOY,SHAGD,AA,SHAGG,AC
  555 FORMAT(/5X,'TOóHOCTú PEòEHàü áAÑAóà Hãè',6X,'E=',D12.5,
     &       /5X,'óàCãO àTEPAñàâ',19X,'D=',I5,
     &       /5X,'MAÜOPAHTA ÉOãÑCTEâHA',13X,'MG=',D12.5,
     &       /5X,'èAPAMETP Ñãü BõÑEã.AKTàBHõX OÉP.',1X,'SLOY=',D12.5,
     &       /5X,'òAÉ ÑàîîEPEHñàPOBAHàü',12X,'SHAGD=',D12.5,
     &       /5X,'èOPüÑOK ÑàîîEPEHñàPOBAHàü',8X,'AA=',I5,
     &       /5X,'òAÉ BõóàCãEHàü ÉECCàAHA',10X,'SHAGG=',D12.5,
     &       /5X,'èOPüÑOK BõóàCãEHàü ÉECCàAHA',6X,'AC=',I5)
  333 IF(.NOT.(POVTOR))
     &               CALL PRTNLP(0,N,L,M,X,ZNFUNK,P,ST,CRIT,1,4,TEXT)
C
C  O C H O B H O â    ñ à K ã    è O    K
C
33333 K=KK+1
      IF(.NOT.(K.LT.D+1.AND.DABS(CRIT).GT.E)) GO TO 22222
C
C  BõóàCãEHàE à OÅPAôEHàE ÉECCàAHA LZZ
C
      IF(MACTIV.LT.1) GO TO 81
      DO 8 I=1,MACTIV
         DO 9 I1=1,MACTIV
            LZZ((N3+I1-1)*N4+N3+I)=0
 9       CONTINUE
 8    CONTINUE
 81   IF(L1.GT.MACTIV) GO TO 101
         DO 10 I=L1,MACTIV
            LZZ((N3+I-1)*N4+N3+I)=2*ZNFUNK(ACTIV(I))
 10      CONTINUE
 101  CALL CGS(F,X,ZNFUNK,M,CGR,GR,HES,0,N1,N2,SHAGG,AC)
      DO 11 I=1,N3
         DO 12 I1=1,N3
            LZZ((I1-1)*N4+I)=HES((N1-1+I1-1)*N3+(N1-1+I))
 12      CONTINUE
 11   CONTINUE
      IF(MACTIV.LT.1) GO TO 131
      DO 13 I=1,MACTIV
         I1=ACTIV(I)
         CALL CGS(F,X,ZNFUNK,M,CGR,GR,HES,I1,N1,N2,SHAGG,AC)
         DO 14 I2=1,N3
            DO 15 I3=1,N3
         LZZ((I3-1)*N4+I2)=LZZ((I3-1)*N4+I2)+
     &                     P(I1)*HES((N1-1+I3-1)*N3+(N1-1+I2))
  15        CONTINUE
 14      CONTINUE
 13   CONTINUE
 131  CALL DMINV(LZZ,N4,DET,LRAB,MRAB)
      IF(DET.NE.0) GO TO 161
         PRINT 162
 162     FORMAT(/10X,'ÉECCàAH BõPOÜÑEH')
         RETURN
C
C  BõóàCãEHàE  HúûTOHOBCKOÉO  HAèPABãEHàü
C
 161  DO 16 I=1,N4
         NAPR(I)=0
         DO 17 I1=1,N4
            NAPR(I)=NAPR(I)+LZZ((I1-1)*N4+I)*LZ(I1)
 17      CONTINUE
 16   CONTINUE
C
C  PEÉìãàPOBKA ÑãàHõ òAÉA BÑOãú HAèPABãEHàü
C
      STEP=1
 18   IF(N1.GT.N2) GO TO 201
      DO 20 I=N1,N2
         XPR(I)=X(I)-STEP*NAPR(I-N1+1)
 20   CONTINUE
 201  IF(MACTIV.LT.1) GO TO 211
      DO 21 I=1,MACTIV
         DVPR(I)=DVOY(I)-STEP*NAPR(I+N3)
 21   CONTINUE
 211  CALL F(XPR,ZNFPR,-1)
      CALL GRADC8(XPR,DVPR,ZNFPR,NORLZ1,F)
      IF(.NOT.(NORLZ1.GT.(1-STEP*MAJGOS)*NORLZ.AND.STEP.GT.1.D-9))
     &                    GO TO 19
      STEP=STEP/2
      GO TO 18
C
C  èPOBEPàM,OÅHìãàãACú ãà KAKAü-ãàÅO ÑBOâCTBEHHAü èEPEMEHHAü
C           èPà HAPìòEHHOM AKTàBHOM OÉPAHàóEHàà
C
 19   IF(L1.GT.MACTIV) GO TO 221
         DO 22 I=L1,MACTIV
            I1=ACTIV(I)
            IF(.NOT.(ZNFPR(I1).GT.0.AND.DVPR(I).LT.DSQRT(CONST1)))
     *          GO TO 22
            IF(P(I1).LT.NACHDV) GO TO 23
            P(I1)=10*P(I1)
            GO TO 25
 23         P(I1)=10*NACHDV
 25         PRINT 26
 26         FORMAT(/10X,'METOÑ CXOÑàTCü HE K PEòEHàû')
            RETURN
 22      CONTINUE
C
C  èPOBEPàM, ÑOÅABàãàCú ãà HOBõE AKTàBHõE OÉPAHàóEHàü
C
 221  IF(L.GT.MACTIV) GO TO 28
      L11=L
      IF(L.EQ.0) L11=1
      DO 27 I=L11,MACTIV
         IF(.NOT.(I.EQ.L11.AND.L.EQ.0)) GO TO 273
            I1=M
            IF(MACTIV.GT.0) I1=ACTIV(1)-1
            J1=1
            GO TO 274
 273     IF(I.EQ.MACTIV) GO TO 271
            I1=ACTIV(I+1)-1
            GO TO 272
 271     I1=M
 272     J1=ACTIV(I)+1
 274     IF(J1.GT.I1) GO TO 27
         DO 32 I2=J1,I1
            IF(.NOT.(ZNFPR(I2).GT.0)) GO TO 32
            P(I2)=2*MINDV
            POVTOR=.TRUE.
            GO TO 11111
 32      CONTINUE
 27   CONTINUE
C
C  èEPEKAóKA àá MACCàBOB: XPR,DVPR,ZNFPR,NORLZ1 B MACCàBõ:
C                 X,DVOY,P,ZNFUNK,NORLZ.
C
 28   DO 33 I=N1,N2
         X(I)=XPR(I)
 33   CONTINUE
      IF(L.EQ.0) GO TO 30
      DO 34 I=1,L
         P(I)=DVPR(I)
         DVOY(I)=DVPR(I)
 34   CONTINUE
 30   IF(L1.GT.MACTIV) GO TO 336
         DO 35 I=L1,MACTIV
            DVOY(I)=DVPR(I)
            P(ACTIV(I))=DVOY(I)**2
 35      CONTINUE
 336  DO 36 I=1,M1
         ZNFUNK(I)=ZNFPR(I)
 36   CONTINUE
      NORLZ=NORLZ1
      KK=K
      DO 37 I=1,M1
         FUNK(I)=ZNFUNK(I)
 37   CONTINUE
      CALL SHTRAF
      CRIT=NORLZ
      CALL PRTNLP(KK,N,L,M,X,ZNFUNK,P,ST,CRIT,SHAGP,PODRP,TEXT)
      GO TO 33333
C
C               K O H E ñ   ñ à K ã A   è O    K
C
22222 IF(PODRP.GT.0.AND.CRIT.LE.E) TEXT=2
      IF(PODRP.GT.0.AND.KK.EQ.D.AND.TEXT.NE.2) TEXT=3
      IF(PODRP.NE.0) PRINT 444
 444  FORMAT(/10X,'OèTàMAãúHAü TOóKA')
      CALL PRTNLP(KK,N,L,M,X,ZNFUNK,P,ST,CRIT,1,4,TEXT)
      RETURN
      END
C
C  BõóàCãEHàE ÉPAÑàEHTA LZ à EÉO HOPMõ,
C  óACTàóHOE áAèOãHEHàE MATPàñõ  LZZ .
C
      SUBROUTINE GRADC8(X,DVOY,ZNFUNK,NGR,F)
      COMMON/A1/M1,N,L /A2/N1,N2,N3,N4,MACTIV /A3/SHAGD,AA
     &      /C81/ACTIV /C83/GR /C85/LZ /C86/LZZ
      INTEGER I,I1,I2,M1,M,N,L,N1,N2,N3,N4,AA,MACTIV,ACTIV(1)
      REAL*8 X(N),DVOY(1),ZNFUNK(M1),NGR,F,GR(1),LZ(1),LZZ(1),SHAGD
      EXTERNAL F
C
      M=M1-1
      CALL CGR(F,X,ZNFUNK,M,GR,0,N1,N2,SHAGD,AA)
      DO 5 I2=1,N3
         LZ(I2)=GR(N1-1+I2)
    5 CONTINUE
      IF(MACTIV.LT.1) GO TO 11
      DO 1 I=1,MACTIV
         I1=ACTIV(I)
         CALL CGR(F,X,ZNFUNK,M,GR,I1,N1,N2,SHAGD,AA)
         IF(.NOT.(I.LE.L)) GO TO 6
            DO 7 I2=1,N3
               LZ(I2)=LZ(I2)+GR(N1-1+I2)*DVOY(I)
               LZZ((N3+I-1)*N4+I2)=GR(N1-1+I2)
               LZZ((I2-1)*N4+N3+I)=LZZ((N3+I-1)*N4+I2)
 7          CONTINUE
         LZ(N3+I)=ZNFUNK(I1)
         GO TO 1
 6       DO 8 I2=1,N3
            LZ(I2)=LZ(I2)+GR(N1-1+I2)*DVOY(I)**2
            LZZ((N3+I-1)*N4+I2)=2*GR(N1-1+I2)*DVOY(I)
            LZZ((I2-1)*N4+N3+I)=LZZ((N3+I-1)*N4+I2)
 8       CONTINUE
         LZ(N3+I)=2*DVOY(I)*ZNFUNK(I1)
 1    CONTINUE
 11   NGR=0
      DO 9 I=1,N4
         NGR=NGR+LZ(I)**2
 9    CONTINUE
      NGR=DSQRT(NGR)
      RETURN
      END
C
C  B õ ó à C ã E H à E    ò T P A î A
C
      SUBROUTINE SHTRAF
      COMMON/A1/M1,N,L /A4/ST /A5/ZNFUNK
      INTEGER M1,M,N,L,I,L1
      REAL*8 ST,ZNFUNK(1)
C
      M=M1-1
      ST=0
      IF(L.EQ.0) GO TO 111
      DO 11 I=1,L
         ST=ST+DABS(ZNFUNK(I))
 11   CONTINUE
 111  L1=L+1
      IF(L1.GT.M) GO TO 12
         DO 13 I=L1,M
            IF(ZNFUNK(I).GT.0) ST=ST+ZNFUNK(I)
 13      CONTINUE
 12   RETURN
      END
