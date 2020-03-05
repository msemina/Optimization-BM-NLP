! METOÄ HÜÞTOHA
SUBROUTINE C8(N,L,M,X,A,B,P,F,CGR,CGS,ZNFUNK,PAR,Q,UNCONS)
    COMMON /A1/   M1
    COMMON /A2/   N1,N2,N3,N4,MACTIV
    COMMON /A3/   SHAGD,AA
    COMMON /A4/   ST
    COMMON /A5/   FUNK
    COMMON /A10/  NF
    COMMON /C80/  ZNFPR
    COMMON /C81/  ACTIV
    COMMON /C82/  XPR
    COMMON /C83/  GR
    COMMON /C84/  HES
    COMMON /C85/  LZ 
    COMMON /C86/  LZZ
    COMMON /C87/  NAPR
    COMMON /C88/  DVOY
    COMMON /C89/  DVPR 
    COMMON /C801/ LRAB
    COMMON /C802/ MRAB
    REAL(8)::E,MAJGOS,SLOY,NACHDV,MINDV,SHAGD,SHAGG,CONST1,CONST2,STEP,NORLZ,NORLZ1,ST,DET,CRIT
    REAL(8),DIMENSION(1)::XPR,GR,HES,LZ,LZZ,NAPR,DVOY,DVPR,ZNFPR,LRAB,MRAB,FUNK
    REAL(8),DIMENSION(40)::PAR(40)
    REAL(8),DIMENSION(N)::A,B,X
    REAL(8),DIMENSION(M)::P
    REAL(8),DIMENSION(M1)::ZNFUNK
    INTEGER::N,L,M,N1,N2,N3,N4,MACTIV,K,KK,I,I1,I2,I3,J1,D,AA,AC,SHAGP,PODRP,TEXT,ACTIV(1),Q,L1,M1,N11,N21
    LOGICAL::POVTOR
    EXTERNAL F,CGR
! ÏPÈCBOEHÈE ÇHA×EHÈÉ KOHCTAHTAM,ÏAPAMETPAM METOÄA È ÈX AHAËÈÇ.
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
! BÛÄEËEHÈE AKTÈBHÛX OÃPAHÈ×EHÈÉ
    POVTOR=.FALSE.
    11111 CONTINUE
    CALL F(X,ZNFUNK,-1)
    MACTIV=L
    L1=L+1
    IF(L/=0)THEN
        DO I=1,L
            ACTIV(I)=I
        END DO
    END IF
    IF(L1<=M)THEN
        DO I=L1,M
            IF(ZNFUNK(I)>-SLOY.OR.P(I)>MINDV)THEN
                MACTIV=MACTIV+1
                ACTIV(MACTIV)=I
                IF(P(I)<CONST1) P(I)=NACHDV
            END IF
        END DO
    END IF
    N4=N3+MACTIV
! OCHOBHÛE ÄEÉCTBÈß METOÄA HÜÞTOHA ÇAÄAHÈE ÇHA×EHÈÉ  XPR,DVOY
    N11=N1-1
    IF(N11/=0)THEN
        DO I=1,N11
            XPR(I)=X(I)
        END DO
    END IF
    N21=N2+1
    IF(N21<=N)THEN
        DO I=N21,N
            XPR(I)=X(I)
        END DO
    END IF
    IF(L/=0)THEN
        DO I=1,L
            DVOY(I)=P(I)
        END DO
    END IF
    IF(L1<=MACTIV)THEN
        DO I=L1,MACTIV
            DVOY(I)=DSQRT(P(ACTIV(I)))
        END DO
    END IF
    CALL GRADC8(X,DVOY,ZNFUNK,NORLZ,F)
    DO I=1,M1
        FUNK(I)=ZNFUNK(I)
    END DO
    CALL SHTRAF
    CRIT=NORLZ
    TEXT=0
    IF(PODRP/=0)THEN
        WRITE(*,"(5X,'METOÄ HÜÞTOHA : C8')")
        WRITE(*,"(5X,'TO×HOCTÜ PEØEHÈß ÇAÄA×È HËÏ',6X,'E=',D12.5)")E
        WRITE(*,"(5X,'×ÈCËO ÈTEPAÖÈÉ',19X,'D=',I5)")D
        WRITE(*,"(5X,'MAÆOPAHTA ÃOËÄCTEÉHA',13X,'MG=',D12.5)")MAJGOS
        WRITE(*,"(5X,'ÏAPAMETP ÄËß BÛÄEË.AKTÈBHÛX OÃP.',1X,'SLOY=',D12.5)")SLOY
        WRITE(*,"(5X,'ØAÃ ÄÈÔÔEPEHÖÈPOBAHÈß',12X,'SHAGD=',D12.5)")SHAGD
        WRITE(*,"(5X,'ÏOPßÄOK ÄÈÔÔEPEHÖÈPOBAHÈß',8X,'AA=',I5)")AA
        WRITE(*,"(5X,'ØAÃ BÛ×ÈCËEHÈß ÃECCÈAHA',10X,'SHAGG=',D12.5)")SHAGG
        WRITE(*,"(5X,'ÏOPßÄOK BÛ×ÈCËEHÈß ÃECCÈAHA',6X,'AC=',I5)")AC
    END IF
    IF(POVTOR)CALL PRTNLP(0,N,L,M,X,ZNFUNK,P,ST,CRIT,1,4,TEXT)
! OCHOBHOÉ ÖÈKË
    DO
        K=KK+1
        IF(.NOT.(K<D+1.AND.DABS(CRIT)>E))EXIT
        ! BÛ×ÈCËEHÈE È OÁPAÙEHÈE ÃECCÈAHA LZZ
        IF(MACTIV>=1)THEN
            DO I=1,MACTIV
                DO I1=1,MACTIV
                    LZZ((N3+I1-1)*N4+N3+I)=0
                END DO
            END DO
        END IF
        IF(L1<=MACTIV)THEN
            DO I=L1,MACTIV
                LZZ((N3+I-1)*N4+N3+I)=2*ZNFUNK(ACTIV(I))
            END DO
        END IF
        CALL CGS(F,X,ZNFUNK,M,CGR,GR,HES,0,N1,N2,SHAGG,AC)
        DO I=1,N3
            DO I1=1,N3
                LZZ((I1-1)*N4+I)=HES((N1-1+I1-1)*N3+(N1-1+I))
            END DO
        END DO
        IF(MACTIV>=1)THEN
            DO I=1,MACTIV
                I1=ACTIV(I)
                CALL CGS(F,X,ZNFUNK,M,CGR,GR,HES,I1,N1,N2,SHAGG,AC)
                DO I2=1,N3
                    DO I3=1,N3
                        LZZ((I3-1)*N4+I2)=LZZ((I3-1)*N4+I2)+P(I1)*HES((N1-1+I3-1)*N3+(N1-1+I2))
                    END DO
                END DO
            END DO
        END IF
        CALL DMINV(LZZ,N4,DET,LRAB,MRAB)
        IF(DET==0)THEN
            WRITE(*,"(/10X,'ÃECCÈAH BÛPOÆÄEH')")
            RETURN
        END IF
        ! BÛ×ÈCËEHÈE  HÜÞTOHOBCKOÃO  HAÏPABËEHÈß
        DO I=1,N4
            NAPR(I)=0
            DO I1=1,N4
                NAPR(I)=NAPR(I)+LZZ((I1-1)*N4+I)*LZ(I1)
            END DO
        END DO
        ! PEÃÓËÈPOBKA ÄËÈHÛ ØAÃA BÄOËÜ HAÏPABËEHÈß
        STEP=1
        DO
            IF(N1<=N2)THEN
                DO I=N1,N2
                    XPR(I)=X(I)-STEP*NAPR(I-N1+1)
                END DO
            END IF
            IF(MACTIV>=1)THEN
                DO I=1,MACTIV
                    DVPR(I)=DVOY(I)-STEP*NAPR(I+N3)
                END DO
            END IF
            CALL F(XPR,ZNFPR,-1)
            CALL GRADC8(XPR,DVPR,ZNFPR,NORLZ1,F)
            IF(NORLZ1>(1-STEP*MAJGOS)*NORLZ.AND.STEP>1.D-9)THEN
                STEP=STEP/2
            ELSE
                EXIT
            END IF
        END DO
        ! ÏPOBEPÈM,OÁHÓËÈËACÜ ËÈ KAKAß-ËÈÁO ÄBOÉCTBEHHAß ÏEPEMEHHAß ÏPÈ HAPÓØEHHOM AKTÈBHOM OÃPAHÈ×EHÈÈ
        IF(L1<=MACTIV)THEN
            DO I=L1,MACTIV
                I1=ACTIV(I)
                IF(ZNFPR(I1)>0.AND.DVPR(I)<DSQRT(CONST1))THEN
                    IF(P(I1)>=NACHDV)THEN
                        P(I1)=10*P(I1)
                    ELSE
                        P(I1)=10*NACHDV
                    END IF
                    WRITE(*,"(/10X,'METOÄ CXOÄÈTCß HE K PEØEHÈÞ')")
                    RETURN
                END IF
            END DO
        END IF
        ! ÏPOBEPÈM, ÄOÁABÈËÈCÜ ËÈ HOBÛE AKTÈBHÛE OÃPAHÈ×EHÈß
        IF(L<=MACTIV)THEN
            L11=L
            IF(L==0) L11=1
            DO I=L11,MACTIV
                IF(I==L11.AND.L==0)THEN
                    I1=M
                    IF(MACTIV>0) I1=ACTIV(1)-1
                    J1=1
                ELSE
                    IF(I/=MACTIV)THEN
                        I1=ACTIV(I+1)-1
                    ELSE
                        I1=M
                    END IF
                    J1=ACTIV(I)+1
                END IF
                IF(J1<=I1)THEN
                    DO I2=J1,I1
                        IF(ZNFPR(I2)>0)THEN
                            P(I2)=2*MINDV
                            POVTOR=.TRUE.
                            GOTO 11111
                        END IF
                    END DO
                END IF
            END DO
        END IF
        ! ÏEPEKA×KA ÈÇ MACCÈBOB: XPR,DVPR,ZNFPR,NORLZ1 B MACCÈBÛ: X,DVOY,P,ZNFUNK,NORLZ.
        DO I=N1,N2
            X(I)=XPR(I)
        END DO
        IF(L/=0)THEN
            DO I=1,L
                P(I)=DVPR(I)
                DVOY(I)=DVPR(I)
            END DO
        END IF
        IF(L1<=MACTIV)THEN
            DO I=L1,MACTIV
                DVOY(I)=DVPR(I)
                P(ACTIV(I))=DVOY(I)**2
            END DO
        END IF
        DO I=1,M1
            ZNFUNK(I)=ZNFPR(I)
        END DO
        NORLZ=NORLZ1
        KK=K
        DO I=1,M1
            FUNK(I)=ZNFUNK(I)
        END DO
        CALL SHTRAF
        CRIT=NORLZ
        CALL PRTNLP(KK,N,L,M,X,ZNFUNK,P,ST,CRIT,SHAGP,PODRP,TEXT)
    END DO
! KOHEÖ ÖÈKËA
    IF(PODRP>0.AND.CRIT<=E) TEXT=2
    IF(PODRP>0.AND.KK==D.AND.TEXT/=2) TEXT=3
    IF(PODRP/=0) WRITE(*,"(/10X,'OÏTÈMAËÜHAß TO×KA')")
    CALL PRTNLP(KK,N,L,M,X,ZNFUNK,P,ST,CRIT,1,4,TEXT)
    RETURN
END SUBROUTINE C8
!--------------------------------------------------------------------------------------------------------------------------
! BÛ×ÈCËEHÈE ÃPAÄÈEHTA LZ È EÃO HOPMÛ, ×ACTÈ×HOE ÇAÏOËHEHÈE MATPÈÖÛ  LZZ .
SUBROUTINE GRADC8(X,DVOY,ZNFUNK,NGR,F)
    COMMON /A1/  M1,N,L
    COMMON /A2/  N1,N2,N3,N4,MACTIV
    COMMON /A3/  SHAGD,AA
    COMMON /C81/ ACTIV
    COMMON /C83/ GR
    COMMON /C85/ LZ
    COMMON /C86/ LZZ
    INTEGER::I,I1,I2,M1,M,N,L,N1,N2,N3,N4,AA,MACTIV
    INTEGER,DIMENSION(1)::ACTIV
    REAL(8)::NGR,F,SHAGD
    REAL(8),DIMENSION(1)::DVOY,GR,LZ,LZZ
    REAL(8),DIMENSION(N)::X
    REAL(8),DIMENSION(M1)::ZNFUNK
    EXTERNAL F
    M=M1-1
    CALL CGR(F,X,ZNFUNK,M,GR,0,N1,N2,SHAGD,AA)
    DO I2=1,N3
        LZ(I2)=GR(N1-1+I2)
    END DO
    IF(MACTIV>=1)THEN
        DO I=1,MACTIV
            I1=ACTIV(I)
            CALL CGR(F,X,ZNFUNK,M,GR,I1,N1,N2,SHAGD,AA)
            IF(I<=L)THEN
                DO I2=1,N3
                    LZ(I2)=LZ(I2)+GR(N1-1+I2)*DVOY(I)
                    LZZ((N3+I-1)*N4+I2)=GR(N1-1+I2)
                    LZZ((I2-1)*N4+N3+I)=LZZ((N3+I-1)*N4+I2)
                END DO
                LZ(N3+I)=ZNFUNK(I1)
            ELSE
                DO I2=1,N3
                    LZ(I2)=LZ(I2)+GR(N1-1+I2)*DVOY(I)**2
                    LZZ((N3+I-1)*N4+I2)=2*GR(N1-1+I2)*DVOY(I)
                    LZZ((I2-1)*N4+N3+I)=LZZ((N3+I-1)*N4+I2)
                END DO
                LZ(N3+I)=2*DVOY(I)*ZNFUNK(I1)
            END IF
        END DO
    END IF
    NGR=0
    DO I=1,N4
        NGR=NGR+LZ(I)**2
    END DO
    NGR=DSQRT(NGR)
    RETURN
END SUBROUTINE GRADC8
! BÛ×ÈCËEHÈE ØTPAÔA
SUBROUTINE SHTRAF
    COMMON/A1/M1,N,L /A4/ST /A5/ZNFUNK
    INTEGER M1,M,N,L,I,L1
    REAL*8 ST,ZNFUNK(1)
    M=M1-1
    ST=0
    IF(L/=0)THEN
        DO I=1,L
             ST=ST+DABS(ZNFUNK(I))
        END DO
    END IF
    L1=L+1
    IF(L1<=M)THEN
        DO I=L1,M
            IF(ZNFUNK(I)>0) ST=ST+ZNFUNK(I)
        END DO
    END IF
    RETURN
END SUBROUTINE SHTRAF
