! METOÑ HúûTOHA. BEPCàü 2.
SUBROUTINE A8(N,X,A,B,F,AGR,AGS,Y,G1,Q,PAR,FNLP)
! éèàëÄçàÖ èÖêÖåÖççõï
    COMMON /C/    NF
    COMMON /A81/  HES
    COMMON /A82/  P1
    COMMON /A83/  P2
    COMMON /A84/  XV
    COMMON /A85/  FV
    COMMON /A86/  XT
    COMMON /A87/  TR
    COMMON /A88/  L1
    COMMON /GRD/  Z1,Z2
    COMMON /A829/ M1
    REAL(8),DIMENSION(1)::HES,P1,P2,XV,FV,XT,TR,L1,M1
    REAL(8),DIMENSION(40)::PAR
    REAL(8),DIMENSION(N)::X,A,B,G1
    REAL(8)::C,E,H,NG1,E1,E2,E3,Z1,Z2,Z3,Z4,ST,C0,C1,C2,C3,C4,CMIN,FC,F1,F2,F3,F4,FMIN,F5,F6,Z,F,Y
    INTEGER::D,K,KK,I,J,N1,N2,PODRP,SHAGP,INF,NF,Q
    LOGICAL::GRAT
    ! ÇÇÖÑå ÑéèéãçàíÖãúçìû èÖêÖåÖççìû Ñãü áÄåÖçõ ÅÖáìëãéÇçõï èÖêÖïéÑéÇ
    INTEGER::TMP
    EXTERNAL F,FNLP
!-------------------------------------------------------------------------------------------------------------------
    E=PAR(1+Q)
    D=PAR(2+Q)
    KK=PAR(3+Q)
    INF=PAR(4+Q)
    ST=PAR(5+Q)
    E1=PAR(6+Q)
    H=PAR(7+Q)
    SHAGP=PAR(8+Q)
    PODRP=PAR(9+Q)
    E2=1.D-11
    E3=1.D-18
    N1=1
    N2=N
    IF(E1<0.000001D0.OR.E1>0.499999D0) E1=0.499D0
    NF=0
    DO I=1,N
        XV(I)=X(I)
    END DO
    IF(INF==0.OR.INF==2) Y=F(X,FNLP)
    CALL GRADT(N,F,NG1,X,G1,H,1,N,P2,FNLP)
! èEóATú àCXOÑHõX ÑAHHõX
    IF(PODRP/=0)THEN
        WRITE(*,"(5X,'MàHàMàáAñàü METOÑOM HúûTOHA.BEPCàü 2.',/)")
        WRITE(*,"(5X,'PAáMEPHOCTú èPOCTPAHCTBA èEPEMEHHõX',2X,'N=',I3)")N
        WRITE(*,"(5X,'TOóHOCTú PEòEHàü áAÑAóà',14X,'E=',D11.4)")E
        WRITE(*,"(5X,'óàCãO àTEPAñàâ',23X,'D=',I5)")D
        WRITE(*,"(5X,'HAóAãúHõâ òAÉ CèìCKA',17X,'ST=',D11.4)")ST
        WRITE(*,"(5X,'MAÜOPAHTA ÉOãÑCTEâHA',17X,'E1=',D11.4)")E1
        WRITE(*,"(5X,'òAÉ ÑàîîEPEHñàPOBAHàü',17X,'H=',D11.4,/)")H
    END IF
    CALL PRTUCM(0,NF,N,X,Y,NG1,SHAGP,PODRP)
    K=1
    TMP=0
! HAóAãO OCHOBHOÉO ñàKãA
    DO
        IF(TMP==0)THEN
            IF(.NOT.(K<=D.AND.NG1>E)) EXIT
            DO J=N1,N2
                Z1=X(J)
                Z3=P2((J-1)*3+3)
                X(J)=Z1+Z3
                HES((J-1)*N+J)=(P2((J-1)*3+1)-2*Y+P2((J-1)*3+2))/Z3/Z3
                N3=J-1
                IF(N1<=N3)THEN
                    DO I=N1,N3
                        Z2=X(I)
                        Z4=P2((I-1)*3+3)
                        X(I)=Z2+Z4
                        HES((J-1)*N+I)=(F(X,FNLP)-P2((J-1)*3+2)-P2((I-1)*3+2)+Y)/Z3/Z4
                        HES((I-1)*N+J)=HES((J-1)*N+I)
                        X(I)=Z2
                    END DO
                END IF
                X(J)=Z1
            END DO
            CALL DMINV(HES,N,DET,L1,M1)
            IF(DET==0)THEN
                WRITE(*,"(5X,'ÉECCàAH BõPOÜÑEH')")
                GRAT=.TRUE.
                TMP=1
            ELSE
                GRAT=.FALSE.
                DO I=N1,N2
                    P1(I)=0
                    DO J=N1,N
                        P1(I)=P1(I)+HES((J-1)*N+I)*G1(J)
                    END DO
                END DO
                Z1=0
                DO I=N1,N2
                    Z1=Z1+G1(I)*P1(I)
                END DO
                C=-1.D0
                IF(Z1>0) C=1.D0
                DO I=N1,N2
                    XV(I)=X(I)-C*P1(I)
                END DO
                FC=F(XV,FNLP)
                Y1=Y-E1*C*Z1
                IF(FC<Y1)THEN
                    TMP=9
                ELSE
                    C0=1.D0
                    DO I=N1,N2
                        Z1=DEXP(DLOG(E3+DABS(X(I)))+DLOG(E2)-DLOG(E3+DABS(P1(I))))
                        IF(Z1<C0) C0=Z1
                    END DO
                    IF(C0<E3) C0=E3
                    TMP=2
                END IF
            END IF
        END IF
        IF(TMP==1)THEN
            C0=ST
            C=ST
            DO I=N1,N2
                Z1=DEXP(DLOG(E3+DABS(X(I)))+DLOG(E2)-DLOG(E3+DABS(G1(I))))
                IF(Z1<C0) C0=Z1
                P1(I)=G1(I)
                XV(I)=X(I)-C*P1(I)
            END DO
            IF(C0<E3) C0=E3
            FC=F(XV,FNLP)
            TMP=2
        END IF
        IF(TMP==2)THEN
            C1=0
            F1=Y
            IF(FC<Y)THEN
                CMIN=C
                FMIN=FC
                C3=C*0.5
                DO I=N1,N2
                    XV(I)=X(I)-C3*P1(I)
                END DO
                F3=F(XV,FNLP)
                IF(F3>FMIN)THEN
                    TMP=6
                ELSE
                    C4=C
                    F4=FC
                    TMP=4
                END IF
            ELSE
                C3=C
                F3=FC
                TMP=3
            END IF
        END IF
        IF(TMP==3)THEN
            C4=C3
            F4=F3
            C3=C3*0.1D0
            IF(DABS(C3)<DABS(C0))THEN
                IF(.NOT.GRAT)THEN
                    GRAT=.TRUE.
                    TMP=1
                ELSE
                    WRITE(*,"(10X,'OÑHOMEPHõâ èOàCK HEìÑAóEH')")
                    RETURN
                END IF
            END IF
            IF(TMP==3)THEN
                DO I=N1,N2
                    XV(I)=X(I)-C3*P1(I)
                END DO
                F3=F(XV,FNLP)
                IF(F3>Y)THEN
                    TMP=3
                ELSE
                    TMP=4
                END IF
            END IF
        END IF
        IF(TMP==4)THEN
            CMIN=C3
            FMIN=F3
            TMP=5
        END IF
        IF(TMP==5)THEN
            C2=C3*0.5D0
            IF(DABS(C2)<DABS(C0))THEN
                IF(.NOT.GRAT)THEN
                    GRAT=.TRUE.
                    TMP=1
                ELSE
                    WRITE(*,"(10X,'OÑHOMEPHõâ èOàCK HEìÑAóEH')")
                    RETURN
                END IF
            END IF
            IF(TMP==5)THEN
                DO I=N1,N2
                    XV(I)=X(I)-C2*P1(I)
                END DO
                F2=F(XV,FNLP)
                IF(F2>FMIN)THEN
                    TMP=8
                ELSE
                    CMIN=C2
                    FMIN=F2
                    C4=C3
                    F4=F3
                    C3=C2
                    F3=F2
                    TMP=5
                END IF
            END IF
        END IF
        IF(TMP==6)THEN
            C2=C3
            F2=F3
            C3=C
            F3=FC
            TMP=7
        END IF
        IF(TMP==7)THEN
            DO
                C4=C3*2
                DO I=N1,N2
                    XV(I)=X(I)-C4*P1(I)
                END DO
                F4=F(XV,FNLP)
                IF(F4>F3)THEN
                    TMP=8
                    EXIT
                END IF
                CMIN=C4
                FMIN=F4
                C1=C2
                F1=F2
                C2=C3
                F2=F3
                C3=C4
                F3=F4
            END DO
        END IF
        IF(TMP==8)THEN
            C1=C1/C3
            C4=C4/C3
            FC=(1-C1)*2*(C1-C4)/(.5-C4)*F2+2*(C1-.5)*(C1-C4)/(1-C4)*F3+(C1-.5)/(C4-.5)*(C1-1)/(C4-1)*F4
            F5=(FC-F1)
            F6=(F1-F3)*0.0001D0
            IF(DABS(F5)<DABS(F6))THEN
                Z1=F2*(1-C4)
                Z2=F3*(C4-.5)
                Z3=(-F4*.5)
                Z=Z1+Z2+Z3
                IF(DABS(Z)<E3)THEN
                    DO I=N1,N2
                        X(I)=X(I)-CMIN*P1(I)
                    END DO
                    Y=FMIN
                    TMP=10
                ELSE
                    C=(Z1*(1+C4)+Z2*(.5+C4)+Z3*1.5)/(Z1+Z2+Z3)*C3*.5
                    DO I=N1,N2
                        XV(I)=X(I)-C*P1(I)
                    END DO
                    FC=F(XV,FNLP)
                    IF(FMIN>FC)THEN
                        TMP=9
                    ELSE
                        DO I=N1,N2
                            X(I)=X(I)-CMIN*P1(I)
                        END DO
                        Y=FMIN
                        TMP=10
                    END IF
                END IF
            ELSE
                F1=(-.5*(C4-.5)*(C4-1)*F1)
                F2=(1-C1)*(C4-C1)*(C4-1)*F2
                F3=(-(.5-C1)*(C4-C1)*(C4-.5)*F3)
                F4=.5*(.5-C1)*(1-C1)*F4
                Z1=3*(F1+F2+F3+F4)
                Z2=(-(F1*(1.5+C4)+F2*(C1+1+C4)+F3*(C1+.5+C4)+F4*(C1+1.5)))
                Z3=F1*(.5+1.5*C4)+F2*(C1+C1*C4+C4)+F3*(C1*.5+C1*C4+.5*C4)+F4*(C1*1.5+.5)
                IF(DABS(Z1)>=1.D-15)THEN
                    C=(DSQRT(DABS(Z2*Z2-Z1*Z3))-Z2)/Z1*C3
                    DO I=N1,N2
                        XV(I)=X(I)-C*P1(I)
                    END DO
                    FC=F(XV,FNLP)
                    IF(FMIN>FC)TMP=9
                END IF
                IF(TMP==8)THEN
                    DO I=N1,N2
                        X(I)=X(I)-CMIN*P1(I)
                    END DO
                    Y=FMIN
                    TMP=10
                END IF
            END IF
        END IF
        IF(TMP==9)THEN
            Y=FC
            DO I=N1,N2
                X(I)=XV(I)
            END DO
            TMP=10
        END IF
        IF(TMP==10)THEN
            CALL GRADT(N,F,NG1,X,G1,H,1,N,P2,FNLP)
            CALL PRTUCM(K,NF,N,X,Y,NG1,SHAGP,PODRP)
            KK=K
            K=K+1
            TMP=0
        END IF
    END DO
! KOHEñ OCHOBHOÉO ñàKãA
    IF(PODRP/=0)WRITE(*,"(/5X,'OèTàMAãúHAü TOóKA')")
    CALL PRTUCM(K,NF,N,X,Y,NG1,1,3)
    PAR(3+Q)=KK
    RETURN
END SUBROUTINE A8
!-------------------------------------------------------------------------------------------------------------------
SUBROUTINE GRADT(N,F,NG1,X,G1,H,N1,N2,P2,FNLP)
    COMMON /GRD/ Z1, Z2
    REAL(8),DIMENSION(N)::X,G1
    REAL(8),DIMENSION(3,N)::P2
    REAL(8)::NG1,Z1,Z2,H,F
    INTEGER::I,N1,N2
    EXTERNAL FNLP,F
    NG1=0
    DO I=N1,N2
        Z1=X(I)
        Z2=H*(1.D0+0.001D0*DABS(Z1))
        P2(3,I)=Z2
        X(I)=Z1+Z2
        P2(2,I)=F(X,FNLP)
        X(I)=Z1-Z2
        P2(1,I)=F(X,FNLP)
        G1(I)=(P2(2,I)-P2(1,I))/2/Z2
        X(I)=Z1
        NG1=NG1+G1(I)**2
    END DO
    NG1=DSQRT(NG1)
    RETURN
END SUBROUTINE GRADT