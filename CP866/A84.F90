! KBAáàçúûíéçéÇëäàâ METOÑ
SUBROUTINE A84(N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
    COMMON /C/    NF
    COMMON /A841/ HES
    COMMON /A842/ P1
    COMMON /A843/ P2
    COMMON /A844/ DGR
    COMMON /A845/ XV
    REAL(8),DIMENSION(N)::X,A,B,G1
    REAL(8),DIMENSION(1)::HES,P1,P2,DGR,XV
    REAL(8),DIMENSION(40)::PAR
    REAL(8)::C,E,H,NG1,E1,E2,E3,Z1,Z2,Z3,Z4,ST,C0,C1,C2,C3,C4,CMIN,FC,F1,F2,F3,F4,FMIN,F,Y
    INTEGER::D,PORDIF,PORGES,SHAGP,PODRP,V,NACHG,Q
    LOGICAL::GRAT
    EXTERNAL F,GRAD,FNLP
    !
    INTEGER::SP_VAR
!-----------------------------------------------------------------------
    E=PAR(1+Q)
    D=PAR(2+Q)
    KK=PAR(3+Q)
    INF=PAR(4+Q)
    ST=PAR(5+Q)
    E1=PAR(6+Q)
    V=PAR(7+Q)
    NACHG=PAR(8+Q)
    PORDIF=PAR(9+Q)
    H=PAR(10+Q)
    PORGES=PAR(11+Q)
    SHAGP=PAR(12+Q)
    PODRP=PAR(13+Q)
    N1=1
    N2=N
    E2=1.D-11
    E3=1.D-18
    IF(E1<1.D-6.OR.(E1>0.499999D0)) E1=0.499D0
    DO I=1,N
        XV(I)=X(I)
    END DO
    NF=0
    IF(INF/=0)THEN
        IF(INF==1) CALL GRAD(N,F,X,Y,G1,1,N,H,PORDIF,FNLP)
    ELSE
        Y=F(X,FNLP)
        CALL GRAD(N,F,X,Y,G1,1,N,H,PORDIF,FNLP)
    END IF
    IF(INF==2) Y=F(X,FNLP)
    NG1=0.D0
    DO I=1,N
        NG1=NG1+G1(I)**2
    END DO
    NG1=DSQRT(NG1)
    DO I=1,N
        P1(I)=G1(I)
        HES((I-1)*N+I)=1.D0
        J1=I+1
        IF(J1>N)THEN
            EXIT
        ELSE
            DO J=J1,N
                HES((J-1)*N+I)=0.D0
                HES((I-1)*N+J)=0.D0
            END DO
        END IF
    END DO
    GRAT=.TRUE.
    K=0
! èEóATú àCXOÑHõX ÑAHHõX
    IF(PODRP/=0)THEN
        WRITE(*,"(5X,'MàHàMàáAñàü KBAáàHúûTOHOBCKàM METOÑOM')")
        WRITE(*,"(5X,'PAáMEPHOCTú èPOCTPAHCTBA èEPEMEHHõX',2X,'N=',I3)")N
        WRITE(*,"(5X,'TOóHOCTú PEòEHàü áAÑAóà',14X,           'E=',D11.4)")E
        WRITE(*,"(5X,'óàCãO àTEPAñàâ',23X,                    'D=',I5)")D
        WRITE(*,"(5X,'HAóAãúHõâ òAÉ CèìCKA',17X,              'ST=',D11.4)")ST
        WRITE(*,"(5X,'MAÜOPAHTA ÉOãÑCTEâHA',17X,              'E1=',D11.4)")E1
        WRITE(*,"(5X,'HOMEP BEPCàà METOÑA',18X,               'V=',I3)")V
        WRITE(*,"(5X,'èOPüÑOK ÑàîîEPEHñàPOBAHàü',12X,         'AC=',I3)")PORDIF
        WRITE(*,"(5X,'òAÉ ÑàîîEPEHñàPOBAHàü',17X,             'H=',D11.4)")H
    END IF
    CALL PRTUCM(0,NF,N,X,Y,NG1,1,PODRP)
    K=1
! HAóAãO OCHOBHOÉO ñàKãA
    SP_VAR=0
    DO
        IF(SP_VAR==0)THEN
            IF(K>D.OR.NG1<=E) EXIT
            IF(K==1)THEN
                SP_VAR=11
                CYCLE
            END IF
            IF(V/=1)THEN
                SP_VAR=12
                CYCLE
            END IF
            Z1=0.D0
            Z2=0.D0
            DO I=1,N
                P2(I)=0.D0
                DO J=1,N
                    P2(I)=P2(I)+HES((J-1)*N+I)*P1(J)
                END DO
                Z1=Z1+DGR(I)*P2(I)
                Z2=Z2+P1(I)*P2(I)
            END DO
            Z3=0.D0
            DO I=N1,N2
                P1(I)=C*P1(I)
                DO J=N1,N2
                    P1(I)=P1(I)-HES((J-1)*N+I)*DGR(J)
                END DO
                Z3=Z3+DGR(I)*P1(I)
            END DO
            Z4=Z1**2+Z2*Z3
            IF(DABS(Z4)<1.D-17)THEN
                GRAT=.TRUE.
                SP_VAR=100
                CYCLE
            END IF
            DO I=N1,N2
                HES((I-1)*N+I)=HES((I-1)*N+I)+(2*P2(I)*P1(I)*Z1+P1(I)**2*Z2-P2(I)**2*Z3)/Z4
                J1=I+1
                IF(J1>N2)EXIT
                DO J=J1,N2
                    HES((J-1)*N+I)=HES((J-1)*N+I)+((P2(I)*P1(J)+P2(J)*P1(I))*Z1+P1(I)*P1(J)*Z2-P2(I)*P2(J)*Z3)/Z4
                    HES((I-1)*N+J)=HES((J-1)*N+I)
                END DO
            END DO
        END IF
        !
        IF(SP_VAR==12)THEN
            IF(V/=2)THEN
                SP_VAR=21
                CYCLE
            END IF
            Z1=0.D0
            Z2=0.D0
            DO I=N1,N2
                P2(I)=0.D0
                DO J=N1,N2
                    P2(I)=P2(I)+HES((J-1)*N+I)*DGR(J)
                END DO
                Z1=Z1+DGR(I)*P1(I)
                Z2=Z2+DGR(I)*P2(I)
            END DO
            IF(DABS(Z1)<1.D-17.OR.DABS(Z2)<1.D-17)THEN
                GRAT=.TRUE.
                SP_VAR=100
            ELSE
                DO I=N1,N2
                    HES((I-1)*N+I)=HES((I-1)*N+I)-P2(I)**2/Z2+C*P1(I)**2/Z1
                    J1=I+1
                    IF(J1<=N2)THEN
                        DO J=J1,N2
                            HES((J-1)*N+I)=HES((J-1)*N+I)-P2(I)*P2(J)/Z2+P1(I)*P1(J)*C/Z1
                            HES((I-1)*N+J)=HES((J-1)*N+I)
                        END DO
                    END IF
                END DO
                SP_VAR=21
            END IF
        END IF
        !
        IF(SP_VAR==21)THEN
            IF(V/=3)THEN
                SP_VAR=27
                CYCLE
            END IF
            Z1=0.D0
            DO I=N1,N2
                P1(I)=C*P1(I)
                DO J=N1,N2
                    P1(I)=P1(I)-HES((J-1)*N+I)*DGR(J)
                END DO
                Z1=Z1+DGR(I)*P1(I)
            END DO
            IF(DABS(Z1)<1.D-17)THEN
                GRAT=.TRUE.
                SP_VAR=100
            ELSE
                DO I=N1,N2
                    HES((I-1)*N+I)=HES((I-1)*N+I)+P1(I)**2/Z1
                    J1=I+1
                    IF(J1<=N2)THEN
                        DO J=J1,N2
                            HES((J-1)*N+I)=HES((J-1)*N+I)+P1(I)*P1(J)/Z1
                            HES((I-1)*N+J)=HES((J-1)*N+I)
                        END DO
                    END IF
                END DO
                SP_VAR=27
            END IF
        END IF
        !
        IF(SP_VAR==27)THEN
            IF(V==4)THEN
                Z1=0.D0
                Z2=0.D0
                DO I=N1,N2
                    P2(I)=0.D0
                    DO J=N1,N2
                        P2(I)=P2(I)+HES((J-1)*N+I)*DGR(J)
                    END DO
                    Z1=Z1+DGR(I)*P1(I)
                    Z2=Z2+DGR(I)*P2(I)
                END DO
                IF(DABS(Z1)<1.D-17)THEN
                    GRAT=.TRUE.
                    SP_VAR=100
                    CYCLE
                END IF
                DO I=N1,N2
                    HES((I-1)*N+I)=HES((I-1)*N+I)-2*P2(I)*P1(I)/Z1+P1(I)**2*(C+Z2/Z1)/Z1
                    J1=I+1
                    IF(J1<=N2)THEN
                        DO J=J1,N2
                            HES((J-1)*N+I)=HES((J-1)*N+I)-(P1(I)*P2(J)+P1(J)*P2(I))/Z1+P1(I)*P1(J)*(C+Z2/Z1)/Z1
                            HES((I-1)*N+J)=HES((J-1)*N+I)
                        END DO
                    END IF
                END DO
                SP_VAR=11
            ELSE
                GRAT=.FALSE.
                SP_VAR=11
            END IF
        END IF
        !
        IF(SP_VAR==11)THEN
            IF(GRAT)THEN
                SP_VAR=100
                CYCLE
            END IF
            DO I=N1,N2
                P1(I)=0.D0
                DO J=N1,N2
                    P1(I)=P1(I)+HES((J-1)*N+I)*G1(J)
                END DO
            END DO
            Z1=0.D0
            DO I=N1,N2
                Z1=Z1+G1(I)*P1(I)
            END DO
            C=-1.D0
            IF(Z1>0.D0) C=1.D0
            DO I=N1,N2
                XV(I)=X(I)-C*P1(I)
            END DO
            FC=F(XV,FNLP)
            IF(FC<Y-E1*C*Z1)THEN
                SP_VAR=900
                CYCLE
            END IF
            C0=1.D0
            DO I=N1,N2
                Z1=DEXP(DLOG(E3+DABS(X(I)))+DLOG(E2)-DLOG(E3+DABS(P1(I))))
                IF(Z1<C0) C0=Z1
            END DO
            IF(C0<E3) C0=E3
            SP_VAR=200
        END IF
        !
        IF(SP_VAR==100)THEN
            C=ST
            C0=C
            DO I=N1,N2
                Z1=DEXP(DLOG(E3+DABS(X(I)))+DLOG(E2)-DLOG(E3+DABS(G1(I))))
                IF(Z1<C0) C0=Z1
                P1(I)=G1(I)
                XV(I)=X(I)-C*P1(I)
            END DO
            IF(C0<E3) C0=E3
            FC=F(XV,FNLP)
            SP_VAR=200
        END IF
        !
        IF(SP_VAR==200)THEN
            C1=0
            F1=Y
            IF(FC<Y)THEN
                CMIN=C
                FMIN=FC
                C3=C*0.5D0
                DO I=N1,N2
                    XV(I)=X(I)-C3*P1(I)
                END DO
                F3=F(XV,FNLP)
                IF(F3>FMIN)THEN
                    SP_VAR=600
                    CYCLE
                END IF
                C4=C
                F4=FC
                SP_VAR=400
            ELSE
                C3=C
                F3=FC
                SP_VAR=300
            END IF
        END IF
        !
        IF(SP_VAR==300)THEN
            DO
                C4=C3
                F4=F3
                C3=C3*0.1D0
                IF(DABS(C3)<DABS(C0))THEN
                    IF(GRAT)THEN
                        WRITE(*,"(5X,'OÑHOMEPHõâ èOàCK HEìÑAóEH')")
                        EXIT
                    END IF
                    GRAT=.TRUE.
                    SP_VAR=100
                    EXIT
                END IF
                DO I=N1,N2
                    XV(I)=X(I)-C3*P1(I)
                END DO
                F3=F(XV,FNLP)
                IF(F3<=Y)EXIT
            END DO
            IF(SP_VAR==300)SP_VAR=400
        END IF
        !
        IF(SP_VAR==400)THEN
            CMIN=C3
            FMIN=F3
            SP_VAR=500
        END IF
        !
        IF(SP_VAR==500)THEN
            DO
                C2=C3*0.5D0
                IF(DABS(C2)<DABS(C0))THEN
                    IF(GRAT)THEN
                        WRITE(*,"(5X,'OÑHOMEPHõâ èOàCK HEìÑAóEH')")
                        EXIT
                    END IF
                    GRAT=.TRUE.
                    SP_VAR=100
                    EXIT
                END IF
                DO I=N1,N2
                    XV(I)=X(I)-C2*P1(I)
                END DO
                F2=F(XV,FNLP)
                IF(F2>FMIN)THEN
                    SP_VAR=800
                    EXIT
                END IF
                CMIN=C2
                FMIN=F2
                C4=C3
                F4=F3
                C3=C2
                F3=F2
            END DO
        END IF
        !
        IF(SP_VAR==600)THEN
            C2=C3
            F2=F3
            C3=C
            F3=FC
            SP_VAR=700
        END IF
        !
        IF(SP_VAR==700)THEN
            C4=C3*2.D0
            DO I=N1,N2
                XV(I)=X(I)-C4*P1(I)
            END DO
            F4=F(XV,FNLP)
            IF(F4>F3)THEN
                SP_VAR=800
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
        END IF
        !
        IF(SP_VAR==800)THEN
            C1=C1/C3
            C4=C4/C3
            FC=(1.D0-C1)*2.D0*(C1-C4)/(0.5D0-C4)*F2+2.D0*(C1-0.5D0)*(C1-C4)/(1.D0-C4)*F3+(C1-0.5D0)/(C4-0.5D0)*(C1-1.D0)/(C4-1.D0)*F4
            IF(DABS(FC-F1)<(F1-F3)*0.0001D0)THEN
                Z1=F2*(1.D0-C4)
                Z2=F3*(C4-0.5D0)
                Z3=-F4*0.5D0
                IF(DABS(Z1+Z2+Z3)<E3)THEN
                    SP_VAR=1111
                    CYCLE
                END IF
                C=(Z1*(1.D0+C4)+Z2*(0.5D0+Z4)+Z3*1.5D0)/(Z1+Z2+Z3)*C3*0.5D0
            ELSE
                F1=-0.5D0*(C4-0.5D0)*(C4-1.D0)*F1
                F2=(1.D0-C1)*(C4-C1)*(C4-1.D0)*F2
                F3=-(0.5D0-C1)*(C4-C1)*(C4-0.5D0)*F3
                F4=0.5D0*(0.5D0-C1)*(1.D0-C1)*F4
                Z1=3.D0*(F1+F2+F3+F4)
                Z2=-(F1*(1.5D0+C4)+F2*(C1+1.D0+C4)+F3*(C1+0.5D0+C4)+F4*(C1+1.5D0))
                Z3=F1*(0.5D0+1.5D0*C4)+F2*(C1+C1*C4+C4)+F3*(C1*0.5D0+C1*C4+0.5D0*C4)+F4*(C1*1.5D0+0.5D0)
                IF(DABS(Z1)<1.D-15)THEN
                    SP_VAR=1111
                    CYCLE
                END IF
                C=(DSQRT(DABS(Z2*Z2-Z1*Z3))-Z2)/Z1*C3
            END IF
            DO I=N1,N2
                XV(I)=X(I)-C*P1(I)
            END DO
            FC=F(XV,FNLP)
            IF(FMIN>FC)THEN
                SP_VAR=900
            ELSE
                SP_VAR=1111
            END IF
        END IF
        !
        IF(SP_VAR==1111)THEN
            DO I=N1,N2
                X(I)=X(I)-CMIN*P1(I)
            END DO
            C=CMIN
            Y=FMIN
            SP_VAR=1000
        END IF
        !
        IF(SP_VAR==900)THEN
            Y=FC
            DO I=N1,N2
                X(I)=XV(I)
            END DO
            SP_VAR=1000
        END IF
        !
        IF(SP_VAR==1000)THEN
            DO I=N1,N2
                DGR(I)=G1(I)
            END DO
            CALL GRAD(N,F,X,Y,G1,N1,N2,H,PORDIF,FNLP)
            NG1=0.D0
            DO I=N1,N2
                NG1=NG1+G1(I)**2
                DGR(I)=G1(I)-DGR(I)
            END DO
            NG1=DSQRT(NG1)
            KK=K
            CALL PRTUCM(K,NF,N,X,Y,NG1,SHAGP,PODRP)
            K=K+1
        END IF
        !
    END DO
! KOHEñ OCHOBHOÉO ñàKãA
    IF(PODRP/=0)WRITE(*,"(/5X,'OèTàMAãúHAü TOóKA')")
    CALL PRTUCM(K,NF,N,X,Y,NG1,1,3)
    RETURN
END
