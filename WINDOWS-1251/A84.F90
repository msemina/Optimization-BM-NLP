!  ¬¿«»Õ‹ﬁ“ŒÕŒ¬— »… Ã≈“Œƒ
SUBROUTINE A84(N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /C/    NF
    COMMON /A841/ HES
    COMMON /A842/ P1
    COMMON /A843/ P2
    COMMON /A844/ DGR
    COMMON /A845/ XV
    REAL(8)::C,E,H,NG1,E1,E2,E3,Z1,Z2,Z3,Z4,ST,C0,C1,C2,C3,C4,CMIN,FC,F1,F2,F3,F4,FMIN,F,Y
    INTEGER::D,PORDIF,PORGES,SHAGP,PODRP,V,NACHG,Q
    LOGICAL::GRAT
    REAL(8), DIMENSION(4)::HES
    REAL(8), DIMENSION(2)::P1,P2,DGR,XV
    REAL(8), DIMENSION(N)::X,A,B,G1
    REAL(8), DIMENSION(40)::PAR
    EXTERNAL F,GRAD,FNLP
    ! —À”∆≈¡Õ€≈ œ≈–≈Ã≈ÕÕ€≈
    INTEGER::SP_VAR
! 
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
    IF(E1<1.D-6.OR.(E1>0.499999D0))E1=0.499D0
    DO I=1,N
         XV(I)=X(I)
    END DO
    NF=0
    SELECT CASE(INF)
        CASE(0)
            Y=F(X,FNLP)
            CALL GRAD(N,F,X,Y,G1,1,N,H,PORDIF,FNLP)
        CASE(1)
            CALL GRAD(N,F,X,Y,G1,1,N,H,PORDIF,FNLP)
        CASE(2)
            Y=F(X,FNLP)
    END SELECT
    NG1=0.D0
    DO I=1,N
        NG1=NG1+G1(I)**2
    END DO
    NG1=DSQRT(NG1)
    DO I=1,N
        P1(I)=G1(I)
        HES((I-1)*N+I)=1.D0
        J1=I+1
        IF(J1>N)EXIT
        DO J=J1,N
            HES((J-1)*N+I)=0.D0
            HES((I-1)*N+J)=0.D0
        END DO
    END DO
    GRAT=.TRUE.
    K=0
! œE◊AT‹ »CXOƒH€X ƒAHH€X
    IF(PODRP/=0)THEN
        WRITE(*,"(/A/)")    '     M»H»M»«A÷»ﬂ KBA«»H‹ﬁTOHOBCK»M METOƒOM: A84'
        WRITE(*,"(A,I5)")   '     PA«MEPHOCT‹ œPOCTPAHCTBA œEPEMEHH€X  N= ',N
        WRITE(*,"(A,D12.5)")'     TO◊HOCT‹ PEÿEH»ﬂ «AƒA◊»              E= ',E
        WRITE(*,"(A,I5)")   '     ◊»CÀO »TEPA÷»…                       D= ',D
        WRITE(*,"(A,D12.5)")'     HA◊AÀ‹H€… ÿA√ Cœ”CKA                 ST=',ST
        WRITE(*,"(A,D12.5)")'     MA∆OPAHTA √OÀƒCTE…HA                 E1=',E1
        WRITE(*,"(A,I5)")   '     HOMEP BEPC»» METOƒA                  V= ',V
        WRITE(*,"(A,I5)")   '     œOPﬂƒOK ƒ»‘‘EPEH÷»POBAH»ﬂ            AC=',PORDIF
        WRITE(*,"(A,D12.5)")'     ÿA√ ƒ»‘‘EPEH÷»POBAH»ﬂ                H= ',H
    END IF
    CALL PRTUCM(0,NF,N,X,Y,NG1,1,PODRP)
    K=1
! HA◊AÀO OCHOBHO√O ÷»KÀA
    SP_VAR=10
    DO
        SELECT CASE(SP_VAR)
            CASE(10)
                IF(K>D.OR.NG1<=E)THEN
                    SP_VAR=20
                ELSE
                    IF(K/=1)THEN
                        SELECT CASE(V)
                            CASE(1)
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
                                ELSE
                                    DO I=N1,N2
                                        HES((I-1)*N+I)=HES((I-1)*N+I)+(2*P2(I)*P1(I)*Z1+P1(I)**2*Z2-P2(I)**2*Z3)/Z4
                                        J1=I+1
                                        IF(J1>N2)THEN
                                            EXIT
                                        ELSE
                                            DO J=J1,N2
                                                HES((J-1)*N+I)=HES((J-1)*N+I)+((P2(I)*P1(J)+P2(J)*P1(I))*Z1+P1(I)*P1(J)*Z2-P2(I)*P2(J)*Z3)/Z4
                                                HES((I-1)*N+J)=HES((J-1)*N+I)
                                            END DO
                                        END IF
                                    END DO
                                END IF
                            CASE(2)
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
                                END IF
                            CASE(3)
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
                                END IF
                            CASE(4)
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
                                ELSE
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
                                END IF
                        END SELECT
                        IF(SP_VAR==10)GRAT=.FALSE.
                    END IF
                    IF(SP_VAR==10)THEN
                        IF(GRAT)THEN
                            SP_VAR=100
                        ELSE
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
                            IF(Z1>0.D0)C=1.D0
                            DO I=N1,N2
                                XV(I)=X(I)-C*P1(I)
                            END DO
                            FC=F(XV,FNLP)
                            IF(FC<Y-E1*C*Z1)THEN
                                SP_VAR=900
                            ELSE
                                C0=1.D0
                                DO I=N1,N2
                                    Z1=DEXP(DLOG(E3+DABS(X(I)))+DLOG(E2)-DLOG(E3+DABS(P1(I))))
                                    IF(Z1<C0)C0=Z1
                                END DO
                                IF(C0<E3)C0=E3
                                SP_VAR=200
                            END IF
                        END IF
                    END IF
                END IF
            CASE(100)
                C=ST
                C0=C
                DO I=N1,N2
                    Z1=DEXP(DLOG(E3+DABS(X(I)))+DLOG(E2)-DLOG(E3+DABS(G1(I))))
                    IF(Z1<C0)C0=Z1
                    P1(I)=G1(I)
                    XV(I)=X(I)-C*P1(I)
                END DO
                IF(C0<E3)C0=E3
                FC=F(XV,FNLP)
                SP_VAR=200
            CASE(200)
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
                    ELSE
                        C4=C
                        F4=FC
                        SP_VAR=400
                    END IF
                END IF
                IF(SP_VAR==200)THEN
                    C3=C
                    F3=FC
                    SP_VAR=300
                END IF
            CASE(300)
                C4=C3
                F4=F3
                C3=C3*0.1D0
                IF(DABS(C3)<DABS(C0))THEN
                    IF(GRAT)THEN
                        WRITE(*,"(A)")'     OƒHOMEPH€… œO»CK HE”ƒA◊EH'
                        SP_VAR=20
                    ELSE
                        GRAT=.TRUE.
                        SP_VAR=100
                    END IF
                END IF
                IF(SP_VAR==300)THEN
                    DO I=N1,N2
                        XV(I)=X(I)-C3*P1(I)
                    END DO
                    F3=F(XV,FNLP)
                    IF(F3>Y)THEN
                        SP_VAR=300
                    ELSE
                        SP_VAR=400
                    END IF
                END IF
            CASE(400)
                CMIN=C3
                FMIN=F3
                SP_VAR=500
            CASE(500)
                DO
                    C2=C3*0.5D0
                    IF(DABS(C2)<DABS(C0))THEN
                        IF(GRAT)THEN
                            WRITE(*,"(A)")'     OƒHOMEPH€… œO»CK HE”ƒA◊EH'
                            SP_VAR=20
                        ELSE
                            GRAT=.TRUE.
                            SP_VAR=100
                        END IF
                    END IF
                    IF(SP_VAR/=500)THEN
                        EXIT
                    ELSE
                        DO I=N1,N2
                            XV(I)=X(I)-C2*P1(I)
                        END DO
                        F2=F(XV,FNLP)
                        IF(F2>FMIN)THEN
                            SP_VAR=800
                            EXIT
                        ELSE
                            CMIN=C2
                            FMIN=F2
                            C4=C3
                            F4=F3
                            C3=C2
                            F3=F2
                        END IF
                    END IF
                END DO
            CASE(600)
                C2=C3
                F2=F3
                C3=C
                F3=FC
                SP_VAR=700
            CASE(700)
                C4=C3*2.D0
                DO I=N1,N2
                    XV(I)=X(I)-C4*P1(I)
                END DO
                F4=F(XV,FNLP)
                IF(F4>F3)THEN
                    SP_VAR=800
                ELSE
                    CMIN=C4
                    FMIN=F4
                    C1=C2
                    F1=F2
                    C2=C3
                    F2=F3
                    C3=C4
                    F3=F4
                    SP_VAR=700
                END IF
            CASE(800)
                C1=C1/C3
                C4=C4/C3
                FC=(1.D0-C1)*2.D0*(C1-C4)/(0.5D0-C4)*F2+2.D0*(C1-0.5D0)*(C1-C4)/(1.D0-C4)*F3+(C1-0.5D0)/(C4-0.5D0)*(C1-1.D0)/(C4-1.D0)*F4
                IF(DABS(FC-F1)>=(F1-F3)*0.0001D0)THEN
                    F1=-0.5D0*(C4-0.5D0)*(C4-1.D0)*F1
                    F2=(1.D0-C1)*(C4-C1)*(C4-1.D0)*F2
                    F3=-(0.5D0-C1)*(C4-C1)*(C4-0.5D0)*F3
                    F4=0.5D0*(0.5D0-C1)*(1.D0-C1)*F4
                    Z1=3.D0*(F1+F2+F3+F4)
                    Z2=-(F1*(1.5D0+C4)+F2*(C1+1.D0+C4)+F3*(C1+0.5D0+C4)+F4*(C1+1.5D0))
                    Z3=F1*(0.5D0+1.5D0*C4)+F2*(C1+C1*C4+C4)+F3*(C1*0.5D0+C1*C4+0.5D0*C4)+F4*(C1*1.5D0+0.5D0)
                    IF(DABS(Z1)<1.D-15)THEN
                        SP_VAR=1111
                    ELSE
                        C=(DSQRT(DABS(Z2*Z2-Z1*Z3))-Z2)/Z1*C3
                    END IF
                ELSE
                    Z1=F2*(1.D0-C4)
                    Z2=F3*(C4-0.5D0)
                    Z3=-F4*0.5D0
                    IF(DABS(Z1+Z2+Z3)<E3)THEN
                        SP_VAR=1111
                    ELSE
                        C=(Z1*(1.D0+C4)+Z2*(0.5D0+Z4)+Z3*1.5D0)/(Z1+Z2+Z3)*C3*0.5D0
                    END IF
                END IF
                IF(SP_VAR==800)THEN
                    DO I=N1,N2
                        XV(I)=X(I)-C*P1(I)
                    END DO
                    FC=F(XV,FNLP)
                    IF(FMIN>FC)SP_VAR=900
                END IF
            CASE(1111)
                DO I=N1,N2
                    X(I)=X(I)-CMIN*P1(I)
                END DO
                C=CMIN
                Y=FMIN
                SP_VAR=1000
            CASE(900)
                Y=FC
                DO I=N1,N2
                    X(I)=XV(I)
                END DO
                SP_VAR=1000
            CASE(1000)
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
                SP_VAR=10
            CASE(20)
                EXIT
        END SELECT
    END DO
! KOHE÷ OCHOBHO√O ÷»KÀA
    IF(PODRP/=0)WRITE(*,"(/A)")'     OœT»MAÀ‹HAﬂ TO◊KA'
    CALL PRTUCM(K,NF,N,X,Y,NG1,1,3)
    RETURN
END SUBROUTINE A84
