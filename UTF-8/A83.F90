! МЕТОД НЬЮТОНА. ВЕРСИЯ 3
SUBROUTINE A83(N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
! ОПИСАНИЕ ПЕРЕМЕННЫХ
    COMMON /C/    NF
    COMMON /A830/ M1
    COMMON /A831/ HESP
    COMMON /A832/ G2
    COMMON /A833/ P1
    COMMON /A834/ P2
    COMMON /A835/ XV
    COMMON /A836/ FV
    COMMON /A837/ XT
    COMMON /A838/ TR
    COMMON /A839/ L1
    REAL(8)::F,Y,C,E,H,NG1,NG12,NG22,E1,E2,E3,Z1,Z2,Z3,ST,C0,C1,C2,C3,C4,Y11,CMIN,FC,F1,F2,F3,FMIN,CGR,PHESP,PGR,NP
    INTEGER::D,PORDIF,SHAGP,PODRP,S,Q
    LOGICAL::GRAT
    REAL(8),DIMENSION(2)::HESP,G2,P1,P2,XV,FV,XT,TR,L1,M1
    REAL(8),DIMENSION(40)::PAR
    REAL(8),DIMENSION(N)::X,A,B,G1
    EXTERNAL F,FNLP
    ! СЛУЖЕБНЫЕ ПЕРЕМЕННЫЕ
    INTEGER::SP_VAR
! 
    E=PAR(1+Q)
    D=PAR(2+Q)
    KK=PAR(3+Q)
    INF=PAR(4+Q)
    ST=PAR(5+Q)
    E1=PAR(6+Q)
    S=PAR(7+Q)
    PORDIF=PAR(8+Q)
    H=PAR(9+Q)
    SHAGP=PAR(10+Q)
    PODRP=PAR(11+Q)
    N1=1
    N2=N
    E2=1.D-15
    E3=1.D-60
    IF(E1<1.D-6.OR.E1>0.499999)E1=0.499
    IF(S==0)S=N
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
    NG12=0
    DO I=N1,N2
        NG12=NG12+G1(I)**2
    END DO
    NG1=DSQRT(NG12)
    K=KK
! ПEЧATЬ ИCXOДHЫX ДAHHЫX
    IF(PODRP/=0)THEN
        WRITE(*,"(/A/)")    '     MИHИMИЗAЦИЯ METOДOM HЬЮTOHA (BEPCИЯ 3): A83'
        WRITE(*,"(A,I5)")   '     PAЗMEPHOCTЬ ПPOCTPAHCTBA ПEPEMEHHЫX  N= ',N
        WRITE(*,"(A,D12.5)")'     TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ              E= ',E
        WRITE(*,"(A,I5)")   '     ЧИCЛO ИTEPAЦИЙ                       D= ',D
        WRITE(*,"(A,D12.5)")'     HAЧAЛЬHЫЙ ШAГ CПУCKA                 ST=',ST
        WRITE(*,"(A,D12.5)")'     MAЖOPAHTA ГOЛДCTEЙHA                 E1=',E1
        WRITE(*,"(A,I5)")   '     ПOPЯДOK ДИФФEPEHЦИPOBAHИЯ            AC=',PORDIF
        WRITE(*,"(A,D12.5)")'     ШAГ ДИФФEPEHЦИPOBAHИЯ                H= ',H
    END IF
    CALL PRTUCM(K,NF,N,X,Y,NG1,1,PODRP)
! HAЧAЛO OCHOBHOГO ЦИKЛA
    K=1
    SP_VAR=10
    DO
        SELECT CASE(SP_VAR)
            CASE(10)
                IF(K>D.OR.NG1<=E)THEN
                    EXIT
                END IF
                Z1=10*H/NG1
                DO I=N1,N2
                    P2(I)=G1(I)
                    G2(I)=G1(I)
                    XV(I)=X(I)+Z1*P2(I)
                END DO
                Y11=F(XV,FNLP)
                CALL GRAD(N,F,XV,Y11,HESP,1,N,H,PORDIF,FNLP)
                PHESP=0
                PGR=NG12
                DO I=N1,N2
                    HESP(I)=(HESP(I)-G1(I))/Z1
                    PHESP=PHESP+P2(I)*HESP(I)
                END DO
                IF(DABS(PGR)>=1.D+10*DABS(PHESP))THEN
                    CGR=ST
                    GRAT=.TRUE.
                    SP_VAR=50
                ELSE
                    CGR=PGR/DABS(PHESP)
                    DO I=N1,N2
                        P1(I)=CGR*G1(I)
                    END DO
                    IF(S>=2)THEN
                        DO J=2,S
                            IF(DABS(PGR)>=1.D+9*DABS(PHESP))THEN
                                EXIT
                            ELSE
                                NG22=0
                                Z1=PGR/PHESP
                                DO I=N1,N2
                                    G2(I)=G2(I)-Z1*HESP(I)
                                    NG22=NG22+G2(I)**2
                                END DO
                                IF(NG22<1.D-30)THEN
                                    EXIT
                                ELSE
                                    Z1=NG22/NG12
                                    NG12=NG22
                                    NP=0
                                    PGR=0
                                    DO I=N1,N2
                                        P2(I)=G2(I)+Z1*P2(I)
                                        NP=NP+P2(I)**2
                                        PGR=PGR+P2(I)*G1(I)
                                    END DO
                                    NP=DSQRT(NP)
                                    Z1=10*H/NP
                                    DO I=N1,N2
                                        XV(I)=X(I)+Z1*P2(I)
                                    END DO
                                    Y11=F(XV,FNLP)
                                    CALL GRAD(N,F,XV,Y11,HESP,1,N,H,PORDIF,FNLP)
                                    PHESP=0
                                    DO I=N1,N2
                                        HESP(I)=(HESP(I)-G1(I))/Z1
                                        PHESP=PHESP+P2(I)*HESP(I)
                                    END DO
                                    IF(DABS(PGR)>=1.D+9*DABS(PHESP))THEN
                                        Z1=DSIGN(PGR,PGR)
                                        DO I=N1,N2
                                            P1(I)=P1(I)+3*Z1*P2(I)
                                        END DO
                                        EXIT
                                    ELSE
                                        Z1=PGR/DABS(PHESP)
                                        PGR=0
                                        DO I=N1,N2
                                            P1(I)=P1(I)+Z1*P2(I)
                                            PGR=PGR+G2(I)*P2(I)
                                        END DO
                                    END IF
                                END IF
                            END IF
                        END DO
                    END IF
                    GRAT=.FALSE.
                    Z1=0
                    DO I=N1,N2
                        Z1=Z1+G1(I)*P1(I)
                    END DO
                    C=1
                    IF(Z1<=0) C=-1
                    DO I=N1,N2
                        XV(I)=X(I)-C*P1(I)
                    END DO
                    FC=F(XV,FNLP)
                    IF(FC<(Y-E1*C*Z1))THEN
                        SP_VAR=130
                    ELSE
                        C0=1
                        DO I=N1,N2
                            Z1=DEXP(DLOG(E3+DABS(X(I)))+DLOG(E2)-DLOG(E3+DABS(P1(I))))
                            IF(Z1<C0) C0=Z1
                        END DO
                        IF(C0<E3)C0=E3
                        SP_VAR=60
                    END IF
                END IF
            CASE(50)
                C=CGR
                C0=C
                DO I=N1,N2
                    Z1=DEXP(DLOG(E3+DABS(X(I)))+DLOG(E2)-DLOG(E3+DABS(G1(I))))
                    IF(Z1<C0) C0=Z1
                    P1(I)=G1(I)
                    XV(I)=X(I)-C*P1(I)
                END DO
                IF(C0<E3) C0=E3
                FC=F(XV,FNLP)
                SP_VAR=60
            CASE(60)
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
                        SP_VAR=100
                    ELSE
                        C4=C
                        F4=FC
                        SP_VAR=80
                    END IF
                ELSE
                    C3=C
                    F3=FC
                    SP_VAR=70
                END IF
            CASE(70)
                DO
                    C4=C3
                    F4=F3
                    C3=C3*0.1
                    IF(DABS(C3)<=DABS(C0))THEN
                        IF(GRAT)THEN
                            WRITE(*,"(A)")'     OДHOMEPHЫЙ ПOИCK HEУДAЧEH'
                            SP_VAR=20
                            EXIT
                        ELSE
                            GRAT=.TRUE.
                            SP_VAR=50
                            EXIT
                        END IF
                    END IF
                    DO I=N1,N2
                        XV(I)=X(I)-C3*P1(I)
                    END DO
                    F3=F(XV,FNLP)
                    IF(F3<=Y)THEN
                        SP_VAR=80
                        EXIT
                    END IF
                END DO
            CASE(80)
                CMIN=C3
                FMIN=F3
                SP_VAR=90
            CASE(90)
                DO
                    C2=C3*0.5
                    IF(DABS(C2)<=DABS(C0))THEN
                        IF(GRAT)THEN
                            WRITE(*,"(A)")'     OДHOMEPHЫЙ ПOИCK HEУДAЧEH'
                            SP_VAR=20
                            EXIT
                        END IF
                        GRAT=.TRUE.
                        SP_VAR=50
                        EXIT
                    ELSE
                        DO I=N1,N2
                            XV(I)=X(I)-C2*P1(I)
                        END DO
                        F2=F(XV,FNLP)
                        IF(F2>FMIN)THEN
                            SP_VAR=120
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
            CASE(100)
                C2=C3
                F2=F3
                C3=C
                F3=FC
                SP_VAR=110
            CASE(110)
                DO
                    C4=C3*2
                    DO I=N1,N2
                        XV(I)=X(I)-C4*P1(I)
                    END DO
                    F4=F(XV,FNLP)
                    IF(F4<=F3)THEN
                        CMIN=C4
                        FMIN=F4
                        C1=C2
                        F1=F2
                        C2=C3
                        F2=F3
                        C3=C4
                        F3=F4
                    ELSE
                        SP_VAR=120
                        EXIT
                    END IF
                END DO
            CASE(120)
                C1=C1/C3
                C4=C4/C3
                FC=(1-C1)*2*(C1-C4)/(0.5-C4)*F2+2*(C1-0.5)*(C1-C4)/(1-C4)*F3+(C1-0.5)/(C4-0.5)*(C1-1)/(C4-1)*F4
                IF(DABS(FC-F1)<(F1-F3)*1.D-4)THEN
                    Z1=F2*(1-C4)
                    Z2=F3*(C4-0.5)
                    Z3=-F4*0.5
                    IF(DABS(Z1+Z2+Z3)<E3)THEN
                        SP_VAR=140
                    ELSE
                        C=(Z1*(1+C4)+Z2*(0.5+C4)+Z3*1.5)/(Z1+Z2+Z3)*C3*0.5
                    END IF
                ELSE
                    F1=-0.5*(C4-0.5)*(C4-1)*F1
                    F2=(1-C1)*(C4-C1)*(C4-1)*F2
                    F3=-(0.5-C1)*(C4-C1)*(C4-0.5)*F3
                    F4=0.5*(0.5-C1)*(1-C1)*F4
                    Z1=3*(F1+F2+F3+F4)
                    Z2=-(F1*(1.5+C4)+F2*(C1+1+C4)+F3*(C1+0.5+C4)+F4*(C1+1.5))
                    Z3=F1*(0.5+1.5*C4)+F2*(C1+C1*C4+C4)+F3*(C1*0.5+C1*C4+0.5*C4)+F4*(C1*1.5+0.5)
                    IF(DABS(Z1)<1.D-30)THEN
                        SP_VAR=140
                    ELSE
                        C=(DSQRT(DABS(Z2*Z2-Z1*Z3))-Z2)/Z1*C3
                    END IF
                END IF
                IF(SP_VAR/=140)THEN
                    DO I=N1,N2
                        XV(I)=X(I)-C*P1(I)
                    END DO
                    FC=F(XV,FNLP)
                    IF(FMIN>FC)THEN
                        SP_VAR=130
                    ELSE
                        SP_VAR=140
                    END IF
                END IF
            CASE(140)
                DO I=N1,N2
                    X(I)=X(I)-CMIN*P1(I)
                END DO
                Y=FMIN
                SP_VAR=150
            CASE(130)
                Y=FC
                DO I=N1,N2
                    X(I)=XV(I)
                END DO
                SP_VAR=150
            CASE(150)
                CALL GRAD(N,F,X,Y,G1,1,N,H,PORDIF,FNLP)
                NG12=0
                DO I=N1,N2
                    NG12=NG12+G1(I)**2
                END DO
                NG1=DSQRT(NG12)
                KK=K
                CALL PRTUCM(K,NF,N,X,Y,NG1,SHAGP,PODRP)
                K=K+1
                SP_VAR=10
            CASE(20)
                EXIT
        END SELECT
    END DO
! KOHEЦ OCHOBHOГO ЦИKЛA
    IF(PODRP/=0)WRITE(*,"(A)")'     OПTИMAЛЬHAЯ TOЧKA'
    CALL PRTUCM(K,NF,N,X,Y,NG1,1,3)
    PAR(3+Q)=KK
    RETURN
END SUBROUTINE A83
