SUBROUTINE AP1(N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /A/    C,T1,P1,P2,P3,P4,CS
    COMMON /B/    L1
    COMMON /C/    NF
    COMMON /DA/   E3,EG,EM
    COMMON /EA/   I
    COMMON /EB/   EL
    COMMON /AP11/ X1
    REAL(8)::Y,F,F1,C,T1,P1,P2,P3,P4,CS,H,E,TG,P,T,FS,E1,E3,EM,EL,EG,NG
    INTEGER::D,SHAGP,PODRP,AC,Q
    REAL(8),DIMENSION(2)::X1
    REAL(8),DIMENSION(N)::X,A,B,G1
    REAL(8),DIMENSION(40)::PAR
    EXTERNAL F,F1,FNLP
! 
    E=PAR(1+Q)
    D=PAR(2+Q)
    KK=PAR(3+Q)
    C=PAR(4+Q)
    P1=PAR(5+Q)
    P3=PAR(6+Q)
    P4=PAR(7+Q)
    NS=PAR(8+Q)
    TG=PAR(9+Q)
    AC=PAR(10+Q)
    H=PAR(11+Q)
    SHAGP=PAR(12+Q)
    PODRP=PAR(13+Q)
    NF=0
    EL=1.D-18
    EG=1.D+18
    EM=1.D-11
    IF(NS<4)THEN
        L=1
        L1=NS
    ELSE
        L=2
        L1=NS-3
    END IF
    IF(P1<1.D-4.OR.P1>0.99D0)P1=0.6D0
    IF(P3<1.D-4.OR.P3>0.99D0)P3=0.5D0
    IF(P4<1.D-4.OR.P4>0.49D0)P4=0.4D0
    DO I=1,N
        IF(X(I)<A(I)) X(I)=A(I)
        IF(X(I)>B(I)) X(I)=B(I)
    END DO
    K=0
    NG=0
    DO I=1,N
        X1(I)=X(I)
    END DO
    Y=F(X,FNLP)
    E1=EM*DABS(Y)+E
    FS=Y+3*E1
    IF(L==2)THEN
        CALL GRAD(N,F,X,Y,G1,1,N,  H,AC,   FNLP)
        DO I=1,N
            NG=NG+G1(I)**2
        END DO
        NG=DSQRT(NG)
        T1=0
        J1=0
    END IF
! œE◊AT‹ »CXOƒH€X ƒAHH€X
    IF(PODRP/=0)THEN
        WRITE(*,"(/A/)")    '     M»H»M»«A÷»ﬂ METOƒOM œOKOOPƒ»HATHO√O Cœ”CKA: AP1'
        WRITE(*,"(A,I5)")   '     PA«MEPHOCT‹ œPOCTPAHCTBA œEPEMEHH€X  N= ',N
        WRITE(*,"(A,D12.5)")'     TO◊HOCT‹ PEÿEH»ﬂ «AƒA◊»              E= ',E
        WRITE(*,"(A,I5)")   '     ◊»CÀO »TEPA÷»…                       D= ',D
        WRITE(*,"(A,D12.5)")'     HA◊AÀ‹H€… ÿA√ Cœ”CKA                 C= ',C
        WRITE(*,"(A,D12.5)")'     MA∆OPAHTA APM»…O                     P1=',P1
        WRITE(*,"(A,D12.5)")'     KO›‘‘»÷»EHT ”MEH‹ÿEH»ﬂ ÿA√A          P3=',P3
        WRITE(*,"(A,D12.5)")'     MA∆OPAHTA √OÀƒCTE…HA                 P4=',P4
        WRITE(*,"(A,I5)")   '     HOMEP BEPC»» METOƒA                  V= ',NS
        WRITE(*,"(A,D12.5)")'     "Õ≈Œœ–≈ƒ≈À®ÕŒ—“‹" «¿ƒ¿Õ»ﬂ √–¿Õ»÷     TG=',TG
        WRITE(*,"(A,I5)")   '     œOPﬂƒOK ƒ»‘‘EPEH÷»POBAH»ﬂ            AC=',AC
        WRITE(*,"(A,D12.5)")'     ÿA√ ƒ»‘‘EPEH÷»POBAH»ﬂ                H= ',H
    END IF 
    CALL PRTUCM(K,NF,N,X,Y,NG,1,PODRP)
    K=1
! HA◊AÀO OCHOBHO√O ÷»KÀA
    DO
        IF(.NOT.(K<D+1.AND.FS-Y>E1))EXIT
        FS=Y
        P2=EM*DABS(Y)
        E1=P2+E
        T1=0
        DO I=1,N
            IF(L==1) CALL GRAD(N,F,X,Y,G1,I,I,H,AC,   FNLP)
            P=DABS(X(I)-B(I))
            IF(G1(I)>0) P=DABS(X(I)-A(I))
            IF(P>=TG)THEN
                T=DABS(G1(I))
                SELECT CASE(L)
                    CASE(1)
                        IF(T>P*EL)THEN
                            T=P/T
                        ELSE
                            T=EG
                        END IF
                        T1=G1(I)**2
                        IF(T1>0.AND.NS==3)THEN
                            E3=EM*DABS(X(I)/G1(I))
                            IF(E3<EL) E3=EL
                        END IF
                        CALL S(T,F,F1,N,X,Y,X1,G1,FNLP)
                        X(I)=X(I)-CS*G1(I)
                    CASE(2)
                        IF(T>T1)THEN
                            T1=T
                            J1=I
                            CS=P
                        END IF
                END SELECT
            END IF
        END DO
        IF(L==2.AND.J1>0.5)THEN
            I=J1
            T=T1
            IF(T>CS*EL)THEN
                T=CS/T
            ELSE
                T=EG
            END IF
            T1=T1*T1
            IF(T1>0.AND.NS==6)THEN
                E3=EM*DABS(X(I)/G1(I))
                IF(E3<EL)E3=EL
            END IF
            CALL S(T,F,F1,N,X,Y,X1,G1,FNLP)
            X(I)=X(I)-CS*G1(I)
            CALL GRAD(N,F,X,Y,G1,1,N,H,AC,   FNLP)
            T1=0
            J1=0
            NG=0
            DO I=1,N
                NG=NG+G1(I)**2
            END DO
            NG=DSQRT(NG)
        END IF
        KK=K
        CALL PRTUCM(K,NF,N,X,Y,NG,SHAGP,PODRP)
        K=K+1
    END DO
! KOHE÷ OCHOBHO√O ÷»KÀA
    IF(PODRP/=0)WRITE(*,'(/A)')'     OœT»MAÀ‹HAﬂ TO◊KA'
    CALL PRTUCM(K,NF,N,X,Y,NG,1,3)
    PAR(3+Q)=KK
    RETURN
END SUBROUTINE AP1
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION F1(X2,N,F,X,X1,G1,FNLP)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /EA/ I
    REAL(8)::X2,F,F1
    REAL(8),DIMENSION(N)::X1,G1,X
    EXTERNAL FNLP
! 
    X1(I)=X(I)-X2*G1(I)
    F1=F(X1,FNLP)
    RETURN
END FUNCTION F1
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE S(LS,F,F1,N,X,Y,X1,G1,FNLP)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /B/ L1
    REAL(8)::LS,Y,F,F1
    REAL(8),DIMENSION(N)::X,X1,G1
    EXTERNAL F,F1,FNLP
! 
    IF(L1==1)CALL S1(LS,F,F1,N,X,Y,X1,G1,FNLP)
    IF(L1==2)CALL S2(LS,F,F1,N,X,Y,X1,G1,FNLP)
    IF(L1==3)CALL S3(LS,F,F1,N,X,Y,X1,G1,FNLP)
    RETURN
END SUBROUTINE S
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE S1(LS,F,F1,N,X,Y,X1,G1,FNLP)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /A/  C,T1,P1,P2,P3,P4,CS
    COMMON /EB/ EL
    REAL(8)::LS,Y,C,T1,P1,P2,P3,EL,F,F1,P4,CS,C1,T2,TD,TU,BB,BL,P11
    REAL(8),DIMENSION(N)::X,X1,G1
    EXTERNAL F,FNLP
! 
    TU=2
    J1=1
    CS=0
    C1=C
    P11=C
    DO
        IF(C1>=EL)THEN
            IF(J1==1)THEN
                IF(C1>LS)THEN
                    TU=0
                    C1=LS
                    P11=LS
                END IF
            END IF
            T2=F1(C1,N,F,X,X1,G1,FNLP)-Y
            TD=T2+C1*(1-P4)*T1
            IF(TD<0)THEN
                IF(J1>=2)THEN
                    BL=C1
                    C1=0.5D0*(BL+BB)
                    CYCLE
                END IF
                IF(TU<1)THEN
                    EXIT
                ELSE
                    P11=P11*2
                    C1=C1+P11
                END IF
            END IF
            TU=T2+C1*P4*T1
            IF(TU>=P2)THEN
                IF(J1<2)THEN
                    BL=C1-P11
                    BB=C1
                    C1=0.5*(BB+BL)
                    J1=J1+1
                END IF
                BB=C1
                C1=0.5*(BB+BL)
            ELSE
                EXIT
            END IF
        ELSE
            EXIT
        END IF
    END DO
    IF(T2<0)THEN
        CS=C1
        Y=T2+Y
    END IF
    RETURN
END SUBROUTINE S1
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE S2(LS,F,F1,N,X,Y,X1,G1,FNLP)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /A/  C,T1,P1,P2,P3,P4,CS
    COMMON /EB/ EL
    REAL(8)::LS,Y,EL,F,F1,C,T1,P1,P2,P3,P4,C1,CS,T2,TU
    REAL(8),DIMENSION(N)::X,X1,G1
    EXTERNAL F,FNLP
! 
    C1=C
    CS=0
    IF(C1>LS) C1=LS
    DO
        T2=F1(C1,N,F,X,X1,G1,FNLP)-Y
        TU=T2+P1*C1*T1
        IF(TU>P2)THEN
            C1=C1*P3
            IF(C1<=EL)EXIT
        ELSE
            EXIT
        END IF
    END DO
    IF(T2<0)THEN
        CS=C1
        Y=T2+Y
    END IF
    RETURN
END SUBROUTINE S2
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE S3(LS,F,F1,N,X,Y,X1,G1,FNLP)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /A/  C,T1,P1,P2,P3,P4,CS
    COMMON /DA/ E3,EG,EM
    REAL(8)::LS,Y,F,F1,C,T1,P1,P2,P3,P4,CS,E3,EG,EM,C1,U,V,W,FU,FV,FW,FX,A1,B1,XS,E,M,TOL,T2,R,Q,P,D
    REAL(8),DIMENSION(N)::X,X1,G1
    EXTERNAL F,FNLP
    ! —À”≈¡Õ¿ﬂ œ≈–≈Ã≈ÕÕ¿ﬂ
    INTEGER::SP_VAR
! 
    CS=0
    C1=C
    I1=0
    I2=0
    U=0
    FX=Y
    SP_VAR=0
    DO
        V=U+C1
        IF(V>LS)THEN
            V=LS
            B1=V
            I2=1
        END IF
        FV=F1(V,N,F,X,X1,G1,FNLP)
        IF(FX>FV)THEN
            I1=1
            A1=U
            C1=2*C1
            U=V
            FX=FV
        ELSE
            I2=1
            C1=C1/2
            B1=V
        END IF
        IF(C1<E3.OR.C1>EG)THEN
            XS=V
            SP_VAR=1
            EXIT
        END IF
        IF(.NOT.(I1+I2<1.5))EXIT
    END DO
    IF(SP_VAR==0)THEN
        C1=(3.D0-DSQRT(5.D0))*0.5D0
        XS=A1+C1*(B1-A1)
        W=XS
        V=XS
        E=0
        FX=F1(XS,N,F,X,X1,G1,FNLP)
        FW=FX
        FV=FX
        DO
            M=0.5D0*(A1+B1)
            TOL=EM*DABS(XS)+E3
            T2=2*TOL
            IF(DABS(XS-M)>(T2-0.5D0*(B1-A1)))THEN
                R=0.D0
                Q=0.D0
                P=0.D0
                IF(DABS(E)>TOL)THEN
                    R=(XS-W)*(FX-FV)
                    Q=(XS-V)*(FX-FW)
                    P=(XS-V)*Q-(XS-W)*R
                    Q=2*(Q-R)
                    IF(Q>0)THEN
                        P=-P
                    ELSE
                        Q=-Q
                    END IF
                    R=E
                    E=D
                END IF
                IF(DABS(P)<DABS(0.5D0*Q*R).AND.P>Q*(A1-XS).AND.P<Q*(B1-XS))THEN
                    D=P/Q
                    U=XS+D
                    IF(U-A1<T2.OR.B1-U<T2)THEN
                        D=-TOL
                        IF(XS<M)D=TOL
                    END IF
                ELSE
                    E=A1-XS
                    IF(XS<M)E=B1-XS
                    D=C1*E
                END IF
                IF(DABS(D)<TOL)THEN
                    IF(D>0)THEN
                        U=XS+TOL
                    ELSE
                        U=XS-TOL
                    END IF
                ELSE
                    U=XS+D
                END IF
                FU=F1(U,N,F,X,X1,G1,FNLP)
                IF(FU<FX)THEN
                    IF(U<XS)THEN
                        B1=XS
                    ELSE
                        A1=XS
                    END IF
                    V=W
                    FV=FW
                    W=XS
                    FW=FX
                    XS=U
                    FX=FU
                ELSE
                    IF(U<XS)THEN
                        A1=U
                    ELSE
                        B1=U
                    END IF
                    IF(.NOT.(FU<=FW.OR.W==XS))THEN
                        IF(.NOT.(FU<=FV.OR.V==XS.OR.V==W))CYCLE
                        V=U
                        FV=FU
                    ELSE
                        V=W
                        FV=FW
                        W=U
                        FW=FU
                    END IF
                END IF
            ELSE
                EXIT
            END IF
        END DO
    END IF
    IF(Y>FX)THEN
        CS=XS
        Y=FX
    END IF
    RETURN
END SUBROUTINE S3
!-------------------------------------------------------------------------------------------------------------------------------
