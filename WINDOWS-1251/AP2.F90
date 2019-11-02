! METOƒ HA»CKOPE…ÿE√O Cœ”CKA
SUBROUTINE AP2(N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /A/    C,T1,P1,P2,P3,P4,P
    COMMON /B/    L
    COMMON /C/    NF
    COMMON /EB/   EL
    COMMON /AP21/ X1
    COMMON /AP22/ T
    COMMON /AP23/ PAR0
    COMMON /AP24/ PAR1
    REAL(8)::Y,C,CS,H,P,P1,P2,P3,P4,T1,FS,E,F,F1AP2,E1,EM,EL,EG,TG,NGR
    REAL(8), DIMENSION(2)::X1,T
    REAL(8), DIMENSION(40)::PAR
    REAL(8), DIMENSION(N)::X,A,B,G1
    LOGICAL, DIMENSION(2)::PAR0,PAR1 ! ƒÀ»Õ¿ Õ≈ ¬≈–Õ¿ (¬≈–Œﬂ“ÕŒ N)
    INTEGER::D,SHAGP,PODRP,AC,Q,NS
    EXTERNAL F,F1AP2,FNLP
! 
    E=PAR(1+Q); D=PAR(2+Q); KK=PAR(3+Q); C=PAR(4+Q); P1=PAR(5+Q); P3=PAR(6+Q); P4=PAR(7+Q)
    NS=PAR(8+Q); TG=PAR(9+Q); AC=PAR(10+Q); H=PAR(11+Q); SHAGP=PAR(12+Q); PODRP=PAR(13+Q)
    N1=1
    L=NS
    N2=N
    EM=1.D-11
    EG=1.D+18
    EL=1.D-18
    IF(P1<1.D-4.OR.P1>0.99D0)P1=0.5D0
    IF(P3<1.D-4.OR.P3>0.99D0)P3=0.5D0
    IF(P4<1.D-4.OR.P4>0.49D0)P4=0.4D0
    DO I=1,N
        IF(X(I)<A(I))X(I)=A(I)
        IF(X(I)>B(I))X(I)=B(I)
    END DO
    DO I=1,N
        X1(I)=X(I)
    END DO
    NF=0
    Y=F(X,FNLP)
    E1=EM*DABS(Y)+E
    CALL GRAD(N,F,X,Y,G1,N1,N,H,AC,FNLP)
    NGR=0.D0
    DO I=1,N
        NGR=NGR+G1(I)**2
    END DO
    NGR=DSQRT(NGR)
    FS=Y+10*E1
! œE◊AT‹ »CXOƒH€X ƒAHH€X
    IF(PODRP/=0)THEN
        WRITE(*,"(/A/)")    '     M»H»M»«A÷»ﬂ METOƒOM HA»CKOPE…ÿE√O Cœ”CKA: AP2'
        WRITE(*,"(A,I5)")   '     PA«MEPHOCT‹ œPOCTPAHCTBA œEPEMEHH€X  N= ',N
        WRITE(*,"(A,D12.5)")'     TO◊HOCT‹ PEÿEH»ﬂ «AƒA◊»              E= ',E
        WRITE(*,"(A,I5)")   '     ◊»CÀO »TEPA÷»…                       D= ',D
        WRITE(*,"(A,D12.5)")'     HA◊AÀ‹H€… ÿA√ Cœ”CKA                 C= ',C
        WRITE(*,"(A,D12.5)")'     MA∆OPAHTA APM»…O                     P1=',P1
        WRITE(*,"(A,D12.5)")'     KO›‘‘»÷»EHT ”MEH‹ÿEH»ﬂ ÿA√A          P3=',P3
        WRITE(*,"(A,D12.5)")'     MA∆OPAHTA √OÀƒCTE…HA                 P4=',P4
        WRITE(*,"(A,I5)")   '     HOMEP BEPC»» METOƒA                  V= ',L
        WRITE(*,"(A,D12.5)")'     HEOœPEƒEÀEHHOCT‹ «AƒAH»ﬂ √PAH»÷      TG=',TG
        WRITE(*,"(A,I5)")   '     œOPﬂƒOK ƒ»‘‘EPEH÷»POBAH»ﬂ            AC=',AC
        WRITE(*,"(A,D12.5)")'     ÿA√ ƒ»‘‘EPEH÷»POBAH»ﬂ                H= ',H
    END IF
    CALL PRTUCM(0,NF,N,X,Y,NGR,SHAGP,PODRP)
! HA◊AÀO OCHOBHO√O ÷»KÀA
    IF(D>=0.5)THEN
        K=KK+1
        DO
            IF(.NOT.(K<D+1.AND.FS-Y>E1))EXIT
            FS=Y
            P2=EM*DABS(Y)
            E1=P2+E
            CS=0.D0
            T1=0.D0
            DO I=1,N
                PAR0(I)=.TRUE.
                IF(G1(I)>0.)THEN
                    P=DABS(X(I)-A(I))
                ELSE
                    P=DABS(X(I)-B(I))
                END IF
                IF(P<TG)THEN
                    PAR0(I)=.FALSE.
                    CYCLE
                END IF
                T(I)=DABS(G1(I))
                IF(T(I)>P*EL)THEN
                    T(I)=P/T(I)
                ELSE
                    T(I)=EG
                END IF
                IF(T(I)>CS) CS=T(I)
                T1=T1+G1(I)**2
            END DO
            CALL SAP2(CS,F,F1AP2,N,X,Y,T,X1,G1,PAR0,PAR1,FNLP)
            DO I=1,N
                IF(PAR0(I))THEN
                    IF(PAR1(I))THEN
                        X(I)=X(I)-P*G1(I)
                    ELSE
                        X(I)=X(I)-T(I)*G1(I)
                    END IF
                END IF
            END DO
            Y=F(X,FNLP)
            CALL GRAD(N,F,X,Y,G1,N1,N2,H,AC,FNLP)
            NGR=0.D0
            DO I=1,N
                NGR=NGR+G1(I)**2
            END DO
            NGR=DSQRT(NGR)
            KK=K
            CALL PRTUCM(K,NF,N,X,Y,NGR,SHAGP,PODRP)
            K=K+1
        END DO
    END IF
! KOHE÷ OCHOBHO√O ÷»KÀA
    IF(PODRP/=0)WRITE(*,"(/5X,'OœT»MAÀ‹HAﬂ TO◊KA')")
    CALL PRTUCM(K,NF,N,X,Y,NGR,1,3)
    PAR(3+Q)=KK
    RETURN
END SUBROUTINE AP2
!-------------------------------------------------------------------------------------------------------------------------------
! BCœOMO√ATEÀ‹HAﬂ œOƒœPO√PAMMA ƒÀﬂ AP2 ( PAH‹ÿE ¡€ÀA S )
SUBROUTINE SAP2(LS,F,F1,N,X,Y,T,X1,G1,PAR0,PAR1,FNLP)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /B/ L
    REAL(8)::LS,F,F1,Y
    REAL(8), DIMENSION(N)::T,X,X1,G1
    LOGICAL, DIMENSION(N)::PAR0,PAR1
    EXTERNAL F,F1,FNLP
! 
    IF(L==1)THEN
        CALL S1AP2(LS,F,F1,N,X,Y,T,X1,G1,PAR0,PAR1,FNLP)
    ELSE
        CALL S2AP2(LS,F,F1,N,X,Y,T,X1,G1,PAR0,PAR1,FNLP)
    END IF
    RETURN
END SUBROUTINE SAP2
!-------------------------------------------------------------------------------------------------------------------------------
! BCœOMO√ATEÀ‹HAﬂ œOƒœPO√PAMMA ƒÀﬂ AP2 ( PAH‹ÿE ¡€ÀA F1 )
FUNCTION F1AP2(X2,N,F,X,X1,G1,PAR0,PAR1,FNLP)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    REAL(8)::X2,F,F1AP2
    REAL(8), DIMENSION(N)::X1,G1,X
    LOGICAL, DIMENSION(N)::PAR0,PAR1
    EXTERNAL FNLP
! 
    DO I=1,N
        IF(PAR0(I).AND.PAR1(I))X1(I)=X(I)-X2*G1(I)
    END DO
    F1AP2=F(X1,FNLP)
    RETURN
END FUNCTION F1AP2
!-------------------------------------------------------------------------------------------------------------------------------
! BCœOMO√ATEÀ‹HAﬂ œOƒœPO√PAMMA ƒÀﬂ AP2 ( PAH‹ÿE ¡€ÀA S1 )
SUBROUTINE S1AP2(LS,F,F1,N,X,Y,T,X1,G1,PAR0,PAR1,FNLP)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /A/  C,T1,P1,P2,P3,P4,P
    COMMON /EB/ EL
    REAL(8)::LS,C,Y,T1,P1,P2,P3,P4,F1,F,C1,P,T2,TU,TD,P11,BB,BL,EL
    REAL(8), DIMENSION(N)::X,X1,G1,T
    LOGICAL, DIMENSION(N)::PAR0,PAR1
    EXTERNAL F,FNLP
! 
    TU=2.D0
    J1=1
    C1=C
    P=0.D0
    P11=C
    DO
        IF(J1==1)THEN
            IF(C1>LS)THEN
               TU=0.D0
               C1=LS
               P11=LS 
            END IF
        END IF
        DO I=1,N
            IF(.NOT.PAR0(I))CYCLE
            IF(C1>T(I))THEN
                PAR1(I)=.FALSE.
            ELSE
                PAR1(I)=.TRUE.
            END IF
        END DO
        T2=F1(C1,N,F,X,X1,G1,PAR0,PAR1,FNLP)-Y
        TD=T2+C1*(1-P4)*T1
        IF(TD<0.D0)THEN
            IF(J1>=2)THEN
                BL=C1
                C1=0.5D0*(BB+BL)
                CYCLE
            END IF
            IF(TU<1.D0)EXIT
            P11=2.D0*P11
            C1=C1+P11
            CYCLE
        END IF
        TU=T2+C1*P4*T1
        IF(TU<P2)THEN
            EXIT
        ELSE
            IF(J1<2)THEN
                BL=C1-P11
                BB=C1
                C1=0.5D0*(BB+BL)
                J1=J1+1
                CYCLE
            END IF
            BB=C1
            C1=0.5D0*(BB+BL)
        END IF
    END DO
    IF(T2<0.D0)THEN
        P=C1
        Y=T2+Y
    END IF
    RETURN
END SUBROUTINE S1AP2
!-------------------------------------------------------------------------------------------------------------------------------
! BCœOMO√ATEÀ‹HAﬂ œOƒœPO√PAMMA ƒÀﬂ AP2 ( PAH‹ÿE ¡€ÀA S2 )
SUBROUTINE S2AP2(LS,F,F1,N,X,Y,T,X1,G1,PAR0,PAR1,FNLP)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /A/  C,T1,P1,P2,P3,P4,P
    COMMON /EB/ EL
    REAL(8)::LS,C,Y,T1,P1,P2,P3,P4,F1,F,C1,P,T2,TU,EL
    REAL(8), DIMENSION(N)::T,X,X1,G1
    LOGICAL, DIMENSION(N)::PAR0,PAR1
    EXTERNAL F,FNLP
! 
    C1=C
    IF(C1>LS)C1=LS
    P=0.D0
    DO
        DO I=1,N
            IF(PAR0(I))THEN
                IF(C1>T(I))THEN
                    PAR1(I)=.FALSE.
                ELSE
                    PAR1(I)=.TRUE.
                END IF
            END IF
        END DO
        T2=F1(C1,N,F,X,X1,G1,PAR0,PAR1,FNLP)-Y
        TU=T2+P1*C1*T1
        IF(TU<=P2)THEN
            EXIT
        ELSE
            C1=C1*P3
            IF(C1<=EL)EXIT
        END IF
    END DO
    IF(T2<0)THEN
        P=C1
        Y=T2+Y
    END IF
    RETURN
END SUBROUTINE S2AP2
!-------------------------------------------------------------------------------------------------------------------------------

