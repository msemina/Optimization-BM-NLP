! R-ÀËÃÎÐÈÒÌ ØÎÐÀ
SUBROUTINE A7(N,X,A,B,F,GRAD,AGS,Y,G,Q,PAR,FNLP)
! ÎÏÈÑÀÍÈÅ ÏÅÐÅÌÅÍÍÛÕ
    COMMON /C/   NF
    COMMON /A71/ G1
    COMMON /A72/ G2
    COMMON /A73/ Z
    COMMON /A74/ BB
    REAL(8)::H,S,BET,ALPHA,F1,DD,EPS,D,DELTA,HI,F,Y
    INTEGER::Q,PODRP,SHAGP
    REAL(8),DIMENSION(2)::G1,G2,Z
    REAL(8),DIMENSION(4)::BB
    REAL(8),DIMENSION(N)::X,A,B,G
    REAL(8),DIMENSION(40)::PAR
    EXTERNAL F,FNLP
    ! ÑËÓÆÅÁÍÛÅ ÏÅÐÅÌÅÍÍÛÅ
    INTEGER::SP_VAR
    SP_VAR=0
! 
    EPS=PAR(1+Q)
    KR=PAR(2+Q)
    KK=PAR(3+Q)
    DELTA=PAR(4+Q)
    ALPHA=PAR(5+Q)
    HI=PAR(6+Q)
    NV=PAR(7+Q)
    S=PAR(8+Q)
    SHAGP=PAR(9+Q)
    PODRP=PAR(10+Q)
    H=S
    NH=3
    BET=1.D0/ALPHA-1
    K=KK+1
    NF=0
    SP_VAR=100
! HA×AËO OCHOBHOÃO ÖÈKËA
    DO
        IF(SP_VAR==100)THEN
            DO I=1,N
                DO J=1,N
                    BB((J-1)*N+I)=0.D0
                END DO
                BB((I-1)*N+I)=1.D0
            END DO
            F1=F(X,FNLP)
            N1=1
            N2=N
            CALL GRAD(N,F,X,F1,G,N1,N2,HI,NV,FNLP)
            DD=0.D0
            DO I=1,N
                DD=DD+G(I)**2
            END DO
            DD=DSQRT(DD)
            ! ÏE×ATÜ ÈCXOÄHÛX ÄAHHÛX
            IF(.NOT.(PODRP==0.OR.K>1))THEN
                WRITE(*,"(/A/)")    "     MÈHÈMÈÇAÖÈß R-AËÃOPÈTMOM ØOPA: A7"
                WRITE(*,"(A,I3)")   "     PAÇMEPHOCTÜ ÏPOCTPAHCTBA ÏEPEMEHHÛX     N=    ",N
                WRITE(*,"(A,D12.5)")"     KOÝÔÔÈÖÈEHT PACTßÆEHÈß ÏPOCTPAHCTBA     ALPHA=",ALPHA
                WRITE(*,"(A,D12.5)")"     ÏPEÄÏOËAÃAEMOE PACCTOßHÈE ÄO MÈHÈMÓMA   S=    ",S
                WRITE(*,"(A,D12.5)")"     ×ÈCËA,OÏPEÄEËßÞÙÈE KPÈTEPÈÉ OCTAHOBA:   EPS=  ",EPS
                WRITE(*,"(A,I5)")   "                                             KR=   ",KR
                WRITE(*,"(A,D12.5)")"                                             DELTA=",DELTA
                WRITE(*,"(A,D12.5)")"     ØAÃ ÄÈÔÔEPEHÖÈPOBAHÈß:                  HI=   ",HI
                WRITE(*,"(A,I3)")   "     ÏOPßÄOK ÄÈÔÔEPEHÖÈPOBAHÈß:              NV=   ",NV
            END IF
            CALL PRTUCM(0,NF,N,X,F1,DD,SHAGP,PODRP)
            IF(DD<EPS)THEN
                SP_VAR=200
            ELSE
                DO I=1,N
                    G1(I)=G(I)
                    Z(I)=X(I)
                END DO
                K2=0
                K1=1
                DO
                    D=H/DD
                    DO I=1,N
                        X(I)=X(I)-G(I)*D
                    END DO
                    D=F(X,FNLP)
                    IF(D<F1)THEN
                        K1=0
                        F1=D
                        K2=K2+1
                        IF(K2>=5)THEN           
                            H=H*10
                            K2=0
                        END IF
                    ELSE
                        IF(K1==1)THEN
                            DO I=1,N
                                X(I)=Z(I)
                            END DO
                            H=H/2
                        ELSE
                            EXIT
                        END IF
                    END IF
                END DO
                CALL PRTUCM(K,NF,N,X,F1,DD,SHAGP,PODRP)
                D=0.D0
                DO I=1,N
                    D=D+(X(I)-Z(I))**2
                END DO
                D=DSQRT(D)
                H=D/NH
                IF(D<DELTA)THEN
                    SP_VAR=200
                ELSE
                    F1=F(X,FNLP)
                    CALL GRAD(N,F,X,F1,G,N1,N2,HI,NV,FNLP)
                    DD=0.D0
                    DO I=1,N
                        DD=DD+G(I)**2
                    END DO
                    DD=DSQRT(DD)
                    IF(DD<EPS)SP_VAR=200
                    IF(K>=KR)SP_VAR=200
                END IF
            END IF
        END IF
        !
        IF(SP_VAR==100.OR.SP_VAR==101)THEN
            K=K+1
            KK=K
            DO I=1,N
                G2(I)=0.D0
                DO J=1,N
                    G2(I)=G2(I)+BB((I-1)*N+J)*G(J)
                END DO
            END DO
            DO I=1,N
                G1(I)=G1(I)-G2(I)
            END DO
            D=0.D0
            DO I=1,N
                D=D+G1(I)**2
            END DO
            D=DSQRT(D)
            IF(D<1.D-18)THEN
                DO I=1,N
                    G1(I)=G2(I)
                END DO
            ELSE
                DO I=1,N
                    G1(I)=G1(I)/D
                END DO
                DO I=1,N
                    D=0.D0
                    DO J=1,N
                        D=D+BB((J-1)*N+I)*G1(J)
                    END DO
                    D=D*BET
                    DO J=1,N
                        BB((J-1)*N+I)=BB((J-1)*N+I)+G1(J)*D
                    END DO
                END DO
                D=0.D0
                DO I=1,N
                    D=D+G1(I)*G2(I)
                END DO
                D=BET*D
                DO I=1,N
                    G1(I)=G1(I)*D+G2(I)
                END DO
            END IF
            D=0.D0
            DO I=1,N
                D=D+G1(I)**2
            END DO
            D=DSQRT(D)
            DO I=1,N
                G(I)=0
                DO J=1,N
                    G(I)=G(I)+BB((J-1)*N+I)*G1(J)
                END DO
            END DO
            IF(D<(10.D0)**(-18))THEN
                D=0.D0
                DO I=1,N
                    D=D+(X(I)-Z(I))**2
                END DO
                D=DSQRT(D)
                H=D/NH
                SP_VAR=100
            ELSE
                DO I=1,N
                    G(I)=G(I)/D
                END DO
                DO I=1,N
                    Z(I)=X(I)
                END DO
                K1=0
                DO
                    DO I=1,N
                        X(I)=X(I)-H*G(I)
                    END DO  
                    D=F(X,FNLP)
                    K1=K1+1
                    IF(D<F1)THEN
                        F1=D
                    ELSE
                        EXIT
                    END IF
                END DO
                H=K1*H/NH
                F1=D
                CALL GRAD(N,F,X,F1,G,N1,N2,HI,NV,FNLP)
                CALL PRTUCM(K,NF,N,X,F1,DD,SHAGP,PODRP)
                IF(K>=KR)THEN
                    SP_VAR=200
                ELSE
                    DD=0.D0
                    DO I=1,N
                        DD=DD+G(I)**2
                    END DO
                    DD=DSQRT(DD)
                    IF(DD<EPS)THEN
                        SP_VAR=200
                    ELSE
                        D=0.D0
                        DO I=1,N
                            D=D+(X(I)-Z(I))**2
                        END DO
                        D=DSQRT(D)
                        IF(D<DELTA)THEN
                            SP_VAR=200
                        ELSE
                            IF(H>(10.D0)**(10))THEN
                                H=D/NH
                                SP_VAR=100
                            ELSE
                                D=H/D
                                IF(D<=1000.D0)THEN
                                    SP_VAR=101
                                ELSE
                                    DO I=1,N
                                        DO J=1,N
                                            BB((J-1)*N+I)=D*BB((J-1)*N+I)
                                        END DO
                                    END DO
                                    H=H/D
                                    SP_VAR=101
                                END IF
                            END IF
                        END IF
                    END IF
                END IF
            END IF
        END IF
        !
        IF(SP_VAR==200)EXIT
    END DO
! KOHEÖ OCHOBHOÃO ÖÈKËA
    IF(PODRP/=0)WRITE(*,"(/5X,'OÏTÈMAËÜHAß TO×KA')")
    CALL PRTUCM(K,NF,N,X,F1,DD,1,3)
    PAR(3+Q)=KK
    RETURN
END SUBROUTINE A7
