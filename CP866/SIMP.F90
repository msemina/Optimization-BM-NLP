! O„POƒPAMMA CˆM‹EKC-METO„A „‹Ÿ PE˜EHˆŸ BCOMOƒATE‹œH›X ‡A„A— B METO„AX H‹
SUBROUTINE SIMP(X1,M1,N1,L1,F1)
! Žˆ‘€ˆ… ……Œ…›•
    COMMON /A20/ SM
    COMMON /A21/ SN
    COMMON /A22/ U1
    COMMON /A23/ Y1
    COMMON /A24/ M
    COMMON /A25/ N3,N4
    INTEGER::M1,N1,L1,R
    INTEGER,DIMENSION(4)::SM
    INTEGER,DIMENSION(9)::SN ! „‹ˆ€ … ‚…€
    REAL(8)::F1,EP,V,Z,TH,D1
    REAL(8),DIMENSION(3)::U1
    REAL(8),DIMENSION(4)::Y1
    REAL(8),DIMENSION(N3,N4)::X1
    ! ‘‹“†…›… ……Œ…›…
    INTEGER::SP_VAR
! ‚›—ˆ‘‹Ÿ…Œ€Ÿ —€‘’œ
    N2=N1+M+1
    DO J=1,N1
        SM(J)=J
        U1(J)=0
    END DO
    DO J=1,M1
        SN(J)=N1+J
    END DO
    EP=DABS(X1(1,2))
    DO I=1,N1
        IF(DABS(X1(1,I+1))>EP) EP=DABS(X1(1,I+1))
    END DO
    EP=100*EP
    N11=N1+1
    DO I=1,N11
        F1=0
        M11=M1-L1+1
        IF(M11<=M1)THEN
            DO J=M11,M1
                F1=F1+X1(J+1,I)
            END DO
        END IF
        X1(1,I)=X1(1,I)-F1*EP
    END DO
    DO I=1,M1
        Y1(I)=0
        IF(I>M1-L1) Y1(I)=EP
    END DO
    EP=1.D-5
    D1=0
    V=0
    SP_VAR=0
    DO
        IF(SP_VAR==0)THEN
            Z=-EP
            R=0
            DO I=1,M1
                IF(X1(I+1,1)<Z)THEN
                    R=I
                    Z=X1(I+1,1)
                END IF
            END DO
            IF(R/=0)THEN
                D1=-1
                TH=1.D+16
                K=0
                DO J=1,N1
                    IF(X1(R+1,J+1)<-EP)THEN
                        Z=-X1(1,J+1)/X1(R+1,J+1)
                        IF(Z<TH)THEN
                            TH=Z
                            K=J
                        END IF
                    END IF
                END DO
                IF(K==0)THEN
                    SP_VAR=60
                    EXIT
                    CYCLE
                ELSE
                    SP_VAR=50
                END IF
            END IF
        END IF
        !
        IF((SP_VAR==0).OR.(SP_VAR==40))THEN
            Z=-EP
            K=0
            DO J=1,N1
                IF(X1(1,J+1)<Z)THEN
                    Z=X1(1,J+1)
                    K=J
                END IF
            END DO
            IF(K==0)THEN
                SP_VAR=0
                EXIT
                CYCLE
            END IF
            TH=1.D+16
            R=0
            DO I=1,M1
                IF(X1(I+1,K+1)>EP)THEN
                    Z=X1(I+1,1)/X1(I+1,K+1)
                    IF(Z<TH)THEN
                        TH=Z
                        R=I
                    END IF
                END IF
            END DO
            IF(R==0)THEN
                SP_VAR=60
                EXIT
                CYCLE
            END IF
            D1=1
        END IF
        !
        IF((SP_VAR==0).OR.(SP_VAR==40).OR.(SP_VAR==50))THEN
            Z=X1(R+1,K+1)
            N11=N1+1
            DO J=1,N11
                IF(J/=K+1)THEN
                    IF(DABS(X1(R+1,J))>EP)THEN
                        TH=X1(R+1,J)/Z
                        M11=M1+1
                        DO I=1,M11
                            IF(I/=R+1)THEN
                                X1(I,J)=X1(I,J)-X1(I,K+1)*TH
                            END IF
                        END DO
                    END IF
                END IF
            END DO
        END IF
        !
        V=V+1
        I=SM(K)
        SM(K)=SN(R)
        SN(R)=I
        N11=N1+1
        DO J=1,N11
            X1(R+1,J)=X1(R+1,J)/Z
        END DO
        M11=M1+1
        DO I=1,M11
            X1(I,K+1)=-X1(I,K+1)/Z
        END DO
        X1(R+1,K+1)=1/Z
        !
        IF(D1>0)SP_VAR=40
    END DO
    !
    IF(SP_VAR==60)X1(1,1)=1.D+15
    F1=X1(1,1)
    IF(X1(1,1)<0.999*1.D+15)THEN
        DO J=1,M1
            IF(SN(J)<=N1) U1(SN(J))=X1(J+1,1)
        END DO
        DO J=1,N1
            IF(SM(J)>N1)THEN
                I=SM(J)-N1
                Y1(I)=Y1(I)-X1(1,J+1)
            END IF
        END DO
        DO I=1,N1
            X1(1,I+1)=U1(I)
        END DO
        DO I=1,M1
            X1(I+1,1)=Y1(I)
        END DO
    END IF
    RETURN
END SUBROUTINE SIMP
