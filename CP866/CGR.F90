SUBROUTINE CGR(F,X,Y,M,GR,K,N1,N2,H,A)
! O„POƒPAMMA —ˆC‹EHHOƒO B›—ˆC‹EHˆŸ ƒPA„ˆEHTA „‹Ÿ METO„OB H‹
! Žˆ‘€ˆ… ……Œ…›•
    COMMON /A1/ M1,N
    COMMON /A8/ Y1
    COMMON /A9/ Y2
    INTEGER::A
    REAL(8)::H,XA,XB
    REAL(8),DIMENSION(N)::X,GR
    REAL(8),DIMENSION(M1)::Y
    REAL(8),DIMENSION(6)::Y1,Y2 !„‹ˆ€ … ‚…€ -- ‚…ŽŸ’Ž „‹ˆ€ = M1
! ŠŽ„ Žƒ€ŒŒ›
    DO J=N1,N2
        XA=X(J);XB=H*(1+0.001*DABS(XA));X(J)=XA+XB
        CALL F(X,Y1,K)
        IF(A/=2)THEN
            DO I=1,M1
                Y2(I)=Y(I)
            END DO
        ELSE
            X(J)=XA-XB
            CALL F(X,Y2,K)
            XB=2*XB
        END IF
        KT=K
        IF(K==0)KT=M1
        GR(J)=(Y1(KT)-Y2(KT))/XB
        X(J)=XA
    END DO
!
    RETURN
END SUBROUTINE CGR


