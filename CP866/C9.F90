! åÖíéÑ ùããàèëéàÑéÇ
SUBROUTINE C9(N,L,M,X,A,B,P,F,CGR,CGS,Y,PAR,Q)
! éèàëÄçàÖ èÖêÖåÖççõï
    COMMON /A1/  M1, N__UNUSED, L__UNUSED
    COMMON /A2/  XN
    COMMON /A3/  GN
    COMMON /A4/  G
    COMMON /A5/  G2
    COMMON /A6/  BG
    COMMON /A7/  Y1
    COMMON /A10/ NF
    COMMON /A11/ V,U,ST,EE,JM
    INTEGER::D,K,SHAGP,PODRP,AC,KK,TEXT,SIX,I,J,K1,KD,JM,Q
    REAL(8)::C,H,E,ST,EE,EC,NORMC9,B1,B2,B3,B4,U,V,YR,C1,CS,EM,EL,EG,EX
    REAL(8), DIMENSION(3)::XN,GN,G,G2
    REAL(8), DIMENSION(9)::BG
    REAL(8), DIMENSION(6)::Y1
    REAL(8), DIMENSION(40)::PAR
    REAL(8), DIMENSION(N)::X,A,B
    REAL(8), DIMENSION(M)::P
    REAL(8), DIMENSION(M1)::Y
    EXTERNAL F,CGR
    ! ëãìÜÖÅçõÖ èÖêÖåÖççõÖ
    INTEGER::SP_VAR
! 
    E=PAR(1)
    D=PAR(2)
    KK=PAR(3)
    C=PAR(4)
    EC=PAR(5)
    EE=PAR(6)
    H=PAR(7)
    AC=PAR(8)
    SHAGP=PAR(9)
    PODRP=PAR(10)
    NF=0
    TEXT=0
    SIX=0
    EM=1.D-11
    EL=1.D-16
    EG=1.D+15
    K=KK; D=D+KK
    KD=40*(N+1)
    DO I=1,N
        XN(I)=X(I)
    END DO
    CALL F(X,Y,-1)
    DO I=1,M1
        Y1(I)=Y(I)
    END DO
    B3=C
    B4=C**(2*N)
    CALL STRFUN(L,M,Y1)
    YR=Y1(M1)+EC*ST
    CALL CGR(F,XN,Y,M,GN,JM,1,N,H,AC)
    IF(JM<=L.AND.JM>0)THEN
        DO I=1,N
            GN(I)=GN(I)*Y(JM)/DABS(Y(JM))
        END DO
    END IF  
    IF(PODRP/=0)THEN
        WRITE(*,"(/A/)")    '     METOÑ ùããàèCOàÑOB: C9'
        WRITE(*,"(A,D12.5)")'     TOóHOCTú PEòEHàü áAÑAóà Hãè     E= ',E
        WRITE(*,"(A,I5)")   '     óàCãO àTEPAñàâ                  D= ',D
        WRITE(*,"(A,D12.5)")'     PAÑàìC HAóAãúHOÉO òAPA          C= ',C
        WRITE(*,"(A,D12.5)")'     BEC òTPAîA                      EC=',EC
        WRITE(*,"(A,D12.5)")'     èAPAMETP èPEOÅP.                EE=',EE
        WRITE(*,"(A,D12.5)")'     òAÉ ÑàîîEPEHñàPOBAHàü           H= ',H
        WRITE(*,"(A,I5)")   '     èOPüÑOK ÑàîîEPEHñàPOBAHàü       AC=',AC
    END IF
    CALL PRTNLP(0,N,L,M,X,Y,P,ST,B4,1,4,TEXT)
    B1=DSQRT(DFLOAT(N-1))/DSQRT(DFLOAT(N+1))-1
    B2=N/DSQRT(DFLOAT(N**2-1))
    DO I=1,N
        DO J=1,N
            BG((J-1)*N+I)=0.D0
            IF(I==J)BG((I-1)*N+I)=1.D0
        END DO
    END DO
! HAóAãO OCHOBHOÉO ñàKãA
    SP_VAR=0
    DO
        IF(SP_VAR==0)THEN
            K=KK+1
            IF(.NOT.(K<D+1.AND.B4>E))SP_VAR=2
            IF(SP_VAR/=2)K1=0
        END IF
        IF(SP_VAR==0.OR.SP_VAR==1)THEN
            ! BõóàCãEHàE BEKTOPA PACTüÜEHàü
            DO I=1,N
                G2(I)=0.D0
                DO J=1,N
                    G2(I)=G2(I)+BG((I-1)*N+J)*GN(J)
                END DO
            END DO
            U=NORMC9(G2)
            IF(U<EL)U=EM
            DO I=1,N
                G(I)=-G2(I)/U
            END DO
            ! OèPEÑEãEHàE HOBOÉO ñEHTPA
            DO I=1,N
                G2(I)=0.D0
                DO J=1,N
                    G2(I)=G2(I)+BG((J-1)*N+I)*G(J)
                END DO
                XN(I)=XN(I)+B3*G2(I)/(N+1)
            END DO
            ! áAÑAHàE HOBOâ MATPàñõ
            DO I=1,N
                U=0
                DO J=1,N
                    U=U+BG((J-1)*N+I)*G(J)
                END DO
                U=U*B1
                DO J=1,N
                    BG((J-1)*N+I)=BG((J-1)*N+I)+G(J)*U
                END DO
            END DO
            ! èPOBEPKA BõèOãHEHàü KPàTEPàEB à èEóATú
            CALL F(XN,Y1,-1)
            CALL STRFUN(L,M,Y1)
            B3=B2*B3
            B4=(B1+1)*B4
            CALL CGR(F,XN,Y1,M,GN,JM,1,N,H,AC)
            IF(JM<=L.AND.JM>0)THEN
                DO I=1,N
                    GN(I)=GN(I)*Y1(JM)/DABS(Y1(JM))
                END DO
            END IF
            V=Y1(M1)+EC*ST
            IF(V<YR)THEN
                DO I=1,M1
                    Y(I)=Y1(I)
                END DO
                DO I=1,N
                    X(I)=XN(I)
                END DO
            END IF
            IF(V<YR.OR.B4<=E)THEN
                KK=K
                YR=V
                CALL PRTNLP(KK,N,L,M,X,Y,P,ST,B4,SHAGP,PODRP,TEXT)
                SP_VAR=0
                CYCLE
            END IF
            K1=K1+1
            IF(.NOT.(K1>KD))THEN
                SP_VAR=1
                CYCLE
            ELSE
                EXIT
            END IF
        END IF
        IF(SP_VAR==2)EXIT
    END DO
    IF(SP_VAR/=2)THEN
        SIX=6
        WRITE(*,"(10X,'èPEÑ.óàCãO àTEPAñàâ ÅEá ìãìóòEHàü')")
    END IF
! KOHEñ OCHOBHOÉO ñàKãA
    IF(PODRP/=0)WRITE(*,"(/32X,'OèTàMAãúHAü TOóKA')")
    CALL PRTNLP(K,N,L,M,X,Y,P,ST,B4,1,4,TEXT)
    RETURN
END SUBROUTINE C9
!-------------------------------------------------------------------------------------------------------------------------------
! èPOñEÑìPA BõóàCãEHàü òTPAîA à MAKCàMAãúHOÉO OÉPAHàóEHàü
SUBROUTINE STRFUN(L,M,Y)
! éèàëÄçàÖ èÖêÖåÖççõï
    COMMON /A1/  M1, N__UNUSED, L__UNUSED
    COMMON /A11/ V,U,ST,EE,JM
    REAL(8)::V,U,ST,EE
    INTEGER::JM,L,M
    REAL(8),DIMENSION(M1)::Y
! 
    V=0.D0
    JM=0
    ST=0
    DO I=1,M
        IF(I<=L)THEN
            IF(U>V.AND.U>EE)THEN
                V=U
                JM=I
            END IF
        ELSE
            IF(Y(I)>V)THEN
                V=Y(I)
                JM=I
            END IF
        END IF
        IF(I<=L)THEN
            ST=ST+U
        ELSE
            IF(Y(I)>0)ST=ST+Y(I)
        END IF
    END DO
    RETURN
END SUBROUTINE STRFUN
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION NORMC9(G)
! éèàëÄçàÖ èÖêÖåÖççõï
    COMMON /A1/ M1,N, L__UNUSED
    REAL(8)::P,NORMC9
    REAL(8), DIMENSION(N)::G
! 
    P=0.D0
    DO I=1,N
        P=P+G(I)**2
    END DO
    NORMC9=DSQRT(P)
    RETURN
END FUNCTION NORMC9
!-------------------------------------------------------------------------------------------------------------------------------

