SUBROUTINE C7(N,L,M,X,A,B,P,F,CGR,CGS,Y,PAR,Q)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    COMMON /A1/  M1
    COMMON /A2/  GF
    COMMON /A3/  GFT
    COMMON /A4/  AA
    COMMON /A5/  GR
    COMMON /A6/  X1
    COMMON /A7/  X2
    COMMON /A10/ NF
    COMMON /A11/ BB
    COMMON /A12/ Y1
    COMMON /A13/ IPVT
    COMMON /A14/ WORK
    INTEGER::N,L,M,Q,D,I,J,K,PR,ACC,SHAGP,PODRP,KK,TEXT
    INTEGER, DIMENSION(3)::IPVT
    REAL(8)::C,ST,E,H,FS,CA,YA,YB,PZ,RP70,XXX,NGR,EW,CRIT,COND,CONDP1
    REAL(8), DIMENSION(3)::GR,X1,X2
    REAL(8), DIMENSION(15)::GFT,GF
    REAL(8), DIMENSION(25)::AA
    REAL(8), DIMENSION(5)::BB,WORK
    REAL(8), DIMENSION(6)::Y1
    REAL(8), DIMENSION(40)::PAR
    REAL(8), DIMENSION(N)::A,B,X
    REAL(8), DIMENSION(M)::P
    REAL(8), DIMENSION(M1)::Y
    EXTERNAL F,CGR,CGS
! 
    EW=1.D-10
    RP70=EW
    E=PAR(1)
    KK=PAR(3)
    D=PAR(2)+KK
    C=PAR(4)
    ACC=PAR(5)
    H=PAR(6)
    SHAGP=PAR(7)
    PODRP=PAR(8)
    TEXT=0
    CRIT=-1.0
    NF=0
    CALL F(X,Y,-1)
    ST=0
    IF(L<=1)THEN
        DO I=1,L
            ST=ST+DABS(Y(I))
        END DO
    END IF
    L1=L+1
    IF(M<=L1)THEN
        DO I=L1,M
             IF(Y(I)>0) ST=ST+Y(I)
        END DO
    END IF
    YA=1.D-5
    PZ=-RP70
    DO I=1,N
        X1(I)=X(I)
    END DO
    IF(PODRP/=0)THEN
        WRITE(*,"(/A/)")    '     PEÀAKCA÷»OHH€… METOƒ: C7'
        WRITE(*,"(A,D12.5)")'     TO◊HOCT‹ PEÿEH»ﬂ «AƒA◊» HÀœ     E=  ',E
        WRITE(*,"(A,I5)")   '     ◊»CÀO »TEPA÷»…                  D=  ',D
        WRITE(*,"(A,D12.5)")'     HA◊AÀ‹H€… ÿA√ Cœ”CKA            C=  ',C
        WRITE(*,"(A,D12.5)")'     ÿA√ ƒ»‘‘EPEH÷»POBAH»ﬂ           H=  ',H
        WRITE(*,"(A,I5)")   '     œOPﬂƒOK ƒ»‘‘EPEH÷»POBAH»ﬂ       ACC=',ACC
    END IF
    CALL PRTNLP(KK,N,L,M,X,Y,P,ST,CRIT,1,4,TEXT)
    IF(M<=L1)THEN
        DO I=L1,M
            IF(Y(I)>PZ)THEN
                WRITE(*,"(10X,'HA◊AÀ‹HAﬂ TO◊KA HEƒOœ”CT»MA')")
                RETURN
            END IF
        END DO
    END IF
    IF(KK==0)THEN
        IF(L<=1)THEN
            DO I=1,L
                IF(DABS(Y(I))>1.D-4)THEN
                    WRITE(*,"(10X,'HAP”ÿEH€ PABEHCTBA')")
                    RETURN
                END IF
            END DO
        END IF
    END IF
! HA◊AÀO OCHOBHO√O ÷»KÀA
    DO
        K=KK+1
        IF(.NOT.(K<D+1.AND.DABS(CRIT)>E))EXIT
        FS=Y(M1)
        CALL CGR(F,X,Y,M,X2,0,1,N,H,ACC)
        DO I=1,M
            CALL CGR(F,X,Y,M,GR,I,1,N,H,ACC)
            DO J=1,N
                GF((I-1)*N+J)=GR(J)
                GFT((J-1)*M+I)=GR(J)
            END DO
        END DO
        DO I=1,M
            YB=0
            DO J=1,N
                YB=YB+GFT((J-1)*M+I)*X2(J)
            END DO
            BB(I)=-YB
            DO J=1,M
                CA=0
                DO PR=1,N
                    CA=CA+GFT((PR-1)*M+I)*GF((J-1)*N+PR)
                END DO
                AA((J-1)*M+I)=CA
            END DO
        END DO
        IF(M>=L1)THEN
            DO I=L1,M
                AA((I-1)*M+I)=AA((I-1)*M+I)-Y(I)
            END DO
        END IF
        CALL DECOMP(M,M,AA,COND,IPVT,WORK)
        CONDP1=COND+1
        IF(CONDP1==COND)WRITE(*,"(/10X,'B€PO∆ƒEHHOCT‹ MATP»÷€')")
        IF(CONDP1==COND)RETURN
        CALL SOLVE(M,M,AA,BB,IPVT)
        DO I=1,M
            P(I)=BB(I)
        END DO
        DO I=1,N
            DO J=1,M
                X2(I)=X2(I)+GF((J-1)*N+I)*P(J)
            END DO
        END DO
        DO I=1,M
            P(I)=-P(I)
        END DO
        IF(M>=L1)THEN
            DO I=L1,M
                IF(P(I)<0) P(I)=0
            END DO
        END IF
        CA=C
        DO
            DO I=1,N
                X1(I)=X(I)-CA*X2(I)
            END DO
            CALL F(X1,Y1,-1)
            PR=1
            IF(Y1(M1)>Y(M1)+PZ) PR=0
            IF(M>=L1)THEN
                DO I=L1,M
                    IF(Y1(I)>PZ) PR=0
                END DO
            END IF
            IF(PR==1)THEN
                DO I=1,N
                    X(I)=X1(I)
                END DO
                DO I=1,M1
                    Y(I)=Y1(I)
                END DO
                EXIT
            ELSE
                IF(PR==0.AND.CA>YA)THEN
                    CA=CA/2
                ELSE
                    WRITE(*,"(10X,'ÿA√ Cœ”CKA<ÿA√M»H')")
                    RETURN
                END IF
            END IF
        END DO
        ST=0
        IF(L>=1)THEN
            DO I=1,L
                ST=ST+DABS(Y(I))
            END DO
        END IF
        IF(M>=L1)THEN
            DO I=L1,M
                IF(Y(I)>0) ST=ST+Y(I)
            END DO
        END IF
        KK=K
        CRIT=DABS(FS-Y(M1))/(1+DABS(FS))
        CALL PRTNLP(KK,N,L,M,X,Y,P,ST,CRIT,SHAGP,PODRP,TEXT)
    END DO
! KOHE÷ OCHOBHO√O ÷»KÀA
    IF(PODRP>0.AND.CRIT<=E) TEXT=2
    IF(PODRP>0.AND.KK==D.AND.TEXT/=2)TEXT=3
    IF(PODRP/=0)WRITE(*,"(/32X,'OœT»MAÀ‹HAﬂ TO◊KA')")
    CALL PRTNLP(KK,N,L,M,X,Y,P,ST,CRIT,1,4,TEXT)
    RETURN
END SUBROUTINE C7
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE DECOMP(NDIM,N,A,COND,IPVT,WORK)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    INTEGER::NDIM,N,NM1,I,J,K,KP1,KB,KM1,M
    INTEGER, DIMENSION(N)::IPVT
    REAL(8)::EK,T,ANORM,YNORM,ZNORM,COND
    REAL(8), DIMENSION(N)::WORK
    REAL(8), DIMENSION(NDIM,N)::A
! 
    IPVT(N)=1
    IF(N/=1)THEN
        NM1=N-1
        ! B€◊»CÀ»T‹ 1-HOPM” MATP»÷€ A
        ANORM=0.0
        DO J=1,N
            T=0.0
            DO I=1,N
                T=T+DABS(A(I,J))
            END DO
          IF (T>ANORM) ANORM=T
        END DO
        ! √A”CCOBO »CKÀﬁ◊EH»E C ◊ACT»◊H€M B€¡OPOM BEƒ”ŸE√O ›ÀEMEHTA
        DO K=1,NM1
            KP1=K+1
            ! HA…T» BEƒ”Ÿ»… ›ÀEMEHT
            M=K
            DO I=KP1,N
                IF (DABS(A(I,K))>DABS(A(M,K))) M=I
            END DO
            IPVT(K)=M
            IF (M/=K) IPVT(N)=-IPVT(N)
            T=A(M,K)
            A(M,K)=A(K,K)
            A(K,K)=T
            ! œPOœ”CT»T‹ ›TOT ÿA√, ECÀ» BEƒ”Ÿ»… ›ÀEMEHT PABEH H”Àﬁ
            IF(T/=0.0)THEN
                ! B€◊»CÀ»T‹ MHO∆»TEÀ»
                DO I=KP1,N
                    A(I,K)=-A(I,K)/T
                END DO
                ! œEPECTABÀﬂT‹ » »CKÀﬁ◊AT‹ œO CTOÀ¡÷AM
                DO J=KP1,N
                    T=A(M,J)
                    A(M,J)=A(K,J)
                    A(K,J)=T
                    IF(T/=0.0)THEN
                        DO I=KP1,N
                            A(I,J)=A(I,J)+A(I,K)*T
                        END DO
                    END IF
                END DO
            END IF
        END DO
        ! COND=(1-HOPMA MATP»÷€ A)*(O÷EHKA ƒÀﬂ 1-HOPM€ MATP»÷€,O¡PATHO… K A)
        ! O÷EHKA œOÀ”◊AETCﬂ œOCPEƒCTBOM OƒHO√O ÿA√A METOƒA O¡PATH€X »TEPA÷»… ƒÀﬂ HA»MEH‹ÿE√O 
        ! C»H√”ÀﬂPHO√O BEKTOPA. ›TO TPE¡”ET PEÿEH»ﬂ ƒB”XC»CTEM ”PABHEH»…,
        ! (TPAHCœOH»POBAHHAﬂ ƒÀﬂ A)*Y=E » A*Z=Y,√ƒEE-BEKTOP »«+1 » -1,
        ! B€¡PAH-H€… TAK,◊TO¡€ MAKC»M»«»POBAT‹ BEÀ»◊»H” Y.
        ! ESTIMATE=(1-HOPMA Z)/(1-HOPMA Y)
        ! PEÿ»T‹ C»CTEM” (TPAHCœOH»POBAHHAﬂ ƒÀﬂ A)*Y=E
        DO K=1,N
            T=0.0
            IF(K/=1)THEN
                KM1=K-1
                DO I=1,KM1
                    T=T+A(I,K)*WORK(I)
                END DO
            END IF
            EK=1.0
            IF(T>0.0)EK=-1.0
            IF(A(K,K)==0.0)THEN
                COND=1.0D+32
                RETURN
            END IF
            WORK(K)=-(EK+T)/A(K,K)
        END DO
        DO KB=1,NM1
            K=N-KB
            T=0.0
            KP1=K+1
            DO I=KP1,N
                T=T+A(I,K)*WORK(K)
            END DO
            WORK(K)=T
            M=IPVT(K)
            IF(M/=K)THEN
                T=WORK(M)
                WORK(M)=WORK(K)
                WORK(K)=T
            END IF
        END DO
        YNORM=0.0
        DO I=1,N
            YNORM=YNORM+DABS(WORK(I))
        END DO
        ! PEÿ»T‹ C»CTEM” A*Z=Y
        CALL SOLVE (NDIM,N,A,WORK,IPVT)
        ZNORM=0.0
        DO I=1,N
            ZNORM=ZNORM+DABS(WORK(I))
        END DO
        ! O÷EH»T‹ O¡”CÀOBÀEHHOCT‹
        COND=ANORM*ZNORM/YNORM
        IF(COND>1.0)COND=1.0
        RETURN
        ! CÀ”◊A… MATP»÷€ 1*1
    END IF
    COND=1.0
    IF(A(1,1)/=0.0)RETURN
    ! TO◊HAﬂ B€PO∆ƒEHHOCT‹
    COND=1.0D+32
    RETURN
END SUBROUTINE DECOMP
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SOLVE(NDIM,N,A,B,IPVT)
! Œœ»—¿Õ»≈ œ≈–≈Ã≈ÕÕ€’
    INTEGER::NDIM,N,KB,KM1,NM1,KP1,I,K,M
    INTEGER, DIMENSION(N)::IPVT
    REAL(8)::T
    REAL(8), DIMENSION(NDIM,N)::A
    REAL(8), DIMENSION(N)::B
! 
    IF(N/=1)THEN
        ! œPﬂMO… XOƒ
        NM1=N-1
        DO K=1,NM1
            KP1=K+1
            M=IPVT(K)
            T=B(M)
            B(M)=B(K)
            B(K)=T
            DO I=KP1,N
                B(I)=B(I)+A(I,K)*T
            END DO
        END DO
        ! O¡PATHAﬂ ’Œƒ
        DO KB=1,NM1
            KM1=N-KB
            K=KM1+1
            B(K)=B(K)/A(K,K)
            T=-B(K)
            DO I=1,KM1
                B(I)=B(I)+A(I,K)*T
            END DO
        END DO
    END IF
    B(1)=B(1)/A(1,1)
    RETURN
END SUBROUTINE SOLVE
!-------------------------------------------------------------------------------------------------------------------------------

