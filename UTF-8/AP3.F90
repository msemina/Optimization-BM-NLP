! МЕТОД СОПРЯЖЁННЫХ ГРАДИЕНТОВ
SUBROUTINE AP3(N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
! ОПИСАНИЕ ПЕРЕМЕННЫХ
    COMMON /C/    NF
    COMMON /AP31/ P1
    COMMON /AP32/ P2
    COMMON /AP33/ G2
    COMMON /AP34/ XV
    COMMON /AP35/ W
    COMMON /AP36/ WN
    REAL(8)::FS,EV,NG1,NG2,C1,C2,C3,C5,CV,F,Y,Y1,Y2,Y3,Y4,YV,BT,H1,YA,CAT,E1,EG,EW,R,C,E,H,NGR
    INTEGER::KK,JI,I,K,PR,PR1,PR2,PN,NS,NN,VAR,D,AA,J,KPN,SHAGP,PODRP,Q
    LOGICAL::B1,B2
    REAL(8),DIMENSION(2)::P1,P2,G2,XV
    REAL(8),DIMENSION(N)::X,A,B,G1
    REAL(8),DIMENSION(40)::PAR
    INTEGER,DIMENSION(2)::W,WN
    EXTERNAL F,FNLP
    ! СЛУЖЕБНЫЕ ПЕРЕМЕННЫЕ
    INTEGER(4)::SP_VAR
! 
    SP_VAR=11111
    E=PAR(1+Q)
    D=PAR(2+Q)
    KK=PAR(3+Q)
    C=PAR(4+Q)
    E1=PAR(5+Q)
    EV=PAR(6+Q)
    PN=PAR(7+Q)
    VAR=PAR(8+Q)
    AA=PAR(9+Q)
    H=PAR(10+Q)
    SHAGP=PAR(11+Q)
    PODRP=PAR(12+Q)
    M=PN
    EW=1.D-10
    EG=E**2
    DO I=1,N
        XV(I)=X(I)
    END DO
    DO I=1,N
        P1(I)=0
        R=X(I)
        IF(R>B(I))THEN
            XV(I)=B(I)
            X(I)=B(I)
        END IF
        IF(R<A(I))THEN
            XV(I)=A(I)
            X(I)=A(I)
        END IF
    END DO
    JI=1
    ! NF=0
    Y=F(X,FNLP)
    H1=H
    NG1=0.D0
    PR=0
    CALL GRAD(N,F,X,Y,G1,1,N,H1,AA,FNLP)
    NS=1
    DO I=1,N
        C1=G1(I)
        B1=.FALSE.
        IF(X(I)>A(I)+EW)B1=.TRUE.
        B2=.FALSE.
        IF(X(I)<B(I)-EW)B2=.TRUE.
        IF((B1.AND.B2).OR.(.NOT.B1.AND.C1<0).OR.(.NOT.B2.AND.C1>0))THEN
            W(NS)=I
            NS=NS+1
            NG1=NG1+C1**2
            P1(I)=C1
        END IF
    END DO
    NS=NS-1
    NGR=DSQRT(NG1)
! ПЕЧАТЬ ИСХОДНЫХ ДАННЫХ
    IF(PODRP/=0)THEN
        WRITE(*,"(/A/)")    '     MИHИMИЗAЦИЯ METOДOM COПPЯЖEHHЫX ГPAДИEHTOB: AP3'
        WRITE(*,"(A,I5)")   '     HOMEP BEPCИИ METOДA                     VAR=',VAR
        WRITE(*,"(A,I5)")   '     PAЗMEPHOCTЬ ЗAДAЧИ                      N=  ',N
        WRITE(*,"(A,D12.5)")'     TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ                 E=  ',E
        WRITE(*,"(A,D12.5)")'     HAЧAЛЬHЫЙ ШAГ CПУCKA                    C=',C
        WRITE(*,"(A,D12.5)")'     TOЧHOCTЬ OДHOMEPHOЙ MИHИMИЗAЦИИ         E1= ',E1
        WRITE(*,"(A,D12.5)")'     MИHИMAЛЬHЫЙ ШAГ CПУCKA                  EV= ',EV
        WRITE(*,"(A,I5)")   '     BOCCTAHOBЛEHИE                          M=  ',M
        WRITE(*,"(A,I5)")   '     MAKCИMAЛЬHOE ЧИCЛO ИTEPAЦИЙ             D=  ',D
        WRITE(*,"(A,I5)")   '     ПOPЯДOK ДИФФEPEHЦИPOBAHИЯ               AA= ',AA
        WRITE(*,"(A,D12.5)")'     ШAГ ДИФФEPEHЦИPOBAHИЯ                   H=  ',H
    END IF
    CALL PRTUCM(K,NF,N,X,Y,NGR,1,PODRP)
! HAЧAЛO ГЛABHOГO ЦИKЛA
    K=0
    DO
        SELECT CASE(SP_VAR)
            CASE(11111)
                K=K+1
                IF(.NOT.(K<=D.AND.NGR>E))THEN
                    EXIT
                END IF
                FS=Y
                CV=C
                PR2=0
                KPN=K/PN
                PR1=0
                IF(KPN==JI)PR1=1
                IF(.NOT.(NS==0))THEN
                    SP_VAR=7
                ELSE
                    H1=H1/10
                    PR1=1
                    SP_VAR=10008
                END IF
            CASE(7)
                IF(NS>=1)THEN
                    DO J=1,NS
                        I=W(J)
                        R=X(I)-CV*P1(I)
                        XV(I)=R
                        IF(R>B(I))XV(I)=B(I)
                        IF(R<A(I))XV(I)=A(I)
                    END DO
                END IF
                SP_VAR=100
            CASE(100)
                DO
                    ! PRINT *, (X(I$),XV(I$),A(I$),B(I$),I$=1,3)
                    YV=F(XV,FNLP)
                    IF(YV>=Y)THEN
                        C2=CV
                        Y2=YV
                        CV=CV/2
                        PR2=1
                        IF(CV<EV)THEN
                            IF(PR==1)THEN
                                WRITE(*,"(A)")'               ШAГCП<MИHШAГ'
                                CV=EV
                                RETURN
                            END IF
                            H1=H1/10.D0
                            PR=1
                            PR1=1
                            SP_VAR=10008
                            EXIT
                        ELSE
                            DO J=1,NS
                                I=W(J)
                                R=X(I)-CV*P1(I)
                                XV(I)=R
                                IF(R>B(I))XV(I)=B(I)
                                IF(R<A(I))XV(I)=A(I)
                            END DO
                        END IF
                    ELSE
                        EXIT
                    END IF
                END DO
                IF(SP_VAR==100)THEN
                    PR=0
                    IF(PR2==1)THEN
                        Y1=YV
                        C1=CV
                        YV=Y
                        CV=0.D0
                        SP_VAR=10003
                    ELSE
                        Y1=YV
                        C1=CV
                        CV=0.D0
                        YV=Y
                        SP_VAR=10002
                    END IF
                END IF
            CASE(10002)
                DO
                    C2=C1*2.D0
                    IF(NS>=1)THEN
                        DO J=1,NS
                            I=W(J)
                            R=X(I)-C2*P1(I)
                            XV(I)=R
                            IF (R>B(I)) XV(I)=B(I)
                            IF (R<A(I)) XV(I)=A(I)
                        END DO
                    END IF
                    ! PRINT *, (X(I$),XV(I$),A(I$),B(I$),I$=1,3)
                    Y2=F(XV,FNLP)
                    IF(Y2<Y1)THEN
                        CV=C1
                        YV=Y1
                        C1=C2
                        Y1=Y2
                    ELSE
                        EXIT
                    END IF
                END DO
                SP_VAR=10003
            CASE(10003)
                C3=(C1-CV)/2.D0
                IF(NS>=1)THEN
                    DO J=1,NS
                        I=W(J)
                        R=X(I)-(CV+C3)*P1(I)
                        XV(I)=R
                        IF (R>B(I)) XV(I)=B(I)
                        IF (R<A(I)) XV(I)=A(I)
                    END DO
                END IF
                ! PRINT *, (X(I$),XV(I$),A(I$),B(I$),I$=1,3)
                Y3=F(XV,FNLP)
                IF(Y3<Y1)THEN
                    C2=C1
                    Y2=Y1
                    C1=CV+C3
                    Y1=Y3
                ELSE
                    YV=Y3
                    CV=CV+C3
                END IF
                SP_VAR=10030
            CASE(10030)
                C5=(C2-C1)/2.D0
                IF(NS>=1)THEN
                    DO J=1,NS
                        I=W(J)
                        R=X(I)-(C5+C1)*P1(I)
                        XV(I)=R
                        IF (R>B(I)) XV(I)=B(I)
                        IF (R<A(I)) XV(I)=A(I)
                    END DO
                END IF
                ! PRINT *, (X(I$),XV(I$),A(I$),B(I$),I$=1,3)
                Y3=F(XV,FNLP)
                IF(Y3<Y1)THEN
                    CV=C1
                    YV=Y1
                    C1=C1+C5
                    Y1=Y3
                ELSE
                    C2=C1+C5
                    Y2=Y3
                END IF
                CAT=C2-CV
                IF(CAT<EV)THEN
                    H1=CAT/5.D0
                    SP_VAR=10007
                ELSE
                    PR2=0
                    SP_VAR=10004
                END IF
            CASE(10004)
                H1=(C2-CV)/5.D0
                IF(H<H1)H1=H
                IF(NS>=1)THEN
                    DO J=1,NS
                        I=W(J)
                        R=X(I)-(C1+H1)*P1(I)
                        XV(I)=R
                        IF (R>B(I)) XV(I)=B(I)
                        IF (R<A(I)) XV(I)=A(I)
                    END DO
                END IF
                ! PRINT *, (X(I$),XV(I$),A(I$),B(I$),I$=1,3)
                YA=F(XV,FNLP)
                IF(NS>=1)THEN
                    DO J=1,NS
                        I=W(J)
                        R=X(I)-(C1-H1)*P1(I)
                        XV(I)=R
                        IF (R>B(I)) XV(I)=B(I)
                        IF (R<A(I)) XV(I)=A(I)
                    END DO
                END IF
                Y3=F(XV,FNLP)
                Y3=(YA-Y3)/2.D0/H1
                IF(DABS(Y3)<E1)THEN
                    SP_VAR=10007
                ELSE
                    IF(PR2>0)THEN
                        SP_VAR=10005
                    ELSE
                        C5=C2
                        IF(Y3>0)C5=CV
                        IF(NS>=1)THEN
                            DO J=1,NS
                                I=W(J)
                                R=X(I)-(C5+H1)*P1(I)
                                XV(I)=R
                                IF (R>B(I)) XV(I)=B(I)
                                IF (R<A(I)) XV(I)=A(I)
                            END DO
                        END IF
                        YA=F(XV,FNLP)
                        IF(NS>=1)THEN
                            DO J=1,NS
                                I=W(J)
                                R=X(I)-(C5-H1)*P1(I)
                                XV(I)=R
                                IF (R>B(I)) XV(I)=B(I)
                                IF (R<A(I)) XV(I)=A(I)
                            END DO
                        END IF
                        Y4=F(XV,FNLP)
                        Y4=(YA-Y4)/2.D0/H1
                        IF(Y4*DSIGN(Y3,Y3)>0.D0)THEN
                            IF(Y3>0.D0)THEN
                                SP_VAR=10003
                            ELSE
                                SP_VAR=10030
                            END IF
                        ELSE
                            IF(Y3>0.D0)THEN
                                C2=C1
                                Y2=Y3
                                YV=Y4
                            ELSE
                                CV=C1
                                YV=Y3
                                Y2=Y4
                            END IF
                            C1=(CV*Y2-C2*YV)/(Y2-YV)
                            PR2=1
                            SP_VAR=10004
                        END IF
                    END IF
                END IF
            CASE(10005)
                IF(Y3<0.D0)THEN
                    CV=C1
                    YV=Y3
                ELSE
                    C2=C1
                    Y2=Y3
                END IF
                IF(C2-CV<EV)THEN
                    SP_VAR=10007
                ELSE
                    IF(PR2==1)THEN
                        C1=(CV+C2)/2.D0
                        PR2=2
                    ELSE
                        C1=(CV*Y2-C2*YV)/(Y2-YV)
                        PR2=1
                    END IF
                    SP_VAR=10004
                END IF
            CASE(10007)
                C=C1
                IF(NS>=1)THEN
                    DO J=1,NS
                        I=W(J)
                        R=X(I)-C*P1(I)
                        XV(I)=R
                        X(I)=R
                        IF(R<A(I))THEN
                            XV(I)=A(I)
                            X(I)=A(I)
                        END IF
                        IF(R>B(I))THEN
                            XV(I)=B(I)
                            X(I)=B(I)
                        END IF
                    END DO
                END IF
                SP_VAR=10008
            CASE(10008)
                DO
                    Y=F(X,FNLP)
                    Y3=0.D0
                    C3=0.D0
                    NG2=0.D0
                    DO I=1,N
                        P2(I)=0.D0
                    END DO
                    CALL GRAD(N,F,X,Y,G2,1,N,H1,AA,FNLP)
                    IF(NS>=1)THEN
                        DO J=1,NS
                            I=W(J)
                            C1=G2(I)
                            NG2=NG2+C1**2
                            Y3=Y3-C1*P1(I)
                            C3=C3+G1(I)*C1
                        END DO
                    END IF
                    IF(PR1==1.OR.NG2<EG)THEN
                        NN=1
                        DO I=1,N
                            C1=G2(I)
                            B1=.FALSE.
                            IF(X(I)>A(I)+EW)B1=.TRUE.
                            B2=.FALSE.
                            IF(X(I)<B(I)-EW)B2=.TRUE.
                            IF((B1.AND.B2).OR.(.NOT.B1.AND.C1<0.D0).OR.(.NOT.B2.AND.C1>0.D0))THEN
                                WN(NN)=I
                                NN=NN+1
                            END IF
                        END DO
                    ELSE
                        NN=1
                        IF(NS>=1)THEN
                            DO J=1,NS
                                I=W(J)
                                IF((X(I)>A(I)+EW).AND.(X(I)<B(I)-EW))THEN
                                    WN(NN)=I
                                    NN=NN+1
                                END IF
                            END DO
                        END IF
                    END IF
                    NN=NN-1
                    IF(NN==0.AND.PR1/=1)THEN
                        H1=H1/10.D0
                        PR1=1
                    ELSE
                        EXIT
                    END IF
                END DO
                PR2=0
                IF(NS==NN)THEN
                    IF(NN>=1)THEN
                        DO J=1,NN
                            IF (W(J)/=WN(J)) PR2=1
                        END DO
                    END IF
                ELSE
                    PR2=1
                END IF
                IF(PR2==1)THEN
                    BT=0.D0
                    NG2=0.D0
                    IF(NN>=1)THEN
                        DO J=1,NN
                            NG2=NG2+G2(WN(J))**2
                        END DO
                    END IF
                    SP_VAR=10009
                ELSE
                    IF(NN==0)THEN
                        SP_VAR=10009
                    ELSE
                        IF(NG1<1.D-12)THEN
                            BT=0.D0
                            SP_VAR=10009
                        ELSE
                            BT=-NG2/NG1
                            IF(VAR==1)BT=(C3-NG2)/NG1
                            IF(DABS(Y3*BT)>NG2/5)THEN
                                BT=0.D0
                                E1=E1/50.D0
                            ELSE
                                E1=1.1D0*E1
                            END IF
                            IF(E1<1.D-7)E1=1.D-7
                            SP_VAR=10009
                        END IF
                    END IF
                END IF
            CASE(10009)
                IF(PR==1)THEN
                    JI=KPN+1
                    BT=0.D0
                END IF
                IF(NN>=1)THEN
                    DO J=1,NN
                        I=WN(J)
                        P2(I)=G2(I)-BT*P1(I)
                        W(J)=I
                    END DO
                END IF
                NS=NN
                NG1=NG2
                NGR=DSQRT(NG1)
                DO I=1,N
                    P1(I)=P2(I)
                    G1(I)=G2(I)
                END DO
                KK=K
                CALL PRTUCM(K,NF,N,X,Y,NGR,SHAGP,PODRP)
                !PRINT *, (X(I),XV(I),A(I),B(I),I=1,3)
                SP_VAR=11111
        END SELECT
    END DO
! KOHEЦ ГЛABHOГO ЦИKЛA
    IF(PODRP/=0)WRITE(*,"(/A)")'     OПTИMAЛЬHAЯ TOЧKA'
    CALL PRTUCM(K,NF,N,X,Y,NGR,1,3)
    PAR(3+Q)=KK
    RETURN
END SUBROUTINE AP3
