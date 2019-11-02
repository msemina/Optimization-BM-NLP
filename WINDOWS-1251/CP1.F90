SUBROUTINE CP1(N,L,M,X,A,B,P,F,CGR,CGS,Y,PAR,Q)
! ÎÏÈÑÀÍÈÅ ÏÅÐÅÌÅÍÍÛÕ
    COMMON /A1/  M1
    COMMON /A2/  IS
    COMMON /A3/  XA
    COMMON /A4/  HX
    COMMON /A5/  D1
    COMMON /A6/  D2
    COMMON /A7/  GR
    COMMON /A10/ NFU
    COMMON /A11/ ES
    COMMON /A12/ A1
    COMMON /A13/ A2
    COMMON /A14/ YA
    COMMON /A24/ N5
    COMMON /A25/ N3,N4
    INTEGER::N,L,M,Q,I,J,K,KK,R,I1,J1,D,I2,I3,SHAGP,PODRP,ACC,QQ,VAR,TEXT
    REAL(8)::C,E,H,S,S1,FA,FS,CA,E1,ST,C1,EW,B1,B2,B3,B4,B5,B6,V1,V2,V3,NF,ST1,CC,Q1,Q2,R1,R2,SB,CRIT,EBM
    INTEGER, DIMENSION(7)::IS
    REAL(8), DIMENSION(2)::XA,HX,D1,D2,GR
    REAL(8), DIMENSION(50)::ES
    REAL(8), DIMENSION(20)::A1
    REAL(8), DIMENSION(10)::A2
    REAL(8), DIMENSION(4)::YA
    REAL(8), DIMENSION(N)::X,A,B
    REAL(8), DIMENSION(M)::P
    REAL(8), DIMENSION(M1)::Y
    REAL(8), DIMENSION(40)::PAR
    EXTERNAL F,CGR
! 
    E=PAR(1)
    D=PAR(2)
    KK=PAR(3)
    VAR=PAR(4)
    SB=PAR(5)
    C=PAR(6)
    B5=PAR(7)
    E1=PAR(8)
    H=PAR(9)
    ACC=PAR(10)
    SHAGP=PAR(11)
    PODRP=PAR(12)
    CC=C
    TEXT=0
    CRIT=-1.0
    EW=1.D-10
    N1=N+1
    L1=L+1
    N2=N+M+2
    N3=N+M+2
    N4=N+2
    N5=M
    NFU=0
    CALL F(X,Y,-1)
    ST=0
    ST1=0
    DO I=1,M
        C1=Y(I)
        IF(I<=L)THEN
            C1=DABS(C1)
            IF(C1>ST1)ST1=C1
        ELSE
            IF(C1<0)C1=0
        END IF
        ST=ST+C1
    END DO
    IF(PODRP/=0)THEN
        WRITE(*,"(/A/)")            '     METOÄ BOÇMOÆHÛX HAÏPABËEHÈÉ: CP1'
        WRITE(*,"(A,D12.5)")        '     TO×HOCTÜ PEØEHÈß ÇAÄA×È HËÏ     E=  ',E
        WRITE(*,"(A,I5)")           '     ×ÈCËO ÈTEPAÖÈÉ                  D=  ',D
        WRITE(*,"(A,I5)")           '     BEPCÈß METOÄA                   VAR=',VAR
        WRITE(*,"(A,D12.5)")        '     BEC OÃPAHÈ×EHÈÉ                 SB= ',SB
        WRITE(*,"(A,D12.5)")        '     HA×AËÜHÛÉ ØAÃ CÏÓCKA            C=  ',C
        WRITE(*,"(A,D12.5,A,D12.5)")'     ÏAPAMETPÛ BCÏOMOÃ. ÇAÄA×È       B5= ',B5,', E1=',E1
        WRITE(*,"(A,D12.5)")        '     ØAÃ ÄÈÔÔEPEHÖÈPOBAHÈß           H=  ',H
        WRITE(*,"(A,I5)")           '     ÏOPßÄOK ÄÈÔÔEPEHÖÈPOBAHÈß       ACC=',ACC
    END IF
    CALL PRTNLP(0,N,L,M,X,Y,P,ST,CRIT,1,4,TEXT)
    DO I=1,N
        IF(X(I)<A(I)-EW.OR.X(I)>B(I)+EW)THEN
           WRITE(*,"(10X,'HA×.TO×KA BHE ÏAPAËËEËEÏÈÏEÄA')")
           RETURN
        END IF
    END DO
    IF(ST1>0.001)THEN
         WRITE(*,"(10X,'HAPÓØEHÛ PABEHCTBA')")
         RETURN
    END IF
    IF(L/=0)THEN
        DO I=1,L
            CALL CGR(F,X,Y,M,GR,I,1,N,H,ACC)
            DO J=1,N
                A2((J-1)*L1+I+1)=GR(J)
            END DO
            A2(L1*N+I+1)=0
        END DO
    END IF
    DO I=1,N
        A1((I-1)*N1+1)=0
        A1((I-1)*N1+I+1)=1
        A1(N*N1+I+1)=0
        I11=I+1
        IF(I11>N)EXIT
        DO J=I11,N
            A1((I-1)*N1+J+1)=0
            A1((J-1)*N1+I+1)=0
        END DO
    END DO
    A1(N*N1+1)=-1
    B1=2
    B2=1
    IS(1)=0
    B6=B5
    Q1=1
    Q2=1
    R1=1
    R2=1
! HA×AËO OCHOBHOÃO ÖÈKËA
    K=0
    DO
        K=K+1
        IF(K/=1)THEN
            IF(.NOT.(K<D+1.AND.DABS(CRIT)>E))EXIT
        END IF
        FS=Y(M1)
        B3=B1/(1+K**B2)
        B3=1
        V1=0
        IF(L1<=M)THEN
            DO I=L1,M
                IF(Y(I)>V1)V1=Y(I)
            END DO
        END IF
        DO
            IF(VAR==1)THEN
            I1=1
            IF(L1<=M)THEN
                DO I=L1,M
                    IF(Y(I)>=V1-E1)THEN
                        I1=I1+1
                        IS(I1)=I
                    END IF
                END DO
            END IF
            B4=1
            ELSE
            I1=M-L+1
            IF(L1<=M)THEN
                DO I=L1,M
                    IS(I-L+1)=I
                END DO
            END IF
            END IF
            DO I=1,N
            IF(A(I)-X(I)>-B3)D1(I)=A(I)-X(I)
            IF(A(I)-X(I)<=-B3)D1(I)=-B3
            END DO
            DO I=1,N
            D2(I)=B3
            IF(B(I)-X(I)<B3)D2(I)=B(I)-X(I)
            END DO
            IF(VAR==1.AND.I1==1.AND.L==0)THEN
            CALL CGR(F,X,Y,M,GR,0,1,N,H,ACC)
            NF=0
            DO I=1,N
                NF=NF+GR(I)*GR(I)
            END DO
            NF=DSQRT(NF)
            IF(NF<1.D-3)THEN
                WRITE(*,"(10X,'TO×KA BHÓTP. ÃPAÄ. ÔÓHK. <MÈH')")
                IF(L1>M)RETURN
                DO I=L1,M
                    P(I)=0
                END DO
                RETURN
            END IF
            S=0
            DO I=1,N
                HX(I)=D2(I)
                IF(GR(I)>0)HX(I)=D1(I)
                S=S+HX(I)*GR(I)
            END DO
            S=S/B5
            ELSE
                ES(1)=0
                DO I=1,N1
                    DO J=1,N1
                        ES(J*N2+I)=A1((J-1)*N1+I)
                    END DO
                END DO
                DO I=1,N
                    ES(I+1)=D2(I)-D1(I)
                END DO
                DO I=1,I1
                    J=IS(I)
                    J1=N+I
                    CALL CGR(F,X,Y,M,GR,J,1,N,H,ACC)
                    DO R=1,N
                        ES(R*N2+J1+1)=GR(R)
                    END DO
                    IF(VAR==1)THEN
                        ES(N1*N2+J1+1)=B5
                        IF(J>0)ES(N1*N2+J1+1)=1
                    ELSE
                        ES(N1*N2+J1+1)=B6
                        IF(J>0)ES(N1*N2+J1+1)=1
                    END IF
                    IF(VAR==1)THEN
                        ES(J1+1)=Q1*B4*V1
                        IF(J>0)ES(J1+1)=0
                    ELSE
                        ES(J1+1)=V1*Q2
                        IF(J>0)ES(J1+1)=V1-Y(J)
                    END IF
                END DO
                R=N+I1
                IF(L/=0)THEN
                    DO I=1,L
                        R=R+1
                        DO J=1,N1
                            ES(J*N2+R+1)=A2((J-1)*L1+I+1)
                        END DO
                        ES(R+1)=0
                    END DO
                END IF
                IF(N1<=R)THEN
                    DO I=N1,R
                        FA=0
                        DO J=1,N
                            FA=FA+ES(J*N2+I+1)*D1(J)
                        END DO
                        ES(I+1)=ES(I+1)-FA
                    END DO
                END IF
                CALL SIMP(ES,N+L+I1,N+1,L,FA)
                IF(FA>=0.999*1.D+15)THEN
                    IF(E1>1.D-8)THEN
                        E1=E1/2
                        CYCLE
                    END IF
                    WRITE(*,"(10X'HET PEØEHÈß ÇAÄA×È ËÏ')")
                    RETURN
                END IF
                DO I=1,N
                    HX(I)=ES(I*N2+1)+D1(I)
                END DO
                S=-ES(N1*N2+1)
                S1=ES(N+2)
            END IF
            DO I=1,M
                P(I)=0
            END DO
            IF(S1<-1.D-8)THEN
                IF(I1>=2)THEN
                    DO I=2,I1
                        P(IS(I))=ES(N+I+1)/S1
                    END DO
                END IF
                IF(L/=0)THEN
                    DO I=1,L
                        P(I)=ES(N+I1+I+1)/S1
                    END DO
                END IF
            END IF
            IF(VAR==2.OR.VAR==1.AND.S<=-E1)THEN
                CA=CC
                DO
                    DO I=1,N
                        XA(I)=X(I)+CA*HX(I)
                    END DO
                    QQ=0
                    DO I=1,N
                        IF(XA(I)>B(I).OR.XA(I)<A(I))QQ=QQ+1
                    END DO
                    IF(QQ>0)THEN
                        IF(CA<1.D-8)THEN
                            WRITE(*,"(10X,'ØAÃ CÏÓCKA < MÈHØAÃ')")
                            RETURN
                        END IF
                        CA=CA/2
                        CYCLE
                    END IF
                    CALL F(XA,YA,0)
                    V2=(YA(M1)-FS)*R2
                    IF(VAR==1) V2=(YA(M1)-FS)*R1
                    IF(L1<=M)THEN
                    DO I=L1,M
                        CALL F(XA,YA,I)
                        IF(YA(I)>V2)V2=YA(I)
                    END DO
                    END IF
                    IF(V2>V1+S*CA/2)THEN
                        IF(CA<1.D-8)THEN
                            WRITE(*,"(10X,'ØAÃ CÏÓCKA < MÈHØAÃ')")
                            RETURN
                        END IF
                        CA=CA/2
                        CYCLE
                    END IF
                    EXIT
                END DO
                DO I=1,N
                    X(I)=XA(I)
                END DO
                DO I=1,M1
                    Y(I)=YA(I)
                END DO
                CC=4*CA
                IF(CC>C)CC=C
                IF(CC<0.001)CC=0.001
            ELSE
                IF(E1<1.D-8)THEN
                    WRITE(*,"(10X,'TOËÙÈHA CËOß < MÈHCËOÉ')")
                    RETURN
                END IF
                IF(I1>1)THEN
                    E1=E1/2
                    CYCLE
                END IF 
                WRITE(*,"(10X,'TO×KA BHÓTP.ÃPAÄ.ÔÓHK.<MÈH')")
                IF(L1>M)RETURN
                DO I=L1,M
                    P(I)=0
                END DO
                RETURN
            END IF
            EXIT
        END DO
        DO I=1,L1
            CALL F(X,Y,I-1)
        END DO
        ST=0
        ST1=0
        DO I=1,M
            C1=Y(I)
            IF(I<=L)THEN
                C1=DABS(C1)
                IF(C1>ST1)ST1=C1
            ELSE
                IF(C1<0)C1=0
            END IF
            ST=ST+C1
        END DO
        IF(L1<=M)THEN
            DO I=L1,M
                IF(VAR==1.AND.Y(I)<V1-E1.OR.VAR==2.AND.Y(I)<-1.D-1)P(I)=0
            END DO
        END IF
        IF(ST1>0.001)THEN
            WRITE(*,"(10X,'HAPÓØEHÛ PABEHCTBA')")
            RETURN
        END IF
        KK=K
        CRIT=DABS(Y(M1)-FS)/(1+DABS(FS))+SB*ST
        CALL PRTNLP(KK,N,L,M,X,Y,P,ST,CRIT,SHAGP,PODRP,TEXT)
    END DO
! KOHEÖ OCHOBHOÃO ÖÈKËA
    IF(PODRP>0.AND.CRIT<=E)TEXT=2
    IF(PODRP>0.AND.KK==D.AND.TEXT/=2)TEXT=3
    IF(PODRP/=0)WRITE(*,"(/32X,'OÏTÈMAËÜHAß TO×KA')")
    CALL PRTNLP(KK,N,L,M,X,Y,P,ST,CRIT,1,4,TEXT)
    RETURN
END SUBROUTINE CP1
