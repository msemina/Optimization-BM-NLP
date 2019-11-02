! ÌÅÒÎÄ ÕÓÊÀ-ÄÆÈÂÑÀ
SUBROUTINE AP4(N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
! ÎÏÈÑÀÍÈÅ ÏÅÐÅÌÅÍÍÛÕ
    COMMON /C/    NF
    COMMON /AP41/ X1
    COMMON /AP42/ X2
    COMMON /AP43/ ST
    COMMON /AP44/ SS
    COMMON /AP45/ SF
    REAL(8)::F,Y,TG,R1,P,T,E,C,C1
    INTEGER::Q,D,SHAGP,PODRP
    REAL(8), DIMENSION(2)::X1,X2,ST
    REAL(8), DIMENSION(40)::PAR
    REAL(8), DIMENSION(N)::X,A,B,G1
    INTEGER, DIMENSION(2)::SS,SF
    EXTERNAL F,FNLP
! 
    E=PAR(1+Q)
    D=PAR(2+Q)
    KK=PAR(3+Q)
    C=PAR(4+Q)
    C1=PAR(5+Q)
    TG=PAR(6+Q)
    SHAGP=PAR(7+Q)
    PODRP=PAR(8+Q)
    IF(DABS(C1)>1.D0)C1=0.5D0
    DO I=1,N
        IF(A(I)>B(I))THEN
            P=A(I)
            A(I)=B(I)
            B(I)=P
        END IF
        IF(X(I)<A(I))X(I)=A(I)
        IF(X(I)>B(I))X(I)=B(I)
    END DO
    K=0
    KK=K
    L=1
    NF=0
    Y=F(X,FNLP)
    T=Y
    R1=Y
    DO I=1,N
        X1(I)=X(I)
        X2(I)=X(I)
        SF(I)=0
        SS(I)=0
        ST(I)=C
    END DO
! ÏE×ATÜ ÈCXOÄHÛX ÄAHHÛX
    IF(PODRP/=0)THEN
        WRITE(*,"(/A/)")    '     MÈHÈMÈÇAÖÈß METOÄOM ÏPßMOÃO ÏOÈCKA: AP4'
        WRITE(*,"(A,D12.5)")'     TO×HOCTÜ PEØEHÈß ÇAÄA×È             E= ',E
        WRITE(*,"(A,I5)")   '     ×ÈCËO ÈTEPAÖÈÉ                      D= ',D
        WRITE(*,"(A,D12.5)")'     HA×AËÜHÛÉ ØAÃ CÏÓCKA                C= ',C
        WRITE(*,"(A,D12.5)")'     KOÝÔÔÈÖÈEHT ÓMEHÜØEHÈß ØAÃA         C1=',C1
        WRITE(*,"(A,D12.5)")'     TO×HOCTÜ BÛÏOËHEHÈß OÃPAHÈ×EHÈÉ     TG=',TG
        WRITE(*,"(A,I5)")   '     ×ÈCËO OÏTÈMÈÇÈPÓEMÛX ÏAPAMETPOB     N= ',N
    END IF 
    CALL PRTUCM(K,NF,N,X,Y,0,1,PODRP)
    K=1
    KK=1
! HA×AËO OCHOBHOÃO ÖÈKËA. ÏOÈCK ÏO KOOPÄÈHATHÛM HAÏPABËEHÈßM
    DO
        DO I=1,N
            P=X2(I)
            IF(X2(I)<(B(I)-TG))THEN
                X2(I)=P+ST(I)
                IF(X2(I)>B(I))X2(I)=B(I)
                R1=F(X2,FNLP)
            END IF
            IF(T<R1)THEN
                IF(P>(A(I)+TG))THEN
                    X2(I)=P-ST(I)
                    IF(X2(I)<A(I)) X2(I)=A(I)
                    R1=F(X2,FNLP)
                END IF
            END IF
            IF(R1>=T)THEN
                SF(I)=SF(I)+1.D0
                SS(I)=0
                X2(I)=P
                ST(I)=ST(I)*2.D0**(-SF(I))
                IF(ST(I)<(16.D0)**(-13)) ST(I)=(16.D0)**(-13)
                CYCLE
            END IF
            T=R1
            SS(I)=SS(I)+1
            SF(I)=0.D0
            ST(I)=ST(I)*2.D0**SS(I)
        END DO
        IF(Y<=T)THEN
            ! ÏOÈCK HEÓÄA×EH BAPÈAHTÛ A B
            IF(L<=1)THEN
                ! B ÈÇ TEKÓÙEÃO MÈHÈMÓMA
                P=0
                DO I=1,N
                    P=P+ST(I)
                    ST(I)=ST(I)*C1
                    IF(ST(I)<(16.D0)**(-13)) ST(I)=(16.D0)**(-13)
                END DO
                IF(DABS(P/N)<E)EXIT
                CYCLE
            END IF
            ! A ÈÇ TO×KÈ ÝKCTPAÏOËßÖÈÈ
            L=1
            DO I=1,N
                X2(I)=X(I)
            END DO
            T=Y
            R1=T
            CYCLE
        END IF
        ! ÓCÏEØHÛÉ ÏOÈCK. ÈTEPAÖÈß ÇABEPØAETCß
        Y=T
        DO I=1,N
             X(I)=X2(I)
        END DO
        IF(K==D)EXIT
        CALL PRTUCM(K,NF,N,X,Y,0,SHAGP,PODRP)
        K=K+1
        KK=K
        ! HA×AËO HOBOÉ ÈTEPAÖÈÈ TO×KA ÝKCTPAÏOËßÖÈÈ
        DO I=1,N
            X2(I)=2*X(I)-X1(I)
            IF(X2(I)>B(I)) X2(I)=B(I)
            IF(X2(I)<A(I)) X2(I)=A(I)
        END DO
        ! BCÏOMOÃATEËÜHÛE OÏEPAÖÈÈ
        IF(L>1)THEN
            DO I=1,N
                X1(I)=X(I)
            END DO
        END IF
        L=L+1
        R1=F(X2,FNLP)
        T=R1
    END DO
! KOHEÖ ÏOÈCKA ÏO KOOPÄÈHATHÛM HAÏPABËEHÈßM
    IF(PODRP/=0)WRITE(*,"(/A)")'     OÏTÈMAËÜHAß TO×KA'
    CALL PRTUCM(K,NF,N,X,Y,0,1,3)
    PAR(3+Q)=KK
    RETURN
END SUBROUTINE AP4
