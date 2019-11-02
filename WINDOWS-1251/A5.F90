SUBROUTINE A5(N,X,A,B,F,GRAD,AGS,Y,G,Q,PAR,FNLP)
! ÎÏÈÑÀÍÈÅ ÌÅÒÎÄÀ
    !******************************************************************
    !*     ÝTA ÏPOÖEÄÓPA MÈHÈMÈÇÈPÓET ÔÓHKÖÈÞ F(X) N ÏEPEMEHHÛX       *
    !* X(1),...,X(N) METOÄOM ÏAÓÝËËA.   BHA×AËE BEKTOP X COÄEPÆÈT     *
    !* CTAPTOBÓÞ TO×KÓ,B PEÇÓËÜTATE PAÁOTÛ ÏPOÃPAMMÛ B X ÁÓÄET HA-    *
    !* XOÄÈTÜCß HAÉÄEHHAß TO×KA MÈHÈMÓMA ,C OØÈÁKOÉ                   *
    !*           /ERROR/<SQRT(MACHEPS)*/X/+T, ÃÄE                     *
    !* MACHEPS-ÝTO MAØÈHHAß TO×HOCTÜ_HAÈMEHÜØEE ×ÈCËO, TAKOE ×T  O    *
    !* 1+MACHEPS>1,T-TO×HOCTÜ,/./_2-HOPMA.                            *
    !* ÏOÄÏPOÃPAMMA MAØÈHHO HEÇABÈCÈMA,ÇA ÈCKËÞ×EHÈEM ÇHA×EHÈß        *
    !* MACHEPS.  ÏPEÄÏOËAÃAETCß,×TO MACHEPS**(-4) HE BÛÇÛBAET ÏEPE-   *
    !* ÏOËHEHÈß (ECËÈ ÝTO ÏPOÈCXOÄÈT,MACHEPS ÄOËÆHO ÁÛTÜ ÓBEËÈ×EHO)   *
    !* È ×TO B CËÓ×AE ÈC×EÇHOBEHÈß ÏOPßÄKA C ÏËABAÞÙEÉ ÇAÏßTOÉ,PE-    *
    !* ÇÓËÜTAT ÏPÈPABHÈBAETCß K HÓËÞ.                                 *
    !* ECËÈ OCÈ ÏËOXO MACØTAÁÈPOBAHÛ,CËEÄÓET ÏOËOÆÈTÜ SCBD=10,        *
    !* B ÏPOTÈBHOM CËÓ×AE 1.                                          *
    !* ECËÈ ÈÇBECTHO,×TO ÇAÄA×A ÏËOXO OÁÓCËOBËEHA,ÏOËOÆÈTÜ ILLC       *
    !* B ÏPOTÈBHOM CËÓ×AE .FALSE.                                     *
    !* KTM+1_ÝTO ×ÈCËO ÈTEPAÖÈÉ ÁEÇ ÓËÓ×ØEHÈß,ÏOCËE KOTOPOÃO          *
    !* AËÃOPÈTM KOH×AET CBOÞ PAÁOTÓ.KTM=4_O×EHÜ OCTOPOÆHÛÉ KPÈTEPÈÉ   *
    !* OCTAHOBA,OÁÛ×HO KTM=1 BÏOËHE ÄOCTATO×HO.                       *
    !******************************************************************
! ÎÏÈÑÀÍÈÅ ÏÅÐÅÌÅÍÍÛÕ
    COMMON /C/    NF
    COMMON /A51/  T
    COMMON /A52/  Z
    COMMON /A53/  V
    COMMON /A54/  Q0
    COMMON /A55/  Q1
    COMMON /A56/  E1
    COMMON /A57/  XI
    COMMON /GLOB/ QF1,QD1,QD0,QA,QB,QC,FX,NL
    COMMON /BREN/ H,E,M2,M4,LDT,DMIN,MACHEP,SMALL
    REAL(8)::F,Y,S,SL,DN,DMIN,FX,F1,LDS,LDT,SF,DF,M2,M4,SMALL,VSMALL,LARGE,VLARGE,SCBD,LDFAC,T2,H,MACHEP,C,E,QF1,QD0,QD1,QA,QB,QC
    INTEGER::D,SHAGP,PODRP,IL,KK,KTM,K2,KL,KT,NF,NL,IT,Q
    LOGICAL::ILLC
    REAL(8),DIMENSION(40)::PAR
    REAL(8),DIMENSION(N)::X,A,B,G
    REAL(8),DIMENSION(4)::V ! ÄËÈÍÀ Â ÁÀÉÒÀÕ ÂÅÐÍÀ, ÍÎ ÝÒÎ ÌÀÒÐÈÖÀ (N,N)
    REAL(8),DIMENSION(2)::T,Z,Q0,Q1,E1,XI
    EXTERNAL F,FNLP
! 
    E=PAR(1+Q)
    D=PAR(2+Q)
    KK=PAR(3+Q)
    C=PAR(4+Q)
    SCBD=PAR(5+Q)
    IL=PAR(6+Q)
    SHAGP=PAR(7+Q)
    PODRP=PAR(8+Q)
    KTM=PAR(9+Q)
    H=C
    MACHEP=(16.D0)**(-13)
    SMALL=MACHEP**2
    VSMALL=SMALL**2
    LARGE=1.D0/SMALL
    VLARGE=1.D0/VSMALL
    M2=DSQRT(MACHEP)
    M4=DSQRT(M2)
    ILLC=(IL==1)
    LDFAC=0.01
    IF(ILLC) LDFAC=0.1
    IT=0
    KT=0
    NL=0
    NF=0
    S=F(X,FNLP)
    FX=S
    QF1=FX
    T2=SMALL+DABS(E)
    E=T2
    DMIN=SMALL
    IF(H<(100*E))H=100*E
    LDT=H
    DO I=1,N
        DO J=1,N
            V((J-1)*N+I)=0.D0
            IF(I==J)V((J-1)*N+I)=1.D0
        END DO
    END DO
    QD0=0
    G(1)=0
    DO I=1,N
        Q1(I)=X(I)
    END DO
! ÏE×ATÜ ÈCXOÄHÛX ÄAHHÛX
    IF(PODRP/=0)THEN
        WRITE(*,"(/A/)")    '     MÈHÈMÈÇAÖÈß METOÄOM ÏAÓÝËËA: A5'
        WRITE(*,"(A,D11.4)")'     TO×HOCTÜ PEØEHÈß ÇAÄA×È                E=   ',E
        WRITE(*,"(A,D11.4)")'     HA×AËÜHÛÉ ØAÃ CÏÓCKA                   C=   ',C
        WRITE(*,"(A,D11.4)")'     KOÝÔÔÈÖÈEHT MACØTAÁÈPOBAHÈß            SCBD=',SCBD
        WRITE(*,"(A,I3)")   '     ÏPÈMEHEHÈE CËÓ×AÉHOÃO ÏOÈCKA           IL=  ',IL
        WRITE(*,"(A)")      '     ×ÈCËO ÈTEPAÖÈÉ ÁEÇ ÓËÓ×ØEHÈß,'
        WRITE(*,"(A,I3)")   '     ÏOCËE KOT. AËÃOPÈTM KOH×AET PAÁOTÓ     KTM= ',KTM
        WRITE(*,"(A,I3)")   '     ×ÈÑËÎ ÈÒÅÐÀÖÈÉ                         D=   ',D
    END IF
    CALL PRTUCM(IT,NF,N,X,FX,0,1,PODRP)
! HA×AËO OCHOBHOÃO ÖÈKËA
    IT=1
    IY=1
    KK=1
    IF(IT/=D)THEN
        DO
            SF=G(1)
            S=0
            G(1)=0
            ! MÈHÈMÈÇAÖÈß BÄOËÜ ÏEPBOÃO HAÏPABËEHÈß
            CALL MINIM(1,2,G(1),S,FX,.FALSE.,N,X,V,Q0,Q1,XI,F,FNLP)
            IF(S<=0)THEN
                DO I=1,N
                    V(I)=-V(I)
                END DO
            END IF
            IF(SF<=(0.9*G(1)).OR.(0.9*SF)>=G(1))THEN
                DO I=2,N
                    G(I)=0
                END DO
            END IF
            DO K=2,N
                DO I=1,N
                    T(I)=X(I)
                END DO
                SF=FX
                ILLC=ILLC.OR.(KT>0)
                DO
                    KL=K
                    DF=0
                    IF(ILLC)THEN
                        ! CËÓ×AÉHÛÉ ØAÃ ÄËß ÓCTPAHEHÈß ÏPOÁËEMÛ OBPAÃA
                        DO I=1,N
                            Z(I)=(0.1*LDT+T2*10**KT)*(URAND(IY)-0.5D0)
                            S=Z(I)
                            ! ÏPEÄÏOËAÃAETCß,×TO RANDOM BOÇBPAÙAET CËÓ×AÉHOE ×ÈCËO,
                            ! PABHOMEPHO PACÏPEÄEËEHHOH HA [0,1) È ×TO HEOÁXOÄÈMAß
                            ! ÈHÈÖÈAËÈÇAÖÈß ÃEHEPATOPA CËÓ×AÉHÛX ×ÈCEË ÁÛËA ÏPOBEÄEHA
                            DO J=1,N
                                X(J)=X(J)+S*V((I-1)*N+J)
                            END DO
                        END DO
                        FX=F(X,FNLP)
                    END IF
                    DO K2=K,N
                        SL=FX
                        S=0
                        ! MÈHÈMÈÇAÖÈß BÄOËÜ HECOÏPßÆEHHÛX HAÏPABËEHÈÉ
                        CALL MINIM(K2,2,G(K2),S,FX,.FALSE.,N,X,V,Q0,Q1,XI,F,FNLP)
                        IF(ILLC)THEN
                            S=G(K2)*(S+Z(K2))**2
                        ELSE
                            S=SL-FX
                        END IF
                        IF(DF<S)THEN
                            DF=S
                            KL=K2
                        END IF
                    END DO
                    IF(.NOT.(.NOT.ILLC.AND.(DF<DABS(100*MACHEP*FX))))THEN
                        EXIT
                    ELSE
                        ! HET ÓCÏEXA ÏPÈ ILLC=.FALSE.,TOÃÄA ÏOÏÛTAEMCßEÙE PAÇ C ILLC=.TR
                        ILLC=.TRUE.
                    END IF
                END DO
                K0=K-1
                DO K2=1,K0
                    ! MÈHÈMÈÇAÖÈß BÄOËÜ COÏPßÆEHHÛX HAÏPABËEHÈÉ
                    S=0
                    CALL MINIM(K2,2,G(K2),S,FX,.FALSE.,N,X,V,Q0,Q1,XI,F,FNLP)
                END DO
                F1=FX
                FX=SF
                LDS=0
                DO I=1,N
                    SL=X(I)
                    X(I)=T(I)
                    T(I)=SL-T(I)
                    SL=T(I)
                    LDS=LDS+SL*SL
                END DO
                LDS=DSQRT(LDS)
                IF(LDS>SMALL)THEN
                    ! OTÁPACÛBAEM HAÏPABËEHÈE KL È MÈHÈMÈÇÈPÓEM BÄOËÜ HOBOÃO COÏPßÆEHHOÃO HAÏPABËEHÈß
                    K0=KL-1
                    IF(K0>=K)THEN
                        DO I1=K,K0
                            I=K0+K-I1
                            DO J=1,N
                                V(I*N+J)=V((I-1)*N+J)
                            END DO
                           G(I+1)=G(I)
                        END DO
                    END IF
                    G(K)=0
                    DO I=1,N
                        V((K-1)*N+I)=T(I)/LDS
                    END DO
                    CALL MINIM(K,4,G(K),LDS,F1,.TRUE.,N,X,V,Q0,Q1,XI,F,FNLP)
                    IF(LDS<=0)THEN
                        LDS=-LDS
                        DO I=1,N
                           V((K-1)*N+I)=-V((K-1)*N+I)
                        END DO
                    END IF
                END IF
                LDT=LDFAC*LDT
                IF(LDT<LDS) LDT=LDS
                T2=0
                DO I=1,N
                    T2=T2+X(I)**2
                END DO
                T2=M2*DSQRT(T2)+E
                ! AHAËÈÇ: ÏPEBÛØAET ËÈ ÄËÈHA ØAÃA ÏOËOBÈHÓ TO×HOCTÈ
                KT=KT+1
                IF(LDT>(0.5*T2)) KT=0
                IF(KT>KTM.OR.IT==D)THEN
                    Y=FX
                    IF(PODRP/=0)WRITE(*,"(/5X,'OÏTÈMAËÜHAß TO×KA')")
                    CALL PRTUCM(IT,NF,N,X,FX,0,1,3)
                    PAR(3+Q)=KK
                    RETURN
                END IF
            END DO
            ! KBAÄPATÈ×HAß ÝKCTPAÏOËßÖÈß, B CËÓ×AE ÇACTPEBAHÈß ÏOÈCKA B ÈCKPÈBËEHHOM OBPAÃE
            CALL QUAD(N,X,Q0,Q1,V,XI,F,FNLP)
            DN=0
            DO I=1,N
                G(I)=1/DSQRT(G(I))
                IF(DN<G(I)) DN=G(I)
            END DO
            DO J=1,N
                S=G(J)/DN
                DO I=1,N
                    V((J-1)*N+I)=S*V((J-1)*N+I)
                END DO
            END DO
            IF(SCBD>1.0001)THEN
            ! MACØTAÁÈPOBAHÈE OCEÉ C ÖEËÜÞ ÈÇMEHEHÈß ×ÈCËA OÁÓCËOBËEHHOCTÈ
                S=VLARGE
                DO I=1,N
                    SL=0
                    DO J=1,N
                        SL=SL+V((J-1)*N+I)**2
                    END DO
                    Z(I)=DSQRT(SL)
                    IF(Z(I)<M4) Z(I)=M4
                    IF(S>Z(I)) S=Z(I)
                END DO
                DO I=1,N
                    SL=S/Z(I)
                    Z(I)=1/SL
                    IF(Z(I)>SCBD)THEN
                        SL=1/SCBD
                        Z(I)=SCBD
                    END IF
                END DO
            END IF
            ! TPAHCÏOHÈPOBAHÈE V ÄËß MINFIT
            DO I=2,N
                K0=I-1
                DO J=1,K0
                    S=V((J-1)*N+I)
                    V((J-1)*N+I)=V((I-1)*N+J)
                    V((I-1)*N+J)=S
                END DO
            END DO
            ! HAXOÆÄEHÈß CÈHÃÓËßPHOÃO PAÇËOÆEHÈß V. ÝTO ÄAET COÁCTBEHHÛE
            ! ÇHA×EHÈß È ÃËABHÛE OCÈ AÏÏPOKCÈMÈPÓÞÙEÉ KBAÄPATÈ×HOÉ ÔOPMÛ
            ! ÁEÇ BOÇBEÄEHÈß B KBAÄPAT ×ÈCËA OÁÓCËOBËEHHOCTÈ.
            CALL MINFIT(N,MACHEP,VSMALL,V,G,E1)
            IF(SCBD>1.0001)THEN
                DO I=1,N
                    S=Z(I)
                    DO J=1,N
                        V((J-1)*N+I)=S*V((J-1)*N+I)
                    END DO
                END DO
                DO I=1,N
                    S=0
                    DO J=1,N
                        S=S+V((I-1)*N+J)**2
                    END DO
                    S=DSQRT(S)
                    G(I)=S*G(I)
                    S=1/S
                    DO J=1,N
                        V((I-1)*N+J)=S*V((I-1)*N+J)
                    END DO
                END DO
            END IF
            DO I=1,N
                IF(DN*G(I)>LARGE)THEN
                    G(I)=VSMALL
                ELSE
                    IF(DN*G(I)<SMALL)THEN
                        G(I)=VLARGE
                    ELSE
                        G(I)=(DN*G(I))**(-2)
                    END IF
                END IF
            END DO
            ! COPTÈPOBKA HOBÛX COÁCTBEHHÛX ÇHA×EHÈÉ È COÁCTBEHHÛX BEKTOPOB
            CALL SORT(N,G,V)
            DMIN=G(N)
            IF(DMIN<SMALL) DMIN=SMALL
            ILLC=(M2*G(1))>DMIN
            CALL PRTUCM(IT,NF,N,X,FX,0,SHAGP,PODRP)
            IT=IT+1
            KK=IT
        END DO
    END IF
! KOHEÖ OCHOBHOÃO ÖÈKËA
    Y=FX
    IF(PODRP/=0)WRITE(*,"(/5X,'OÏTÈMAËÜHAß TO×KA')")
    CALL PRTUCM(IT,NF,N,X,FX,0,1,3)
    PAR(3+Q)=KK
    RETURN
CONTAINS
!--------------------------------------------------------------------------------------------------------------------
FUNCTION URAND(IY) ! DONE
! ÎÏÈÑÀÍÈÅ ÏÎÄÏÐÎÃÐÀÌÌÛ
    ! URAND - ÝTO ÄAT×ÈK PABHOMEPHO PACÏPEÄEËEHHÛX CËÓ×AÉHÛX ×ÈCEË
    ! ÏEPEÄ ÏEPBÛM OÁPAÙEHÈEM K URAND ÖEËOÉ ÏEPEMEHHOÉ IY CËEÄÓET
    ! ÏPÈCBOÈTÜ ÏPOÈÇBOËÜHOE ÖEËO×ÈCËEHHOE HA×AËÜHOE ÇHA×EHÈE. BÛÇÛ-
    ! BAÞÙAß ÏPOÃPAMMA HE ÄOËÆHA MEHßTÜ IY. ÇHA×EHÈß ÔÓHKÖÈÈ URAND ßB-
    ! ËßÞTCß ×ÈCËAMÈ ÈÇ ÈHTEPBAËA(0,1).
! ÎÏÈÑÀÍÈÅ ÏÅÐÅÌÅÍÍÛÕ
    INTEGER::IA,IC,ITWO,M2,M,MIC,IY 
    REAL(8)::S,HALFM,URAND
    DATA M2/0/,ITWO/2/
! 
    IF(M2==0)THEN
        ! ECËÈ ÝTO ÏEPBÛÉ BXOÄ, TO BÛ×ÈCËÈTÜ ÄËÈHÓ ÖEËO×ÈCËEHHOÃO MAØÈHHOÃO ÖËO
        M=1
        DO
            M2=M
            M=ITWO*M2
            IF(M<=M2)EXIT
        END DO
        HALFM=M2
        ! BÛ×ÈCËÈTÜ MHOÆÈTEËÜ È ÏPÈPAÙEHÈE ËÈHEÉHOÃO KOHÃPÓEHTHOÃO METOÄA
        IA=8*IDINT(HALFM*DATAN(1.D0)/8.D0)+5
        IC=2*IDINT(HALFM*(0.5D0-DSQRT(3.D0)/6.D0))+1
        MIC=(M2-IC)+M2
        ! S - MACØTAÁÈPÓÞÙÈÉ MHOÆÈTEËÜ ÄËß ÏPEOÁPAÇOBAHÈß B ×ÈCËO C ÏËABAÞÙEÉ TO×KOÉ
        S=0.5/HALFM
        ! BÛ×ÈCËÈTÜ CËEÄÓÞÙEE CËÓ×AÉHOE ×ÈCËO
    END IF
    IY=IY*IA
    ! CËEÄÓÞÙÈÉ OÏEPATOP ÄËß MAØÈH, KOTOPÛE HE ÄOÏÓCKAÞT ÏEPEÏOËHEHÈß ÖEËÛX ×ÈCEË ÏPÈ CËOÆEHÈÈ
    IF(IY>MIC)IY=(IY-M2)-M2
    IY=IY+IC
    ! CËEÄÓÞÙÈÉ OÏEPATOP ÄËß MAØÈH, Ó KOTOPÛX ÄËÈHA CËOBA ÄËß CËOÆEHÈß ÁOËÜØE, ×EM ÄËß ÓMHOÆEHÈß
    IF(IY/2>M2)IY=(IY-M2)-M2
    ! CËEÄÓÞÙÈÉ OÏEPATOP ÄËß MAØÈH, Ó KOTOPÛX ÏEPEÏOËHEHÈE ÖEËOÃO ×ÈCËA BËÈßET HA ÇHAKOBÛÉ PAÇPßÄ
    IF(IY<0)IY=(IY+M2)+M2
    URAND=FLOAT(IY)*S
    RETURN
END FUNCTION URAND
!--------------------------------------------------------------------------------------------------------------------
SUBROUTINE SORT(N,G,V) ! DONE
! ÎÏÈÑÀÍÈÅ ÏÅÐÅÌÅÍÍÛÕ
    INTEGER::N
    REAL(8)::S
    REAL(8),DIMENSION(N)::G
    REAL(8),DIMENSION(N,N)::V
! COPTÈPOBKA ÝËEMEHTOB BEKTOPA G È COOTBETCTBÓÞÙÈX CTOËÁÖOB MATPÈÖÛ V B ÓÁÛBAÞÙEM ÏOPßÄKE
    K1=N-1
    DO I=1,K1
        K=I
        S=G(I)
        K0=I+1
        IF(N>=K0)THEN
            DO J=K0,N
                IF(G(J)>S)THEN
                    K=J
                    S=G(J)
                END IF
            END DO
        END IF
        IF(K>I)THEN
            G(K)=G(I)
            G(I)=S
            DO J=1,N
                S=V(J,I)
                V(J,I)=V(J,K)
                V(J,K)=S
            END DO
        END IF
    END DO
    RETURN
END SUBROUTINE SORT
!--------------------------------------------------------------------------------------------------------------------
SUBROUTINE QUAD(N,X,Q0,Q1,V,XI,F,FNLP)
! ÎÏÈÑÀÍÈÅ ÏÅÐÅÌÅÍÍÛÕ
    COMMON /GLOB/ QF1,QD1,QD0,QA,QB,QC,FX,NL
    INTEGER::N
    REAL(8)::FX,QF1,QD0,QD1,QA,QB,QC,L,S,F
    REAL(8),DIMENSION(N)::X,Q0,Q1,XI
    REAL(8),DIMENSION(N,N)::V
    EXTERNAL F,FNLP
! ÏOÈCK MÈHÈMÓMA BÄOËÜ KPÈBOÉ,OÏPEÄEËßEMOÉ Q0,Q1,X.
    S=FX
    FX=QF1
    QF1=S
    QD1=0
    DO I=1,N
        S=X(I)
        L=Q1(I)
        X(I)=L
        Q1(I)=S
        QD1=QD1+(S-L)**2
    END DO
    QD1=DSQRT(QD1)
    L=QD1
    S=0
    IF((QD0>0).AND.(QD1>0).AND.(NL>=3*N*N))THEN
        CALL MINIM(0,2,S,L,QF1,.TRUE.,N,X,V,Q0,Q1,XI,F,FNLP)
        QA=L*(L-QD1)/(QD0*(QD0+QD1))
        QB=(L+QD0)*(QD1-L)/(QD0*QD1)
        QC=L*(L+QD0)/(QD1*(QD0+QD1))
    ELSE
        FX=QF1
        QB=0
        QA=0
        QC=1
    END IF
    QD0=QD1
    DO I=1,N
        S=Q0(I)
        Q0(I)=X(I)
        X(I)=QA*S+QB*X(I)+QC*Q1(I)
    END DO
    RETURN
END SUBROUTINE QUAD
!--------------------------------------------------------------------------------------------------------------------
FUNCTION FLIN(L,J,N,X,V,Q0,Q1,XI,F,FNLP)
! ÎÏÈÑÀÍÈÅ ÏÅÐÅÌÅÍÍÛÕ
    COMMON /GLOB/ QF1,QD1,QD0,QA,QB,QC,FX,NL
    REAL(8)::L,QA,QB,QC,QD0,QD1,QF1,FX,F,FLIN
    REAL(8),DIMENSION(N)::X,XI,Q0,Q1
    REAL(8),DIMENSION(N,N)::V
    EXTERNAL FNLP
! ÏOÄÏPOÃPAMMA-ÔÓHKÖÈß OÄHOÉ ÏEPEMEHHOÉ L, KOTOPAß MÈHÈMÈÇÈPÓETCß ÏOÄÏPOÃPAMME MINIM
    IF(J<=0)THEN
        ! ÏOÈCK BÄOËÜ ÏAPAÁOËÈ×ECKOÉ ÏPOCTPAHCTBEHHOÉ KPÈBOÉ
        QA=L*(L-QD1)/(QD0*(QD0+QD1))
        QB=(L+QD0)*(QD1-L)/(QD0*QD1)
        QC=L*(L+QD0)/(QD1*(QD0+QD1))
        DO I=1,N
            XI(I)=QA*Q0(I)+QB*X(I)+QC*Q1(I)
        END DO
    ELSE
        ! ËÈHEÉHÛÉ ÏOÈCK
        DO I=1,N
            XI(I)=X(I)+L*V(I,J)
        END DO
    END IF
    FLIN=F(XI,FNLP)
    RETURN
END FUNCTION FLIN
!--------------------------------------------------------------------------------------------------------------------
SUBROUTINE MINIM(J,NITS,D2,X1,F1,FK,N,X,V,Q0,Q1,XI,F,FNLP)
! ÎÏÈÑÀÍÈÅ ÏÎÄÏÐÎÃÐÀÌÌÛ
    ! ÏOÄÏPOÃPAMMA MÈHÈMÈÇÈPÓET F ÈÇ X B HAÏPABËEHÈÈ V(*,J) ÄËß J>=1
    ! KOÃÄA CÄEËAH KBAÄPATÈ×HÛÉ ÏOÈCK B ÏËOCKOCTÈ,OÏPEÄEËßEMOÉ Q0,Q1
    ! X1_OÖEHKA PACCTOßHÈß ÄO MÈHÈMÓMA_BOÇBPAÙAETCß KAK HAÉÄEHHOE
    ! PACCTOßHÈE ÄO MÈHÈMÓMA.  ECËÈ FK=.TRUE.,TO F1=FLIN(X1).  W
    ! ÏPOTÈBHOM CËÓ×AE X1 È F1 ÈÃHOPÈPÓÞTCß ÄO TEX ÏOP,ÏOKA KOHE×HOE
    ! NITS KOHTPOËÈPÓET ×ÈCËO ÏOÏÛTOK,CÄEËAHHÛX ÄËß ÓMEHÜØEHÈß ÈHTEP
    ! BÄBOE. ÏOÁO×HÛE ÝÔÔEKTÛ:  ÈCÏOËÜÇÓÞTCß È MEHßÞTCß X,FX,NF,NL.
    ! ECËÈ J>1, ÈCÏOËÜÇÓÞTCß ÏEPEMEHHÛE Q...  .
    ! ÈCÏOËÜÇÓÞTCß H,N,T,M2,M4,LDT,DMIN,MACHEPS.
! ÎÏÈÑÀÍÈÅ ÏÅÐÅÌÅÍÍÛÕ
    COMMON /GLOB/ QF1,QD1,QD0,QA,QB,QC,FX,NL
    COMMON /BREN/ H,E,M2,M4,LDT,DMIN,MACHEP,SMALL
    REAL(8)::QF1,QD1,QD0,QA,QB,QC,F,D2,X1,F1,X2,XM,F0,F2,FM,D1,T2,S,SF1,SX1,FX,H,E,M2,M4,LDT,DMIN,MACHEP,SMALL
    INTEGER::J,NITS,K
    LOGICAL::FK,DZ
    REAL(8),DIMENSION(N)::X,XI,Q0,Q1
    REAL(8),DIMENSION(N,N)::V
    EXTERNAL F,FNLP
    ! ÑËÓÆÅÁÍÛÅ ÏÅÐÅÌÅÍÍÛÅ
    INTEGER::SP_VAR
! 
    SF1=F1
    SX1=X1
    K=0
    XM=0
    FM=FX
    F0=FX
    DZ=(D2<MACHEP)
! OÏPEÄEËEHÈE ÄËÈHÛ ØAÃA
    S=0
    DO I=1,N
        S=S+X(I)**2
    END DO
    S=DSQRT(S)
    T2=D2
    IF(DZ) T2=DMIN
    T2=M4*DSQRT(DABS(FX)+T2*S*LDT)/DSQRT(T2)+M2*LDT
    S=M4*S+E
    IF(DZ.AND.(T2>S)) T2=S
    IF(T2<SMALL) T2=SMALL
    IF(T2>0.01*H) T2=0.01*H
    IF(FK.AND.(F1<=FM))THEN
        XM=X1
        FM=F1
    END IF
    IF(.NOT.FK.OR.(DABS(X1)<T2))THEN
        IF(X1<0.)THEN
            X1=-T2
        ELSE
            X1=T2
        END IF
        F1=FLIN(X1,J,N,X,V,Q0,Q1,XI,F,FNLP)
    END IF
    IF(F1<=FM)THEN
        XM=X1
        FM=F1
    END IF
    DO
        IF(DZ)THEN
            ! BÛ×ÈCËEHÈE FLIN B ÄPÓÃOÉ TO×KE È OÖEHKA 2-OÉ ÏPOÈÇBOÄHOÉ
            X2=2*X1
            IF(F0<F1) X2=-X1
            F2=FLIN(X2,J,N,X,V,Q0,Q1,XI,F,FNLP)
            IF(F2<=FM)THEN
                XM=X2
                FM=F2
            END IF
            D2=((F1-F0)/X1-(F2-F0)/X2)/(X1-X2)
        END IF
        ! OÖEHKA 1-OÉ ÏPOÈÇBOÄHOÉ B 0
        D1=(F1-F0)/X1-X1*D2
        DZ=.TRUE.
        ! ÏPEÄCKAÇAHÈE MÈHÈMÓMA
        IF(D2<=SMALL)THEN
            X2=-H
            IF(D1<0)X2=H
        ELSE
            X2=-0.5D0*D1/D2
        END IF
        IF(DABS(X2)>H)THEN
            IF(X2<=0.D0)THEN
                X2=-H
            ELSE
                X2=H
            END IF
        END IF
        DO
            ! OÖEHKA F B ÏPEÄÏOËAÃAEMOM MÈHÈMÓME
            F2=FLIN(X2,J,N,X,V,Q0,Q1,XI,F,FNLP)
            IF(K<NITS.AND.F2>F0)THEN
                ! HET ÓCÏEXA, ÏOÏÛTAEMCß CHOBA
                K=K+1
                IF(F0<F1.AND.X1*X2>0)THEN
                    SP_VAR=6
                    EXIT
                END IF
                X2=0.5D0*X2
            ELSE
                SP_VAR=13
                EXIT
            END IF
        END DO
        IF(SP_VAR==13)EXIT
    END DO
    ! ÓBEËÈ×EHÈE C×ET×ÈKA OÄHOMEPHÛX ÏOÈCKOB
    NL=NL+1
    IF(F2<=FM)THEN
        FM=F2
    ELSE
        X2=XM
    END IF
    ! ÏOËÓ×EHÈE HOBOÉ OÖEHKÈ BTOPOÉ ÏPOÈÇBOÄHOÉ
    IF(DABS(X2*(X2-X1))>SMALL)THEN
        D2=((F1-F0)/(X1*(X1-X2))-(FM-F0)/(X2*(X1-X2)))
    ELSE
        IF(K>0) D2=0
    END IF
    IF(D2<=SMALL) D2=SMALL
    X1=X2
    FX=FM
    IF(SF1<FX)THEN
        FX=SF1
        X1=SX1
    END IF
    IF(J>0)THEN
        DO I=1,N
            X(I)=X(I)+X1*V(I,J)
        END DO
    END IF
    RETURN
END SUBROUTINE MINIM
!--------------------------------------------------------------------------------------------------------------------
SUBROUTINE MINFIT(N,EPS,TOL,AB,Q,E)
! ÎÏÈÑÀÍÈÅ ÏÅÐÅÌÅÍÍÛÕ
    REAL(8)::EPS,TOL,C,F,G,H,S,X,Y,Z
    REAL(8),DIMENSION(N)::Q,E
    REAL(8),DIMENSION(N,N)::AB
    ! ÑËÓÆÅÁÍÛÅ ÏÅÐÅÌÅÍÍÛÅ
    INTEGER::SP_VAR
! ÏPÈBEÄEHÈE ÈCXOÄHOÉ MATPÈÖÛ K ÄBÓXÄÈAÃOHAËÜHOÉ ÔOPME C ÏOMOÙÜÞ ÏPEOÁPAÇOBAHÈÉ XAÓCXOËÄEPA
    X=0
    G=0
    DO I=1,N
        E(I)=G
        S=0
        L=I+1
        DO J=I,N
            S=S+AB(J,I)**2
        END DO
        IF(S<TOL)THEN
            G=0
        ELSE
            F=AB(I,I)
            G=DSQRT(S)
            IF(F>=0)G=-DSQRT(S)
            H=F*G-S
            AB(I,I)=F-G
            IF(N>=L)THEN
                DO J=L,N
                    F=0
                    DO K=I,N
                        F=F+AB(K,I)*AB(K,J)
                    END DO
                    F=F/H
                    DO K=I,N
                        AB(K,J)=AB(K,J)+F*AB(K,I)
                    END DO
                END DO
            END IF
        END IF
        Q(I)=G
        S=0
        IF(I<=N)THEN
            IF(N>=L)THEN
                DO J=L,N
                    S=S+AB(I,J)**2
                END DO
            END IF
        END IF
        IF(S>=TOL)THEN
            F=AB(I,I+1)
            G=DSQRT(S)
            IF(F>=0)G=-DSQRT(S)
            H=F*G-S
            AB(I,I+1)=F-G
            IF(N>=L)THEN
                DO J=L,N
                    E(J)=AB(I,J)/H
                END DO
                DO J=L,N
                    S=0
                    DO K=L,N
                        S=S+AB(J,K)*AB(I,K)
                    END DO
                    DO K=L,N
                        AB(J,K)=AB(J,K)+S*E(K)
                    END DO
                END DO
            END IF
        ELSE
            G=0
        END IF
        Y=DABS(Q(I))+DABS(E(I))
        IF(Y>X)X=Y
    END DO
! ÔOPMÈPOBAHÈE PEÇÓËÜTÈPÓÞØEÉ ÄBÓXÄÈAÃOHAËÜHOÉ MATPÈÖÛ ÏPABÛX ÏPEOÁPAÇOBAHÈÉ
    DO I1=1,N
        I=N-I1+1
        SP_VAR=0
        IF(G/=0)THEN
            H=AB(I,I+1)*G
            IF(N<L)THEN
                SP_VAR=261
            ELSE
                DO J=L,N
                    AB(J,I)=AB(I,J)/H
                END DO
                DO J=L,N
                    S=0
                    DO K=L,N
                        S=S+AB(I,K)*AB(K,J)
                    END DO
                    DO K=L,N
                        AB(K,J)=AB(K,J)+S*AB(K,I)
                    END DO
                END DO
            END IF
        END IF
        IF(SP_VAR==0)THEN
            IF(N>=L)THEN
                DO J=L,N
                    AB(I,J)=0
                    AB(J,I)=0
                END DO
            END IF
        END IF
        AB(I,I)=1
        G=E(I)
        L=I
    END DO
    EPS=EPS*X
! ÏPÈBEÄEHÈE ÄBÓXÄÈAÃOHAËÜHOÉ MATPÈÖÛ K ÄÈAÃOHAËÜHOÉ ÔOPME
    DO K1=1,N
        K=N-K1+1
        IKT=0
        DO
            IKT=IKT+1
            IF(IKT>30)E(K)=0
            SP_VAR=0
            DO L1=1,K
                L2=K-L1+1
                L=L2
                IF(DABS(E(L))<=EPS)THEN
                    SP_VAR=54
                    EXIT
                END IF
                IF(DABS(Q(L-1))<=EPS)EXIT
            END DO
            ! AHHÓËÈPOBAHÈE ÝËEMEHTA E(L),ECËÈ L>1
            IF(SP_VAR/=54)THEN
                C=0
                S=1
                IF(K>=L)THEN
                    DO I=L,K
                        F=S*E(I)
                        E(I)=C*E(I)
                        IF(DABS(F)>EPS)THEN
                            G=Q(I)
                            IF(DABS(F)<DABS(G))THEN
                                H=DABS(G)*DSQRT(1+(F/G)**2)
                            ELSE
                                H=0
                                IF(F/=0)H=DABS(F)*DSQRT(1+(G/F)**2)
                            END IF
                            Q(I)=H
                            IF(H==0)THEN
                                H=1
                                G=H
                            END IF
                            C=G/H
                            S=-F/H
                        END IF
                    END DO
                END IF
            END IF
            !
            Z=Q(K)
            IF(L/=K)THEN
                ! ÔOPMÈPOBAHÈE CÄBÈÃA ÄËß QR-ÏPEOÁPAÇOBAHÈß
                X=Q(L)
                Y=Q(K-1)
                G=E(K-1)
                H=E(K)
                F=((Y-Z)*(Y+Z)+G**2)/(2*H*Y)-H/(2*Y)
                G=DSQRT(F*F+1)
                IF(F<0)THEN
                    F=F-G
                ELSE
                    F=F+G
                END IF
                F=((X-Z)*(X+Z)+H*(Y/F-H))/X
                ! O×EPEÄHOÉ ØAÃ QR-ÏPEOÁPAÇOBAHÈß
                S=1
                C=S
                K0=L+1
                IF(K>=K0)THEN
                    DO I=K0,K
                        G=E(I)
                        Y=Q(I)
                        H=S*G
                        G=G*C
                        IF(DABS(F)<DABS(H))THEN
                            Z=DABS(H)*DSQRT(1+(F/H)**2)
                        ELSE
                            Z=0
                            IF(F/=0) Z=DABS(F)*DSQRT(1+(H/F)**2)
                        END IF
                        E(I-1)=Z
                        IF(Z==0)THEN
                            F=1
                            Z=F
                        END IF
                        C=F/Z
                        S=H/Z
                        F=X*C+G*S
                        G=-X*S+G*C
                        H=Y*S
                        Y=Y*C
                        DO J=1,N
                            X=AB(J,I-1)
                            Z=AB(J,I)
                            AB(J,I-1)=X*C+Z*S
                            AB(J,I)=-X*S+Z*C
                        END DO
                        IF(DABS(F)<DABS(H))THEN
                            Z=DABS(H)*DSQRT(1+(F/H)**2)
                        ELSE
                            Z=0
                            IF(F/=0)Z=DABS(F)*DSQRT(1+(H/F)**2)
                        END IF
                        Q(I-1)=Z
                        IF(Z==0)THEN
                            F=1
                            Z=F
                        END IF
                        C=F/Z
                        S=H/Z
                        F=C*G+S*Y
                        X=-S*G+C*Y
                    END DO
                END IF
                E(L)=0
                E(K)=F
                Q(K)=X
            ELSE
                EXIT
            END IF
        END DO
        ! ÔOPMÈPOBAHÈE MACCÈBA Q(K),COCTOßÙEÃO ÈÇ HEOTPÈÖATEËÜHÛX ÝËEMEHTOB
        IF(Z<0)THEN
            Q(K)=-Z
            DO J=1,N
                AB(J,K)=-AB(J,K)
            END DO
        END IF
    END DO
    RETURN
END SUBROUTINE MINFIT
!--------------------------------------------------------------------------------------------------------------------
END SUBROUTINE A5
