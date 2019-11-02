SUBROUTINE C2(N,L,M,X,A,B,P,F,CGR,CGS,YY,PAR,Q)
! ОПИСАНИЕ ПЕРЕМЕННЫХ
    COMMON /A1/  M1
    COMMON /A2/  GR
    COMMON /A3/  GRF
    COMMON /A4/  HX
    COMMON /A5/  XQ
    COMMON /A6/  U
    COMMON /A7/  YQ
    COMMON /A10/ NFU
    COMMON /A11/ ES
    COMMON /A12/ IS 
    COMMON /A13/ IT
    COMMON /A24/ N5
    COMMON /A25/ N3,N4
    REAL(8)::E,E1,E2,G,C,CC,H,FS,C1,CC2,NF,V1,V2,FA,NR,CQ,ST,SB,B1,B2,B3,B4,FB,NFF,ST1,CRIT
    INTEGER::N,L,M,Q,D,K,KK,IL,IN,IM,J1,J2,I,J,KL,SHAGP,PODRP,ACC,VAR,TEXT
    INTEGER, DIMENSION(5)::IS
    INTEGER, DIMENSION(3)::IT
    REAL(8), DIMENSION(3)::GR,GRF,HX,XQ
    REAL(8), DIMENSION(5)::U
    REAL(8), DIMENSION(6)::YQ
    REAL(8), DIMENSION(91)::ES
    REAL(8), DIMENSION(40)::PAR
    REAL(8), DIMENSION(N)::A,B,X
    REAL(8), DIMENSION(M)::P
    REAL(8), DIMENSION(M1)::YY
    EXTERNAL F,CGR,CGS
    ! СЛУЖЕБНЫЕ ПЕРЕМЕННЫЕ
    INTEGER::SP_VAR
! 
    E=PAR(1)
    D=PAR(2)+KK
    KK=PAR(3)
    VAR=PAR(4)
    SB=PAR(5)
    C=PAR(6)
    G=PAR(7)
    E1=PAR(8)
    H=PAR(9)
    ACC=PAR(10)
    SHAGP=PAR(11)
    PODRP=PAR(12)
    N3=N+2
    N4=2*N+L+M+1
    IF(VAR>1.5)N3=M+2
    IF(VAR>1.5)N4=2*N+2
    N5=M
    NFU=0
    TEXT=0
    CRIT=-1.0
    E2=0.5
    KL=1
    B1=2
    B2=1
    CC=C
    CALL F(X,YY,-1)
    C1=0
    ST=0
    DO I=1,M
        CC2=YY(I)
        IF(I<=L)THEN
            CC2=DABS(CC2)
        ELSE
            IF(CC2<0)CC2=0
        END IF
        IF(CC2>C1) C1=CC2
        ST=ST+CC2
    END DO
    IF(PODRP/=0)THEN
        WRITE(*,"(/A/)")    '     METOД ЛИHEAPИЗAЦИИ: C2'
        WRITE(*,"(A,D12.5)")'     TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ HЛП     E=  ',E
        WRITE(*,"(A,I5)")   '     ЧИCЛO ИTEPAЦИЙ                  D=  ',D
        WRITE(*,"(A,I5)")   '     BEPCИЯ METOДA                   VAR=',VAR
        WRITE(*,"(A,D12.5)")'     BEC OГPAHИЧEHИЙ                 SB= ',SB
        WRITE(*,"(A,D12.5)")'     HAЧAЛЬHЫЙ ШAГ CПУCKA            C=  ',C
        WRITE(*,"(A,D12.5)")'     HAЧAЛЬHЫЙ KOЭФФИЦИEHT ШTPAФA    G=  ',G
        WRITE(*,"(A,D12.5)")'     ПAPAMETP BCПOMOГ. ЗAДAЧИ        E1= ',E1
        WRITE(*,"(A,D12.5)")'     ШAГ ДИФФEPEHЦИPOBAHИЯ           H=  ',H
        WRITE(*,"(A,I5)")   '     ПOPЯДOK ДИФФEPEHЦИPOBAHИЯ       ACC=',ACC
    END IF
    CALL PRTNLP(0,N,L,M,X,YY,P,ST,CRIT,1,4,TEXT)
! HAЧAЛO OCHOBHOГO ЦИKЛA
    DO
        K=KK+1
        IF(.NOT.(K<=D.AND.DABS(CRIT)>E))EXIT
        FS=YY(M1)
        B3=B1/(1+K**B2)
        CALL CGR(F,X,YY,M,GRF,0,1,N,H,ACC)
        NF=0
        DO I=1,N
            NF=NF+GRF(I)**2
        END DO
        NF=DSQRT(NF)
        NFF=(2+NF)*KL
        IF(VAR<1.5)NFF=(2+DSQRT(DFLOAT(N))*NF)*KL
        !
        DO
            IL=0
            IN=0
            V1=C1-E1
            DO I=1,M
                CC2=YY(I)
                IF(I<=L)CC2=DABS(CC2)
                IF(CC2>=V1)THEN
                    IN=IN+1
                    IF(I<=L)IL=IL+1
                    IS(IN)=I
                END IF
            END DO
            IF(IN==0)THEN
                IF(NF<1.D-3)THEN
                    WRITE(*,"(20X,'TOЧKA BHУTP;ГPAД ФУHK<MИH')")
                    CALL PRTNLP(KK,N,L,M,X,YY,P,ST,CRIT,1,4,TEXT)
                    RETURN
                END IF
                IF(VAR<1.5)THEN
                    DO I=1,N
                        HX(I)=-B3
                        IF(GRF(I)<0)HX(I)=B3
                    END DO
                END IF
                IF(VAR>=1.5)THEN
                    FA=0
                    J=0
                    DO I=1,N
                        FB=DABS(GRF(I))
                        IF(FB>FA)FA=FB
                        IT(I)=0
                    END DO
                    DO I=1,N
                        IF(FA-GRF(I)<1.D-5)THEN
                            J=J+1
                            IT(I)=1
                        ELSE
                            IF(GRF(I)+FA<1.D-5)THEN
                                J=J+1
                                IT(I)=-1
                            END IF
                        END IF
                    END DO
                    B4=B3/J
                    DO I=1,N
                        HX(I)=-B4*IT(I)
                    END DO
                END IF
            ELSE
                NZ=N+2
                IF(VAR<1.5)THEN
                    ES(1)=0
                    DO I=1,N
                        ES(I+2)=-GRF(I)
                    END DO
                    ES(2)=NFF
                    IM=IN+IL
                    IF(IM>=1)THEN
                        DO I=1,IM
                            ES(I*NZ+2)=0
                        END DO
                    END IF
                    IF(IN>=1)THEN
                        DO I=1,IN
                            J=IS(I)
                            J1=IL+I
                            CALL CGR(F,X,YY,M,GR,J,1,N,H,ACC)
                            DO J2=1,N
                                ES(J1*NZ+J2+2)=GR(J2)
                            END DO
                            ES(J1*NZ+1)=-YY(J)
                            IF(I<=L)THEN
                                ES(I*NZ+1)=-ES(J1*NZ+1)
                                NN=N+1
                                DO J2=2,NN
                                    ES(I*NZ+J2+1)=-ES(J1*NZ+J2+1)
                                END DO
                            END IF
                        END DO
                    END IF
                    IMM=IM+1
                    IMN=IM+2*N
                    IF(.NOT.(IMN<IMM))THEN
                        DO I=IMM,IMN
                            ES(I*NZ+1)=B3
                            ES(I*NZ+2)=1
                        END DO
                    END IF
                    DO I=1,N
                        DO J=1,N
                            IF(I==J)THEN
                                ES((IM+J)*NZ+I+2)=1
                                ES((IM+N+J)*NZ+I+2)=-1
                            ELSE
                                ES((IM+J)*NZ+I+2)=0
                                ES((IM+N+J)*NZ+I+2)=0
                            END IF
                        END DO
                    END DO
                    CALL SIMP(ES,N+1,IM+2*N,N,FA)
                ELSE
                    MZ=M+2
                    IM=IN-IL
                    ES(1)=0
                    ES((2*N+1)*MZ+1)=NFF
                    DO I=1,N
                        ES(I*MZ+1)=GRF(I)
                        ES((I+N)*MZ+1)=-GRF(I)
                    END DO
                    NN=2*N
                    DO I=1,NN
                        ES(I*MZ+2)=1
                    END DO
                    ES(2)=B3
                    ES((2*N+1)*MZ+2)=-1
                    IF(IN>=1)THEN
                        DO I=1,IN
                            ES((2*N+1)*MZ+I+2)=0
                            J=IS(I)
                            CALL CGR(F,X,YY,M,GR,J,1,N,H,ACC)
                            J1=I-IL
                            IF(I<=IL)J1=IM+I
                            ES(2+J1)=-YY(J)
                            DO J2=1,N
                               ES(J2*MZ+J1+2)=GR(J2)
                               ES((J2+N)*MZ+J1+2)=-GR(J2)
                            END DO
                        END DO
                    END IF
                    CALL SIMP(ES,IN+1,2*N+1,IL,FA)
                END IF
                IF(FA>=0.999D+15)THEN
                    IF(E1>1.D-8)THEN
                        E1=E1/2
                    ELSE
                        WRITE(*,"(20X,'HET PEШEHИЯ ЗAДAЧИ ЛП')")
                        CALL PRTNLP(KK,N,L,M,X,YY,P,ST,CRIT,1,4,TEXT)
                        RETURN
                    END IF 
                END IF
                IF((FA>=0.999D+15).AND.(E1>1.D-8))CYCLE
                IF(VAR<1.5)THEN
                    DO I=1,N
                        HX(I)=ES(2+I)
                    END DO
                    IF(IL>=1)THEN
                        DO I=1,IL
                            U(I)=ES((IL+I)*(N+2)+1)-ES(I*(N+2)+1)
                        END DO
                    END IF
                    ILL=IL+1
                    IF(IN>=ILL)THEN
                        DO I=ILL,IN
                            U(I)=ES((IL+I)*(N+2)+1)
                        END DO
                    END IF
                ELSE
                    DO I=1,N
                        HX(I)=ES(I*(M+2)+1)-ES((I+N)*(M+2)+1)
                    END DO
                    IF(IL>=1)THEN
                        DO I=1,IL
                            U(I)=-ES(IM+2+I)
                        END DO
                    END IF
                    ILL=IL+1
                    IF(IN>=ILL)THEN
                        DO I=ILL,IN
                            U(I)=-ES(I-IL+2)
                        END DO
                    END IF
                END IF
            END IF
            CC2=0
            IF(IN>=1)THEN
                DO I=1,IN
                    CC2=CC2+DABS(U(I))
                END DO
            END IF
            IF(G<CC2.OR.G>4*CC2)G=2*CC2
            IF(G<0.1)G=0.1
            DO I=1,M
                P(I)=0
            END DO
            IF(IN>=1)THEN
                DO I=1,IN
                    P(IS(I))=U(I)
                END DO
            END IF
            NR=0
            DO I=1,N
                NR=NR+GRF(I)*HX(I)
            END DO
            NR=NR-G*C1
            V1=FS+G*C1
            CQ=CC
            DO
                DO I=1,N
                    XQ(I)=X(I)+CQ*HX(I)
                END DO
                CALL F(XQ,YQ,-1)
                C1=0
                ST1=0
                DO I=1,M
                    V2=YQ(I)
                    IF(I<=L)THEN
                        V2=DABS(V2)
                    ELSE
                        IF(V2<0)V2=0
                    END IF
                    IF(V2>C1)C1=V2
                    ST1=ST1+V2
                END DO
                V2=YQ(M1)+G*C1
                SP_VAR=0
                IF(.NOT.(V2<V1+CQ*E2*NR))THEN
                    IF(CQ<1.D-8)THEN
                        IF(IN<M)THEN
                            E1=E1*2
                            SP_VAR=1
                            EXIT
                        ELSE
                            WRITE(*,"(20X,'ШAГ CПУCKA<MИHШAГ')")
                            CALL PRTNLP(KK,N,L,M,X,YY,P,ST,CRIT,1,4,TEXT)
                            RETURN
                        END IF
                    ELSE
                        CQ=CQ/2
                    END IF
                ELSE
                    EXIT
                END IF
            END DO
            IF(SP_VAR==0)EXIT
        END DO
        !
        DO I=1,N
            X(I)=XQ(I)
        END DO
        DO I=1,M1
            YY(I)=YQ(I)
        END DO
        LL=L+1
        IF(M>=LL)THEN
            DO I=LL,M
                IF(YY(I)<-E1)P(I)=0
            END DO
        END IF
        CC=4*CQ
        ST=ST1
        IF(CC<0.001)CC=0.001
        IF(CC>C) CC=C
        KK=K
        CRIT=DABS(YY(M1)-FS)/(1+DABS(FS))+SB*ST
        CALL PRTNLP(KK,N,L,M,X,YY,P,ST,CRIT,SHAGP,PODRP,0)
    END DO
! KOHEЦ OCHOBHOГO ЦИKЛA
    IF(PODRP>0.AND.CRIT<=E)TEXT=2
    IF(PODRP>0.AND.K==D.AND.TEXT/=2)TEXT=3
    IF(PODRP/=0)WRITE(*,"(/32X,'OПTИMAЛЬHAЯ TOЧKA')")
    CALL PRTNLP(KK,N,L,M,X,YY,P,ST,CRIT,1,4,TEXT)
    RETURN
END SUBROUTINE C2
