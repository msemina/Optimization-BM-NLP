SUBROUTINE F(X,Y,J)
!       ÏPÈMEP ÏOÄÃOTOBKÈ ÏOÄÏPOÃPAMMÛ PAC×ETA ÖEËEBOÉ
!    ÔÓHKÖÈÈ È OÃPAHÈ×EHÈÉ ÄËß ÇAÄA×È ÓCËOBHOÉ OÏTÈMÈÇAÖÈÈ
!  ( BAPÈAHT ÁEÇ ßBHOÃO Ó×ETA ÏAPAËËEËEÏÈÏEÄHÛX OÃPAHÈ×EHÈÉ )
!   N,M1 - OÏPEÄEËßÞT PAÇMEPHOCTÜ ÇAÄA×È
    COMMON /A1/ M1,N
!   NF - C×ET×ÈK BÛ×ÈCËEHÈÉ ÔÓHKÖÈÈ È OÃPAHÈ×EHÈÉ
    COMMON /A10/ NF
!   X - BEKTOP BAPÜÈPÓEMÛX ÏEPEMEHHÛX
!   Y - BEKTOP ÇHA×EHÈÉ KPÈTEPÈß È OÃPAHÈ×EHÈÉ
    INTEGER::M1,N,NF,J
    REAL(8),DIMENSION(N)::X
    REAL(8),DIMENSION(M1)::Y
!   ECËÈ J < 0,TO BÛ×ÈCËßEM  ÖEËEBÓÞ ÔÓHKÖÈÞ È BCE OÃPAHÈ×EHÈß
!   ECËÈ J = 0,TO BÛ×ÈCËßEM  ÖEËEBÓÞ ÔÓHKÖÈÞ
!   ECËÈ J > 0,TO BÛ×ÈCËßEM  J-OE  OÃPAHÈ×EHÈE
    IF(J==0.OR.J==-1)THEN
        Y(M1)=100 * (X(2) - X(1)**2 )**2 + (1 - X(1))**2
        NF=NF+1
    END IF
    IF(J==1.OR.J==-1)THEN
        Y(1)=(X(1)-1.D0)**2 + (X(2)-1.D0)**2 - 4
        NF=NF+1
    END IF
    IF(J==2.OR.J==-1)THEN
        Y(2)=-X(1)
        NF=NF+1
    END IF
    IF(J==3.OR.J==-1)THEN
        Y(3)=-X(2)
        NF=NF+1
    END IF
!
    RETURN
END SUBROUTINE F
