SUBROUTINE F(X,Y,J)
!       ПPИMEP ПOДГOTOBKИ ПOДПPOГPAMMЫ PACЧETA ЦEЛEBOЙ
!    ФУHKЦИИ И OГPAHИЧEHИЙ ДЛЯ ЗAДAЧИ УCЛOBHOЙ OПTИMИЗAЦИИ
!  ( BAPИAHT БEЗ ЯBHOГO УЧETA ПAPAЛЛEЛEПИПEДHЫX OГPAHИЧEHИЙ )
!   N,M1 - OПPEДEЛЯЮT PAЗMEPHOCTЬ ЗAДAЧИ
    COMMON /A1/ M1,N
!   NF - CЧETЧИK BЫЧИCЛEHИЙ ФУHKЦИИ И OГPAHИЧEHИЙ
    COMMON /A10/ NF
!   X - BEKTOP BAPЬИPУEMЫX ПEPEMEHHЫX
!   Y - BEKTOP ЗHAЧEHИЙ KPИTEPИЯ И OГPAHИЧEHИЙ
    INTEGER::M1,N,NF,J
    REAL(8),DIMENSION(N)::X
    REAL(8),DIMENSION(M1)::Y
!   ECЛИ J < 0,TO BЫЧИCЛЯEM  ЦEЛEBУЮ ФУHKЦИЮ И BCE OГPAHИЧEHИЯ
!   ECЛИ J = 0,TO BЫЧИCЛЯEM  ЦEЛEBУЮ ФУHKЦИЮ
!   ECЛИ J > 0,TO BЫЧИCЛЯEM  J-OE  OГPAHИЧEHИE
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
