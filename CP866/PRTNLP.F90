SUBROUTINE PRTNLP(K,N,L,M,X,Y,P,ST,CRIT,HS,HP,TEXT)
! èOÑèPOÉPAMMA  èEóATà Ñãü METOÑOB Hãè
    COMMON/A1/M1/A10/NF
    INTEGER::HS,HP,TEXT,K,N,L,M1,M
    REAL(8)::CRIT,ST
    REAL(8), DIMENSION(N)::X
    REAL(8), DIMENSION(M1)::Y
    REAL(8), DIMENSION(M)::P
! èêéÇÖêäÄ èÖêÖåÖççõï
    IF(HP==0)RETURN
    IF(K-K/HS*HS/=0)RETURN
! ÇõÇéÑ àçîéêåÄñàà
    IF(.NOT.(K/=0))THEN
        WRITE(*,"(/5X,'óàCãO èEPEMEHHõX',11X,'N=',I3)")N
        WRITE(*,"(5X,'óàCãO OÉPAHàóEHàâ-PABEHCTB L=',I3)")L
        WRITE(*,"(5X,'OÅôEE óàCãO OÉPAHàóEHàâ',4X,'M=',I3,//)")M
    END IF
    IF(TEXT==2)WRITE(*,"(/25X,'ÑOCTàÉHìTA áAÑAHHAü TOóHOCTú')")
    IF(TEXT==3)WRITE(*,"(/24X,'BõèOãHEHO áAÑAHHOE óàCãO òAÉOB')")
    SELECT CASE(HP)
        CASE(1)
            IF(K/=0)THEN
                WRITE(*,'(5X,I5,8X,D15.7,3X,I5)')K,Y(M1),NF
            ELSE
                WRITE(*,"(5X,'ò A É',8X,'îìHKñàü',13X,'CóT')")
            END IF
        CASE(2)
            IF(K/=0)THEN
                WRITE(*,'(5X,I5,8X,D15.7,3X,I5,3X,D15.7,3X,D15.7)')K,Y(M1),NF,ST,CRIT
            ELSE
                WRITE(*,"(/1X,'*** ò A É',8X,'îìHKñàü',13X,'CóT',8X,'òTPAî',6X,'KPàTEPàâ TOóHOCTà')")
            END IF
        CASE(3)
            WRITE(*,"(/1X,'*** ò A É',8X,'îìHKñàü',13X,'CóT',8X,'òTPAî',6X,'KPàTEPàâ TOóHOCTà')")
            WRITE(*,'(5X,I5,8X,D15.7,3X,I5,3X,D15.7,3X,D15.7)')K,Y(M1),NF,ST,CRIT
            WRITE(*,"(5X,'==--> TOóKA')")
            WRITE(*,'(7(3X,D14.7))')(X(I),I=1,N)
        CASE(4)
            WRITE(*,"(/1X,'*** ò A É',8X,'îìHKñàü',13X,'CóT',8X,'òTPAî',6X,'KPàTEPàâ TOóHOCTà')")
            WRITE(*,'(5X,I5,8X,D15.7,3X,I5,3X,D15.7,3X,D15.7)')K,Y(M1),NF,ST,CRIT
            WRITE(*,"(5X,'==--> TOóKA')")
            WRITE(*,'(7(3X,D14.7))')(X(I),I=1,N)
            WRITE(*,"(5X,'==--> OÉPAHàóEHàü')")
            WRITE(*,'(7(3X,D14.7))')(Y(I),I=1,M)
            WRITE(*,"(5X,'==--> ÑBOâCTBEHHõE èEPEMEHHõE')")
            WRITE(*,'(7(3X,D14.7))')(P(I),I=1,M)
    END SELECT
!
    RETURN
END SUBROUTINE PRTNLP
