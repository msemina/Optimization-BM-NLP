! TECTИPOBAHИE METOДOB ИЗ ПAKETA БM METOД СОПРЯЖЁННЫХ ГРАДИЕНТОВ (AP3)
PROGRAM VAP3
! XAPAKTEPИCTИKA ПAPAMETPOB ПPOГPAMMЫ METOДA
    ! N - PAЗMEPHOCTЬ ЗAДAЧИ
    ! X - BEKTOP УПPABЛЯEMЫX ПEPEMEHHЫX
    ! A - BEKTOP ЛEBЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ. ПEPEMEHHЫX
    ! B - BEKTOP ПPABЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ.ПEPEMEHHЫX
    ! F - ИMЯ ПOДПPOГPAMMЫ TИПA FUNCTION ДЛЯ BЫЧИCЛEHИЯ ЗHAЧEHИЯ KPИTEPИЯ
    ! GRAD - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ BЫЧИCЛEHИЯ ГPAДИEHTA
    ! AGS - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ PACЧETA MATPИЦЫ BTOPЫX ПPOИЗBOДHЫX (HE ИCПOЛЬЗУETCЯ)
    ! Y - ЗHAЧEHИE KPИTEPИЯ
    ! G1 - BEKTOP ЗHAЧEHИЙ ПPOИЗBOДHЫX ФУHKЦИИ F
    ! Q - ПAPAMETP C ФИKCИPOBAHHЫM ЗHAЧEHИEM ( = 0)
    ! PAR - BEKTOP ПAPAMETPOB METOДA
    ! FNLP - ФИKCИPOBAHHOE ИMЯ ПOДПPOГPAMMЫ
! OПИCAHИE ПAPAMETPOB ПPOГPAMMЫ METOДA
    ! OПИCAHИE OБЩИX OБЛACTEЙ METOДA
    COMMON /AP31/ P1
    COMMON /AP32/ P2
    COMMON /AP33/ G2
    COMMON /AP34/ XV
    COMMON /AP35/ W
    COMMON /AP36/ WN
    REAL(8)::F,FNLP,Y
    INTEGER::Q
    ! PAЗMEPHOCTЬ MACCИBOB X,A,B,G1 PABHA N
    REAL(8),DIMENSION(2)::X,A,B,G1
    REAL(8),DIMENSION(40)::PAR
    ! PAЗMEPHOCTЬ BCEX MACCИBOB = N
    REAL(8),DIMENSION(2)::P1,P2,G2,XV
    INTEGER,DIMENSION(2)::W,WN
    EXTERNAL F,FNLP,GRAD,AGS
! ИCXOДHЫE ДAHHЫE ЗAДAЧИ
    ! PAЗMEPHOCTЬ ЗAДAЧИ
    N=2
    ! HAЧAЛЬHAЯ TOЧKA
    X(1)=-1.0
    X(2)=-1.0
    ! ЛEBЫE ГPAHИЦЫ УПPABЛ. ПEPEMEHHЫX ПO KAЖДOЙ KOOPДИHATE
    A(1)=-10.
    A(2)=-10.
    ! ПPABЫE ГPAHИЦЫ УПPABЛ. ПEPEMEHHЫX ПO KAЖДOЙ KOOPДИHATE
    B(1)=10.
    B(2)=10.
! ЗAДAHИE ПAPAMETPOB METOДA
    Q=0
    PAR(Q+1)=0.001      ! TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ ПO HOPME ГPAДИEHTA
    PAR(Q+2)=10         ! MAKCИMAЛЬHOE ЧИCЛO ШAГOB,KOTOPOE MOЖHO CДEЛATЬ
    PAR(Q+3)=0          ! BЫXOДHOЙ ПAPAMETP
    PAR(Q+4)=0.0001     ! HAЧAЛЬHЫЙ ШAГ CПУCKA
    PAR(Q+5)=0.1        ! HAЧAЛЬHAЯ TOЧHOCTЬ PEШEHИЯ OДHOMEPHOЙ ЗAДAЧИ BЫБOPA OПTИMAЛЬHOГO ШAГA
    PAR(Q+6)=0.00000001 ! MИHИMAЛЬHЫЙ ШAГ CПУCKA
    PAR(Q+7)=10         ! ЧИCЛO ИTEPAЦИЙ,ЧEPEЗ KOTOPOE METOД "BOCCTAHABЛИBAETCЯ"
    PAR(Q+8)=1          ! HOMEP BEPCИИ METOДA ( = 1 ИЛИ 2 )
    PAR(Q+9)=2          ! ПOPЯДOK ДИФФEPEHЦИPOBAHИЯ ( = 1 ИЛИ 2 )
    PAR(Q+10)=0.00001   ! ШAГ ДИФФEPEHЦИPOBAHИЯ
    PAR(Q+11)=1         ! ЧИCЛO УДAЧHЫX ШAГOB,ЧEPEЗ KOTOPOE  CЛEДУET BЫBOДИTЬ ИHФOPMAЦИЮ
    PAR(Q+12)=3         ! CTEПEHЬ ПOДPOБHOCTИ BЫBOДИMOЙ ИHФOPMAЦИИ: OT 0 ДO 3
! ВЫЗОВ МЕТОДА
    CALL AP3(N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
END PROGRAM VAP3
