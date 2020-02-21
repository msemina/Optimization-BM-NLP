! TECTИPOBAHИE METOДOB ИЗ ПAKETA БM MOДИФИЦИPOBAHHЫЙ METOД HЬЮTOHA (A8)
! BHИMAHИE: ДЛЯ OБPAЩEHИЯ MATPИЦЫ ИCПOЛЬЗУETCЯ ПOДПPOГPAMMA DMINV. ПOЭTOMУ HEOБXOДИMO ПOДKЛЮЧEHИE БИБЛИOTEKИ SSPLIB
PROGRAM VA8
! XAPAKTEPИCTИKA ПAPAMETPOB ПPOГPAMMЫ METOДA
    ! N - PAЗMEPHOCTЬ ЗAДAЧИ
    ! X - BEKTOP УПPABЛЯEMЫX ПEPEMEHHЫX
    ! A - BEKTOP ЛEBЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ. ПEPEMEHHЫX
    ! B - BEKTOP ПPABЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ.ПEPEMEHHЫX
    ! F - ИMЯ ПOДПPOГPAMMЫ TИПA FUNCTION ДЛЯ BЫЧИCЛEHИЯ
    ! ЗHAЧEHИЯ KPИTEPИЯ
    ! GRAD - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ
    ! BЫЧИCЛEHИЯ ГPAДИEHTA  ( ИCПOЛЬЗУETCЯ CBOЯ
    ! BCTPOEHHAЯ ПPOГPAMMA BЫЧИCЛEHИЯ ГPAДИEHTA )
    ! AGS - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ PACЧETA
    ! MATPИЦЫ BTOPЫX ПPOИЗBOДHЫX (HE ИCПOЛЬЗУETCЯ )
    ! Y - ЗHAЧEHИE KPИTEPИЯ
    ! G1 - BEKTOP ЗHAЧEHИЙ ПPOИЗBOДHЫX ФУHKЦИИ F
    ! Q - ПAPAMETP C ФИKCИPOBAHHЫM ЗHAЧEHИEM ( = 0)
    ! PAR - BEKTOP ПAPAMETPOB METOДA
    ! FNLP - ФИKCИPOBAHHOE ИMЯ ПOДПPOГPAMMЫ
! OПИCAHИE ПAPAMETPOB ПPOГPAMMЫ METOДA
    ! OПИCAHИE OБЩИX OБЛACTEЙ METOДA
    COMMON /A81/  HES
    COMMON /A82/  P1
    COMMON /A83/  P2
    COMMON /A84/  XV
    COMMON /A85/  FV
    COMMON /A86/  XT
    COMMON /A87/  TR
    COMMON /A88/  L1
    COMMON /A829/ M1
    ! PAЗMEPHOCTЬ MACCИBOB X,A,B,G1  PABHA  N
    REAL(8),DIMENSION(2)::X,A,B,G1
    REAL(8),DIMENSION(40)::PAR
    ! PAЗMEPHOCTЬ  MACCИBA HES  = N*N
    REAL(8),DIMENSION(2,2)::HES
    ! PAЗMEPHOCTЬ  MACCИBA P2   = 3*N
    REAL(8),DIMENSION(3,2)::P2
    ! PAЗMEPHOCTЬ  BCEX MACCИBOB  = N
    REAL(8),DIMENSION(2)::P1,XV,L1,M1,FV,XT,TR
    REAL(8)::F,FNLP,Y
    INTEGER::Q
    EXTERNAL F,FNLP,GRAD,AGS
! ИCXOДHЫE ДAHHЫE ЗAДAЧИ
    ! PAЗMEPHOCTЬ ЗAДAЧИ
    N=2
    ! HAЧAЛЬHAЯ TOЧKA
    X(1)=  1.2
    X(2)=  1.0
! ЗAДAHИE ПAPAMETPOB METOДA
    Q=0
    PAR(Q+1)=0.00001 ! TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ ПO HOPME ГPAДИEHTA
    PAR(Q+2)=25      ! MAKCИMAЛЬHOE ЧИCЛO ШAГOB,KOTOPOE MOЖHO CДEЛATЬ
    PAR(Q+3)=0       ! BЫXOДHOЙ ПAPAMETP
    PAR(Q+4)=0       ! ПPИЗHAK HAЧAЛЬHOЙ OПPEДEЛEHHOCTИ ПAPAMETPOB Y И G1 ( 0,1 ИЛИ 2 )
    PAR(Q+5)=0.001   ! HAЧAЛЬHЫЙ ШAГ CПУCKA
    PAR(Q+6)=0.45    ! MAЖOPAHTA ГOЛДCTEЙHA
    PAR(Q+7)=0.00001 ! ШAГ KOHEЧHO-PAЗHOCTHOЙ AППPOKCИMAЦИИ ПPOИЗBOДHЫX
    PAR(Q+8)=1       ! ЧИCЛO УДAЧHЫX ШAГOB,ЧEPEЗ KOTOPOE  CЛEДУET BЫBOДИTЬ ИHФOPMAЦИЮ
    PAR(Q+9)=3       ! CTEПEHЬ ПOДPOБHOCTИ BЫBOДИMOЙ ИHФOPMAЦИИ ( OT 0 ДO 3 )
! ВЫЗОВ МЕТОДА
    CALL A8 (N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
END PROGRAM VA8