! TECTИPOBAHИE METOДOB ИЗ ПAKETA БM METOД ЭЛЛИПCOИДOB (A9)
PROGRAM VA9
! XAPAKTEPИCTИKA ПAPAMETPOB ПPOГPAMMЫ METOДA
    ! N - PAЗMEPHOCTЬ ЗAДAЧИ
    ! X - BEKTOP УПPABЛЯEMЫX ПEPEMEHHЫX
    ! A - BEKTOP ЛEBЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ. ПEPEMEHHЫX
    ! B - BEKTOP ПPABЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ.ПEPEMEHHЫX
    ! F - ИMЯ ПOДПPOГPAMMЫ TИПA FUNCTION ДЛЯ BЫЧИCЛEHИЯ ЗHAЧEHИЯ KPИTEPИЯ
    ! GRAD - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ BЫЧИCЛEHИЯ ГPAДИEHTA
    ! AGS - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ PACЧETA MATPИЦЫ BTOPЫX ПPOИЗBOДHЫX (HE ИCПOЛЬЗУETCЯ )
    ! Y - ЗHAЧEHИE KPИTEPИЯ
    ! G1 - BEKTOP ЗHAЧEHИЙ ПPOИЗBOДHЫX ФУHKЦИИ F
    ! Q - ПAPAMETP C ФИKCИPOBAHHЫM ЗHAЧEHИEM ( = 0)
    ! PAR - BEKTOP ПAPAMETPOB METOДA
    ! FNLP - ФИKCИPOBAHHOE ИMЯ ПOДПPOГPAMMЫ
! OПИCAHИE OБЩИX OБЛACTEЙ METOДA
    ! OПИCAHИE OБЩИX OБЛACTEЙ METOДA
    COMMON /A91/ BG
    COMMON /A92/ G
    COMMON /A93/ G2
    COMMON /A94/ GN
    COMMON /A95/ XN 
    COMMON /A96/ X1
    ! PAЗMEPHOCTЬ MACCИBOB BCEX MACCИBOB PABHA N
    REAL(8),DIMENSION(2)::X,A,B,G1,G,G2,GN,XN,X1
    ! PAЗMEPHOCTЬ MACCИBA BG = N*N
    REAL(8),DIMENSION(2,2)::BG
    REAL(8),DIMENSION(40)::PAR
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
    PAR(Q+1)=0.00001            ! TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ ПO HOPME ГPAДИEHTA
    PAR(Q+2)=25                 ! MAKCИMAЛЬHOE ЧИCЛO ШAГOB,KOTOPOE MOЖHO CДEЛATЬ
    PAR(Q+3)=0                  ! BЫXOДHOЙ ПAPAMETP
    PAR(Q+4)=10.                ! PAДИУC HAЧAЛЬHOГO ШAPA,OПPEДEЛЯЮЩEГO OБЛACTЬ ПOИCKA
    PAR(Q+5)=0.001              ! PEШEHИЯ OДHOMEPHOЙ ЗAДAЧИ BЫБOPA ШAГA
    PAR(Q+6)=0.0000000000000001 ! ДOПУCTИMЫЙ OБЪEM ЭЛЛИПCOИДA
    PAR(Q+7)=2                  ! HOMEP BEPCИИ METOДA ( =1 ДЛЯ BЫПУKЛЫX ФУHKЦИЙ ; =2, ECЛИ ФУHKЦИЯ HEBЫПУKЛAЯ )
    PAR(Q+8)=1                  ! ПOPЯДOK ДИФФEPEHЦИPOBAHИЯ ( = 1 ИЛИ 2 )
    PAR(Q+9)=0.00001            ! ШAГ ДИФФEPEHЦИPOBAHИЯ
    PAR(Q+10)=1                 ! ЧИCЛO УДAЧHЫX ШAГOB,ЧEPEЗ KOTOPOE СЛEДУET BЫBOДИTЬ ИHФOPMAЦИЮ
    PAR(Q+11)=3                 ! CTEПEHЬ ПOДPOБHOCTИ BЫBOДИMOЙ ИHФOPMAЦИИ ( OT 0 ДO 3 )
! ВЫЗОВ МЕТОДА
    CALL A9 (N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
END PROGRAM VA9
