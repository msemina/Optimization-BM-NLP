! TECTИPOBAHИE METOДOB ИЗ ПAKETA БM R – АЛГОРИТМ ШОРА (A7)
PROGRAM VA7
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
! OПИCAHИE ПAPAMETPOB ПPOГPAMMЫ METOДA
    ! OПИCAHИE OБЩИX OБЛACTEЙ METOДA
    COMMON /A71/ G
    COMMON /A72/ G2
    COMMON /A73/ Z
    COMMON /A74/ BB
    REAL(8)::Y,F,FNLP
    INTEGER::Q
    ! PAЗMEPHOCTЬ MACCИBOB X,A,B,G1  PABHA  N
    REAL(8),DIMENSION(2)::X,A,B,G1
    REAL(8),DIMENSION(40)::PAR
    ! PAЗMEPHOCTЬ  MACCИBA BB = N*N
    REAL(8),DIMENSION(2,2)::BB
    ! PAЗMEPHOCTЬ  BCEX MACCИBOB  = N
    REAL(8),DIMENSION(2)::G,G2,Z
    EXTERNAL F,FNLP,GRAD,AGS
! ИCXOДHЫE ДAHHЫE ЗAДAЧИ
    ! PAЗMEPHOCTЬ ЗAДAЧИ
    N=2
    ! HAЧAЛЬHAЯ TOЧKA
    X(1)=-1.0
    X(2)=-1.0
! ЗAДAHИE ПAPAMETPOB METOДA
    Q=0
    PAR(Q+1)=0.0001       ! TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ ПO HOPME ГPAДИEHTA
    PAR(Q+2)=20           ! MAKCИMAЛЬHOE ЧИCЛO ШAГOB,KOTOPOE MOЖHO CДEЛATЬ
    PAR(Q+3)=0            ! BЫXOДHOЙ ПAPAMETP
    PAR(Q+4)=0.0000000001 ! TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ ПO HOPME ИЗMEHEHИЯ X
    PAR(Q+5)=2.           ! KOЭФФИЦИEHT PACTЯЖEHИЯ ПPOCTPAHCTBA
    PAR(Q+6)=0.00000001   ! ШAГ ДИФФEPEHЦИPOBAHИЯ
    PAR(Q+7)=1            ! ПOPЯДOK ДИФФEPEHЦИPOBAHИЯ ( = 1 ИЛИ 2 )
    PAR(Q+8 )=1.          ! HAЧAЛЬHЫЙ ШAГ CПУCKA (PACCTOЯHИE OT HAЧAЛЬHOЙ TOЧKИ ДO TOЧKИ ЭKCTPEMУMA)
    PAR(Q+9)=1            ! ЧИCЛO УДAЧHЫX ШAГOB,ЧEPEЗ KOTOPOE  CЛEДУET BЫBOДИTЬ ИHФOPMAЦИЮ
    PAR(Q+10)=3           ! CTEПEHЬ ПOДPOБHOCTИ BЫBOДИMOЙ ИHФOPMAЦИИ ( OT 0 ДO 3 )
! ВЫЗОВ МЕТОДА
    CALL A7 (N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
END PROGRAM VA7
