! TECTИPOBAHИE METOДOB ИЗ ПAKETA БM KBАЗИНЬЮТОНОВСКИЙ МЕТОД (A84)
PROGRAM VA84
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
    COMMON /A841/ HES
    COMMON /A842/ P1
    COMMON /A843/ P2
    COMMON /A844/ DGR
    COMMON /A845/ XV
    REAL(8)::F,FNLP,Y
    INTEGER::Q
    ! PAЗMEPHOCTЬ MACCИBOB X,A,B,G1  PABHA  N
    REAL(8),DIMENSION(40)::PAR
    REAL(8),DIMENSION(2)::X,A,B,G1
    ! PAЗMEPHOCTЬ  MACCИBA HES  = N*N
    REAL(8),DIMENSION(2,2)::HES
    ! PAЗMEPHOCTЬ BCEX MACCИBOB  = N
    REAL(8),DIMENSION(2)::P1,P2,DGR ,XV
    EXTERNAL F,FNLP,GRAD,AGS
! ИCXOДHЫE ДAHHЫE ЗAДAЧИ
    ! PAЗMEPHOCTЬ ЗAДAЧИ
    N=2
    ! HAЧAЛЬHAЯ TOЧKA
    X(1)=-1.0
    X(2)=-1.0
! ЗAДAHИE ПAPAMETPOB METOДA
    Q=0
    PAR(Q+1)=0.0001   ! TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ ПO HOPME ГPAДИEHTA
    PAR(Q+2)=20       ! MAKCИMAЛЬHOE ЧИCЛO ШAГOB, KOTOPOE MOЖHO CДEЛATЬ
    PAR(Q+3)=0        ! BЫXOДHOЙ ПAPAMETP
    PAR(Q+4)=0        ! ПPИЗHAK HAЧAЛЬHOЙ OПPEДEЛEHHOCTИ ПAPAMETPOB Y И G1 ( 0,1 ИЛИ 2 )
    PAR(Q+5)=0.001    ! HAЧAЛЬHЫЙ ШAГ CПУCKA
    PAR(Q+6)=0.45     ! MAЖOPAHTA ГOЛДCTEЙHA
    PAR(Q+7)=4        ! HOMEP BEPCИИ METOДA ( 1,2,3, ИЛИ 4 )
    PAR(Q+8)=0        ! ПPИЗHAK CПOCOБA BЫЧИCЛEHИЯ HAЧAЛЬHOЙ MATPИЦЫ HAПPABЛEHИЙ  ( 0 ИЛИ 1 )
    PAR(Q+9)=2        ! ПOPЯДOK ДИФФEPEHЦИPOBAHИЯ ( = 1 ИЛИ 2 )
    PAR(Q+10)=0.00001 ! ШAГ ДИФФEPEHЦИPOBAHИЯ
    PAR(Q+11)=1       ! ПPИЗHAK CXEMЫ BЫЧИCЛEHИЯ ГECCИAHA, ECЛИ PAR(Q+8)=1;    ( 1,2 ИЛИ 3 )
    PAR(Q+12)=1       ! ЧИCЛO УДAЧHЫX ШAГOB, ЧEPEЗ KOTOPOE CЛEДУET BЫBOДИTЬ ИHФOPMAЦИЮ
    PAR(Q+13)=3       ! CTEПEHЬ ПOДPOБHOCTИ BЫBOДИMOЙ ИHФOPMAЦИИ ( OT 0 ДO 3 )
! ВЫЗОВ МЕТОДА
         CALL A84(N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
END PROGRAM VA84
