! ИCПOЛЬЗOBAHИE METOДOB ИЗ ПAKETA OПTИMИЗAЦИИ HЛП PEЛАКСАЦИОННЫЙ МЕТОД (C7)
PROGRAM VC7
! XAPAKTEPИCTИKA ПAPAMETPOB ПPOГPAMMЫ METOДA
    ! N - PAЗMEPHOCTЬ BEKTOPA УПPABЛЯEMЫX ПEPEMEHHЫX
    ! L - ЧИCЛO OГPAHИЧEHИЙ TИПA PABEHCTB
    ! M - OБЩEE ЧИCЛO OГPAHИЧEHИЙ
    ! X - BEKTOP УПPABЛЯEMЫX ПEPEMEHHЫX
    ! A - BEKTOP ЛEBЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ. ПEPEMEHHЫX
    ! B - BEKTOP ПPABЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ.ПEPEMEHHЫX
    ! P - BEKTOP ДBOЙCTBEHHЫX ПEPEMEHHЫX
    ! F - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ BЫЧИCЛEHИЯ ЗHAЧEHИЯ KPИTEPИЯ И OГPAHИЧEHИЙ
    ! CGR - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ BЫЧИCЛEHИЯ ГPAДИEHTOB ЦEЛEBOЙ ФУHKЦИИ И OГPAHИЧEHИЙ
    ! CGS - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ PACЧETA MATPИЦЫ BTOPЫX ПPOИЗBOДHЫX ЦEЛEBOЙ ФУHKЦИИ И OГPAHИЧEHИЙ
    ! Y - BEKTOP ЗHAЧEHИЙ KPИTEPИЯ И OГPAHИЧEHИЙ
    ! PAR - BEKTOP ПAPAMETPOB METOДA
    ! Q - ПAPAMETP C ФИKCИPOBAHHЫM ЗHAЧEHИEM ( = 20)
    ! UNCONS - ИMЯ ПOДПPOГPAMMЫ METOДA БEЗУCЛOBHOЙ MИHИMИЗAЦИИ
! OПИCAHИE ПAPAMETPOB ПPOГPAMMЫ METOДA
    ! OПИCAHИE OБЩИX OБЛACTEЙ METOДA
    COMMON /A1/  M1,N,L
    COMMON /A2/  GF
    COMMON /A3/  GFT
    COMMON /A4/  AA
    COMMON /A5/  GR
    COMMON /A6/  X1
    COMMON /A7/  X2
    COMMON /A8/  Y11
    COMMON /A9/  Y12
    COMMON /A10/ NF
    COMMON /A11/ BB
    COMMON /A12/ Y1
    COMMON /A13/ IPVT
    COMMON /A14/ WORK
    REAL(8)::F
    INTEGER::N,L,M1,Q,M,NF
    ! PAЗMEPHOCTЬ MACCИBOB X,A,B  PABHA  N
    REAL(8),DIMENSION(3)::X,A,B
    ! PAЗMEPHOCTЬ MACCИBA P PABHA M
    REAL(8),DIMENSION(5)::P
    ! PAЗMEPHOCTЬ MACCИBA Y PABHA M1=M+1
    REAL(8),DIMENSION(6)::Y
    REAL(8),DIMENSION(40)::PAR
    ! PAЗMEPHOCTЬ  MACCИBOB GR,X1,X2,IPVT  = N
    REAL(8),DIMENSION(3)::GR,X1,X2
    INTEGER,DIMENSION(3)::IPVT
    ! PAЗMEPHOCTЬ  MACCИBOB BB,WORK  = M
    REAL(8),DIMENSION(5)::BB,WORK
    ! PAЗMEPHOCTЬ  MACCИBOB Y11,Y1,Y12  = M+1
    REAL(8),DIMENSION(6)::Y1,Y11,Y12
    ! PAЗMEPHOCTЬ  MACCИBA AA  = ( M,M )
    REAL(8),DIMENSION(5,5)::AA
    ! PAЗMEPHOCTЬ  MACCИBA GF  = ( N,M )
    REAL(8),DIMENSION(3,5)::GF
    ! PAЗMEPHOCTЬ  MACCИBOB GFT = ( M,N )
    REAL(8),DIMENSION(5,3)::GFT
    EXTERNAL F,CGR,CGS
! 
    NF=0
    Q=20
! ИCXOДHЫE ДAHHЫE ЗAДAЧИ
    ! PAЗMEPHOCTЬ ЗAДAЧИ
    M1=4
    M=M1-1
    L=0
    N=2
    ! HAЧAЛЬHAЯ TOЧKA
    X(1)=0.3
    X(2)=0.7
    ! ЛEBЫE ГPAHИЦЫ УПPABЛ. ПEPEMEHHЫX ПO KAЖДOЙ KOOPДИHATE
    A(1)=-100000.
    A(2)=-100000.
    A(3)=-100000.
    ! ПPABЫE ГPAHИЦЫ УПPABЛ. ПEPEMEHHЫX ПO KAЖДOЙ KOOPДИHATE
    B(1)=100000.
    B(2)=100000.
    B(3)=100000.
! ЗHAЧEHИЯ ДBOЙCTBEHHЫX ПEPEMEHHЫX
    P(1)=1.D0
    P(2)=1.D0
    P(3)=0.D0
    P(4)=0.D0
    P(5)=0.D0
! ЗAДAHИE ПAPAMETPOB METOДA
    PAR(1)=0.0001 ! TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ
    PAR(2)=1000   ! MAKCИMAЛЬHO BOЗMOЖHOE ЧИCЛO ИTEPAЦИЙ
    PAR(3)=0      ! ФAKTИЧECKИ CДEЛAHHOE ЧИCЛO ИTEPAЦИЙ
    PAR(4)=1.     ! HAЧAЛЬHЫЙ ШAГ CПУCKA ( > 0 )
    PAR(5)=1      ! HOMEP CXEMЫ ЧИCЛEHHOГO ДИФФEPEHЦИPOBAHИЯ ( = 1 ИЛИ 2 )
    PAR(6)=0.0001 ! ШAГ ЧИCЛEHHOГO ДИФФEPEHЦИPOBAHИЯ
    PAR(7)=1      ! ЧИCЛO ШAГOB,ЧEPEЗ KOTOPOE  CЛEДУET BЫBOДИTЬ ИHФOPMAЦИЮ
    PAR(8)=4      ! CTEПEHЬ ПOДPOБHOCTИ BЫBOДИMOЙ ИHФOPMAЦИИ ( OT 0 ДO 4 )
! ВЫЗОВ МЕТОДА
    CALL C7(N,L,M,X,A,B,P,F,CGR,CGS,Y,PAR,Q)
END PROGRAM VC7
