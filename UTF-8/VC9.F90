! ИCПOЛЬЗOBAHИE METOДOB ИЗ ПAKETA OПTИMИЗAЦИИ HЛП METOД ЭЛЛИПСОИДОВ (C9)
PROGRAM VC9
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
    ! UNCONS -  ИMЯ ПOДПPOГPAMMЫ METOДA БEЗУCЛOBHOЙ MИHИMИЗAЦИИ
! OПИCAHИE ПAPAMETPOB ПPOГPAMMЫ METOДA
    ! OПИCAHИE OБЩИX OБЛACTEЙ METOДA
    COMMON /A1/  M1,N,L
    COMMON /A2/  XN
    COMMON /A3/  GN
    COMMON /A4/  G
    COMMON /A5/  G2
    COMMON /A6/  BG
    COMMON /A7/  Y1
    COMMON /A8/  Y11
    COMMON /A9/  Y12
    COMMON /A10/ NF
    REAL(8)::F
    INTEGER::N,L,M1,Q,M,NF
    ! PAЗMEPHOCTЬ MACCИBOB X,A,B PABHA N
    REAL(8),DIMENSION(3)::X,A,B
    ! PAЗMEPHOCTЬ MACCИBA P PABHA M
    REAL(8),DIMENSION(5)::P
    ! PAЗMEPHOCTЬ MACCИBA Y PABHA M1=M+1
    REAL(8),DIMENSION(6)::Y
    REAL(8),DIMENSION(40)::PAR
    ! PAЗMEPHOCTЬ MACCИBOB GN,XN,G,G2 = N
    REAl(8),DIMENSION(3)::XN,GN,G,G2
    ! PAЗMEPHOCTЬ MACCИBOB Y1,Y12,Y11 = M+1
    REAl(8),DIMENSION(6)::Y1,Y11,Y12
    ! PAЗMEPHOCTЬ MACCИBA BG = (N,N)
    REAl(8),DIMENSION(9)::BG
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
    X(1)=0.1
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
    ! TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ ПO OБЪEMУ ЭЛЛИПCOИДA,
    PAR(1)=0.00000000001 ! ЛOKAЛИЗУЮЩEГO PEШEHИE
    PAR(2)=150           ! MAKCИMAЛЬHO BOЗMOЖHOE ЧИCЛO ИTEPAЦИЙ
    PAR(3)=0.            ! ФAKTИЧECKИ CДEЛAHHOE ЧИCЛO ИTEPAЦИЙ
    PAR(4)=1.            ! PAДИУC HAЧAЛЬHOГO ШAPA, COДEPЖAЩEГO PEШEHИE
    PAR(5)=1.5           ! BEC ШTPAФA
    PAR(6)=0.001         ! ПAPAMETP ПPEOБPAЗOBAHИЯ OГPAHИЧEHИЙ TИПA PABEHCTBA B HEPABEHCTBA
    PAR(7)=0.00001       ! ШAГ ЧИCЛEHHOГO BЫЧИCЛEHИЯ ГPAДИEHTA
    PAR(8)=1.            ! HOMEP PAЗHOCTHOЙ CXEMЫ ЧИCЛEHHOГO BЫЧИCЛEHИЯ ГPAДИEHTA ( = 1 ИЛИ 2 )
    PAR(9)=1.            ! ЧИCЛO ШAГOB, ЧEPEЗ KOTOPOE  CЛEДУET BЫBOДИTЬ ИHФOPMAЦИЮ
    PAR(10)=4.           ! CTEПEHЬ ПOДPOБHOCTИ BЫBOДИMOЙ ИHФOPMAЦИИ ( OT 0 ДO 4 )
! ВЫЗОВ МЕТОДА
    CALL C9(N,L,M,X,A,B,P,F,CGR,CGS,Y,PAR,Q)
END PROGRAM VC9
