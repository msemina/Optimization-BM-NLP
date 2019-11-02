! ИCПOЛЬЗOBAHИE METOДOB ИЗ ПAKETA OПTИMИЗAЦИИ METOД ПОИСКА ДОПУСТИМЫХ ТОЧЕК ( BHYTP )
PROGRAM VBNYTP
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
    ! AP3 -  ИMЯ ПOДПPOГPAMMЫ METOДA БEЗУCЛOBHOЙ MИHИMИЗAЦИИ
! OПИCAHИE ПAPAMETPOB ПPOГPAMMЫ METOДA
    ! OПИCAHИE OБЩИX OБЛACTEЙ METOДA
    COMMON /A1/    M1,N,L
    COMMON /A8/    Y11
    COMMON /A9/    Y2
    COMMON /A11/   G
    COMMON /A10/   NF
    COMMON /A13/   Y1
    COMMON /AP31/  P1
    COMMON /AP32/  P2
    COMMON /AP33/  G2
    COMMON /AP34/  XV
    COMMON /AP35/  W
    COMMON /AP36/  WN
    COMMON /AGR2/  YT
    COMMON /AGR3/  GB
    COMMON /AGES1/ G1
    COMMON /AGES2/ GBT
    INTEGER::N,M,M1,L,Q,NF
    REAL(8)::F
    ! PAЗMEPHOCTЬ MACCИBOB X,A,B PABHA N
    REAL(8),DIMENSION(3)::X,A,B
    ! PAЗMEPHOCTЬ MACCИBA P PABHA M
    REAL(8),DIMENSION(5)::P
    ! PAЗMEPHOCTЬ MACCИBA Y PABHA M1=M+1
    REAL(8),DIMENSION(6)::Y
    REAL(8),DIMENSION(40)::PAR
    ! PAЗMEPHOCTЬ  MACCИBOB G,GB,G1 = N
    REAL(8),DIMENSION(3)::G,G1,GB
    ! PAЗMEPHOCTЬ  MACCИBA GBT  = ( N,N )
    REAL(8),DIMENSION(3,3)::GBT
    ! PAЗMEPHOCTЬ  MACCИBOB Y11,YT,Y2,Y1  = M+1
    REAL(8),DIMENSION(6)::Y11,Y2,Y1,YT
    EXTERNAL F,CGR,CGS,AP3
! ПEPEMEHHЫE ДЛЯ METOДA БEЗУCЛOBHOЙ OПTИMИЗAЦИИ (AP3)
    ! PAЗMEPHOCTЬ MACCИBOB P1,P2,G2,XV,W,WN = M
    REAL(8),DIMENSION(6)::P1,P2,G2,XV
    INTEGER,DIMENSION(6)::W,WN
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
    X(1)=-10.
    X(2)= -10.
    ! ЛEBЫE ГPAHИЦЫ УПPABЛ. ПEPEMEHHЫX ПO KAЖДOЙ KOOPДИHATE
    A(1)= -100000.
    A(2)= -100000.
    A(3)= -100000.
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
    PAR(1)=0.0001       ! TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ
    PAR(2)=6.           ! MAKCИMAЛЬHO BOЗMOЖHOE ЧИCЛO ИTEPAЦИЙ
    PAR(3)=0.           ! ФAKTИЧECKИ CДEЛAHHOE ЧИCЛO ИTEPAЦИЙ
    PAR(4)=1.           ! ЧИCЛO ШAГOB,ЧEPEЗ KOTOPOE  CЛEДУET BЫBOДИTЬ ИHФOPMAЦИЮ
    PAR(5)=4.           ! CTEПEHЬ ПOДPOБHOCTИ BЫBOДИMOЙ ИHФOPMAЦИИ ( OT 0 ДO 4 )
! ПAPAMETPЫ METOДA COПPЯЖEHHЫX ГPAДИEHTOB (AP3)
    PAR(Q+1)=0.1        ! TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ ПO HOPME ГPAДИEHTA
    PAR(Q+2)=10.        ! MAKCИMAЛЬHOE ЧИCЛO ШAГOB, KOTOPOE MOЖHO CДEЛATЬ
    PAR(Q+3)=0.         ! BЫXOДHOЙ ПAPAMETP
    PAR(Q+4)=0.0001     ! HAЧAЛЬHЫЙ ШAГ CПУCKA
    PAR(Q+5)=0.1        ! HAЧAЛЬHAЯ TOЧHOCTЬ PEШEHИЯ OДHOMEPHOЙ ЗAДAЧИ BЫБOPA OПTИMAЛЬHOГO ШAГA
    PAR(Q+6)=0.00000001 ! MИHИMAЛЬHЫЙ ШAГ CПУCKA
    PAR(Q+7)=10.        ! ЧИCЛO ИTEPAЦИЙ, ЧEPEЗ KOTOPOE METOД "BOCCTAHABЛИBAETCЯ"
    PAR(Q+8)=1.         ! HOMEP BEPCИИ METOДA ( = 1 ИЛИ 2 )
    PAR(Q+9)=1.         ! ПOPЯДOK ДИФФEPEHЦИPOBAHИЯ ( = 1 ИЛИ 2 )
    PAR(Q+10)=0.0001    ! ШAГ ДИФФEPEHЦИPOBAHИЯ
    PAR(Q+11)=1.        ! ЧИCЛO УДAЧHЫX ШAГOB, ЧEPEЗ KOTOPOE  CЛEДУET BЫBOДИTЬ ИHФOPMAЦИЮ
    PAR(Q+12)=0.        ! CTEПEHЬ ПOДPOБHOCTИ BЫBOДИMOЙ ИHФOPMAЦИИ: OT 0 ДO 3
! ВЫЗОВ МЕТОДА
    CALL BNYTP(N,L,M,X,A,B,P,F,CGR,CGS,Y,PAR,Q,AP3)
END PROGRAM VBNYTP
